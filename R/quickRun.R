.createRecommendListFromConcepts <- function(query,
                                             closestConditionConcept,
                                             conceptList,
                                             llmClient,
                                             connection,
                                             connectionDetails,
                                             cdmDatabaseSchema,
                                             excludedConditions = "none",
                                             additionalInformation = "",
                                             clinicalContext = "",
                                             bucketSize = 1) {

  text <- "included concepts"

  conceptList <- Filter(is.numeric, conceptList) # ensure no garbage was included (rare)
  message("For concept(s): ", paste(conceptList, collapse = ", "))
  message("Getting GenAI similarity response for ", text, " for query: ", query)

  sqlFilename <- "QuickConcepts.sql"
  conceptList <- conceptList[!is.na(conceptList)]
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = sqlFilename,
    packageName = "Phenelope",
    dbms = connectionDetails$dbms,
    cdm_database_schema = cdmDatabaseSchema,
    concept_list = paste(conceptList, collapse = ", ")
  )

  conceptList <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  concepts <- conceptList

  conceptsToUse <- concepts
  results <- data.frame()

  # read in the basic prompt

  promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE.txt", package = "Phenelope")
  originalLines <- readLines(promptUp)

  #test which in the concept list need to be tested
  testList <- NULL
  noTestList <- NULL

  cost <- 0
  llmClient$set_turns(list()) # Reset the chat

  concepts$conceptName <- gsub("[\\[\\]]", " ", concepts$conceptName) #remove any [ or ] from name (interferes with json structure)
  if (nrow(concepts) != 0) {
    startPoint <- 1
    endPoint <- min(bucketSize, nrow(concepts))
    while(startPoint <= nrow(concepts)) {
      cat(paste0("--Querying LLM - Analyzing concepts ", startPoint, " through ", endPoint, " of ", nrow(concepts), "\r"))
      concepts$aboveMin[1] <- T # always test the first concept

      testCondition <- concepts[startPoint:endPoint, c("conceptId", "conceptName")]
      baseCondition <- query

      updatedLines <- gsub("MAIN_CONDITION", baseCondition, originalLines)
      json_all <- jsonlite::toJSON(testCondition)
      updatedLines <- gsub("SUGGESTED_CONDITION", json_all, updatedLines)
      updatedLines <- gsub("EXCLUDED_CONDITIONS", excludedConditions, updatedLines)
      updatedLines <- gsub("CLINICAL_CONTEXT", clinicalContext, updatedLines)
      updatedLines <- gsub("ADDITIONAL_INFORMATION", additionalInformation, updatedLines)

      prompt <- paste(updatedLines, collapse = "\n")

      retryLimit <- 10 # Maximum number of retries
      attempt <- 0 # Initial attempt counter
      success <- FALSE # Flag to indicate success

      while (attempt <= retryLimit && !success) { # llm will mislabel column headers occasionally - usually fixed with a re-try
        tryCatch(
          {
            attempt <- attempt + 1 # Increment the attempt count

            if(attempt > 1) {
              writeLines(prompt, "e:/shared/llm/joel/pe/prompt.txt")
            }

            systemPrompt <- "You are an expert medical doctor specializing in healthcare data analysis. Your primary function is to analyze healthcare data, including electronic health records, to infer causal relationships between exposures and health outcomes."

            llmClient$set_system_prompt(systemPrompt)

            text <- llmClient$chat_structured(prompt,
                                              echo = "none",
                                              type = ellmer::type_array(ellmer::type_object(
                                                conceptId = ellmer::type_string(),
                                                suggestedCondition = ellmer::type_string(),
                                                excludedConditions = ellmer::type_string(),
                                                proposedInExcluded = ellmer::type_string(),
                                                finalAnswer = ellmer::type_string(),
                                                rationaleForAnswer = ellmer::type_string(),
                                                confidenceLevel = ellmer::type_string()
                                              ))
            )

            if (is.character(text)) {
              if (jsonlite::validate(text)) {
                text <- jsonlite::fromJSON(text)
              }
            }

            resultsDf <- data.frame(text)
            resultsDf$tested <- T

            resultsDf$mainCondition <- baseCondition
            resultsDf$cost <- sprintf("%.5f", llmClient$get_cost())

            columnsToFront <- c("suggestedCondition", "conceptId", "mainCondition", "finalAnswer", "rationaleForAnswer", "confidenceLevel")

            # Rearrange the DataFrame
            resultsDf <- resultsDf |>
              dplyr::select(all_of(columnsToFront), everything())

            results <- rbind(results, resultsDf)

            success <- TRUE
            cost <- cost + llmClient$get_cost()
            llmClient$set_turns(list()) # Reset the chat
          },
          error = function(e) {
            # Handle the error: print a message and increment the attempt counter
            message(paste("Attempt", attempt, "failed:", e$message))
            message(paste0("Failure on: ***", testCondition, "***"))
            if (grepl("abort", e$message, ignore.case = TRUE)) {
              cat("Stopping the run as requested.\n")
              stop("Execution stopped by user.")
            }
            if (attempt >= retryLimit) {
              message(paste("Reached attempt limit."))
              userInput <- readline(prompt = "Do you want to continue with the next concept? y/n ")
              if (userInput == tolower("n")) {
                cat("Stopping the run as requested.\n")
                stop("Execution stopped by user.")
              }
            }
            return(NULL) # Return NULL in case of error
          }
        )
      }

      if (!success) {
        message("All attempts failed for test condition ", testCondition, ". Skipping to next test condition.")
        return(NULL) # Skip to the next iteration of the outer loop
      }

      startPoint <- endPoint + 1
      endPoint <- min(endPoint + bucketSize, nrow(concepts))
    }

    message("\n--Total cost was $", sprintf("%.3f", cost))

    fullResults <- results

    fullResults <- unique(fullResults)

    message("--Number of total concepts: ", nrow(fullResults))

    return(fullResults)
  }
}
