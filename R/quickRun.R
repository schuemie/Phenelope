.createRecommendListFromConcepts <- function(query,
                                             conceptList,
                                             llmClient,
                                             prompt,
                                             connectionDetails,
                                             connection,
                                             cdmDatabaseSchema,
                                             excludedConcepts = "none",
                                             additionalInformation = "",
                                             clinicalContext,
                                             bucketSize = 1) {

  text <- "included concepts"

  conceptList <- Filter(is.numeric, conceptList) # ensure no garbage was included (rare)
  message("For concept(s): ", paste(conceptList, collapse = ", "))
  message("Getting GenAI similarity response for ", text, " for query: ", query)

  conceptList <- conceptList[!is.na(conceptList)]

  forSql <- paste0("(", conceptList, ")", collapse = ",")

  sqlFilename <- "QuickConcepts.sql"
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = sqlFilename,
    packageName = "Phenelope",
    dbms = connectionDetails$dbms,
    cdm_database_schema = cdmDatabaseSchema,
    concept_list = forSql
  )

  conceptList <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  concepts <- conceptList

  conceptsToUse <- concepts
  results <- data.frame()

  # read in the basic prompt

  # if(bucketSize > 1) {
  #   # promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE.txt", package = "Phenelope")
  #   promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE_generic.txt", package = "Phenelope")
  # } else {
  #   promptUp <- system.file("prompts", "LLM_Prompt_for_PHOEBE_single.txt", package = "Phenelope")
  # }
  #

  promptUp <- prompt
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
      cat(paste0("--Querying LLM - Analyzing concepts ", startPoint, " through ", endPoint, " of ", nrow(concepts), "  \r"))
      concepts$aboveMin[1] <- T # always test the first concept

      testCondition <- concepts[startPoint:endPoint, c("conceptId", "conceptName")]
      baseCondition <- query

      updatedLines <- gsub("MAIN_CONCEPT", baseCondition, originalLines)

      if(bucketSize == 1) {
        testConditionShort <- concepts[startPoint:endPoint, c("conceptName")]
        updatedLines <- gsub("SUGGESTED_CONCEPT_SHORT", testConditionShort, updatedLines)
      }
      json_all <- jsonlite::toJSON(testCondition)
      updatedLines <- gsub("SUGGESTED_CONCEPT", json_all, updatedLines)
      updatedLines <- gsub("EXCLUDED_CONCEPTS", excludedConcepts, updatedLines)
      updatedLines <- gsub("CLINICAL_CONTEXT", clinicalContext, updatedLines)
      updatedLines <- gsub("ADDITIONAL_INFORMATION", additionalInformation, updatedLines)

      # if(domain == "ALL") { #the concept must almost always be a part of the main concept
      #   proportionValue <- "the vast majority (> 95%)"
      # } else { #the concept must a proportion of the main concept to be a part of the main concept
      #   proportionValue <- "a proportion (> 5%)"
      # }
      # updatedLines <- gsub("PROPORTION_VALUE", proportionValue, updatedLines)

      prompt <- paste(updatedLines, collapse = "\n")

      retryLimit <- 10 # Maximum number of retries
      attempt <- 0 # Initial attempt counter
      success <- FALSE # Flag to indicate success

      while (attempt <= retryLimit && !success) { # llm will mislabel column headers occasionally - usually fixed with a re-try
        tryCatch(
          {
            attempt <- attempt + 1 # Increment the attempt count

            systemPrompt <- "You are an expert medical doctor specializing in healthcare data analysis. Your primary function is to analyze healthcare data, including electronic health records, to infer causal relationships between exposures and health outcomes."

            llmClient$set_system_prompt(systemPrompt)

            fullBucket <- FALSE
            bucketAttempt <- 0
            while(!fullBucket) {
              bucketAttempt <- bucketAttempt + 1
              bucketItems <- (endPoint - startPoint) + 1
              text <- llmClient$chat_structured(prompt,
                                                echo = "none",
                                                type = ellmer::type_array(ellmer::type_object(
                                                  conceptId = ellmer::type_string(),
                                                  suggestedConcept = ellmer::type_string(),
                                                  excludedConcepts = ellmer::type_string(),
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
              if(nrow(resultsDf) == bucketItems) { #same rows sent out as received
                fullBucket <- TRUE
                bucketAttempt <- 0
              } else {
                cat(paste0("---Querying LLM - Analyzing concepts ", startPoint, " through ", endPoint, " of ", nrow(concepts), "\r"))
                if(bucketAttempt == 10) {
                  stop("LLM connection issue...stopping")
                }
              }
            }
            resultsDf$tested <- T

            resultsDf$mainCondition <- baseCondition
            resultsDf$cost <- sprintf("%.5f", llmClient$get_cost())

            columnsToFront <- c("suggestedConcept", "conceptId", "mainCondition", "finalAnswer", "rationaleForAnswer", "confidenceLevel")

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

    message("\n\n--Total cost was $", sprintf("%.3f", cost))

    fullResults <- results

    fullResults <- unique(fullResults)

    message("--Number of total concepts: ", nrow(fullResults))

    saveLastPrompt(prompt)

    return(fullResults)
  }
}
