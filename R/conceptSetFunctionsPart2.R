# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Phenelope
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.fullConceptSetCreation <- function(conceptName,
                                    originalConceptList,
                                    excludedConcepts = "none",
                                    llmClient,
                                    connectionDetails,
                                    cdmDatabaseSchema,
                                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                    minCount = 0,
                                    belowMinimumCountApproach,
                                    outputDirectory,
                                    additionalInformation = "",
                                    excludedVocabularies = c("ICDO3"),
                                    clinicalContext = "any clinical context",
                                    bucketSize = 1,
                                    domain = "ALL",
                                    conditionForFiles,
                                    tryNumber,
                                    phoebeExclusions = phoebeExclusions,
                                    quickRun = FALSE) {
  message("Testing concepts and descendants. ")

  connection3 <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection3))

  if (file.exists(file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")))) {
    # found the first half but not the full analysis, skip the first part and go to the second part
    message(
      "File ",
      file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")),
      " exists...skipping to next part of analysis."
    )
    llmResults <- utils::read.csv(file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")))
  } else {
    llmResults <- .createRecommendListViaLlmFromConceptList(
      query = conceptName,
      closestConditionConcept = conceptName,
      conceptList = originalConceptList,
      llmClient = llmClient,
      connection = connection,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      type = "phoebe",
      minCount = minCount,
      previousResults = NULL,
      excludedConcepts = excludedConcepts,
      belowMinimumCountApproach,
      additionalInformation = additionalInformation,
      clinicalContext = clinicalContext,
      excludedVocabularies = c(excludedVocabularies),
      domain = domain,
      phoebeExclusions = phoebeExclusions,
      bucketSize = bucketSize
    )

    utils::write.csv(llmResults, file.path(outputDirectory, paste0("firstPart_", conditionForFiles, tryNumber, ".csv")), row.names = F)
  }

  previousResults <- llmResults

  included <- unique(c(as.integer(llmResults$conceptId[llmResults$finalAnswer == "YES"])))

  conceptList <- unique(c(originalConceptList, included))

  message("Testing final set of included concepts.")

  # remove ancestors of the original concept set list from the list (don't want to include their descendants)
  sqlFilename <- "RemoveAncestors.sql"
  sql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = sqlFilename,
    packageName = "Phenelope",
    dbms = connectionDetails$dbms,
    cdm_database_schema = cdmDatabaseSchema,
    concepts_to_use = paste(originalConceptList, collapse = ", ")
  )

  ancestorList <- DatabaseConnector::querySql(connection = connection3, sql, snakeCaseToCamelCase = TRUE)

  conceptList <- conceptList[!(conceptList %in% c(unlist(ancestorList)))]

  #second pass for the included concept descendants
  llmResults <- .createRecommendListViaLlmFromConceptList(
    query = conceptName,
    closestConditionConcept = conceptName,
    conceptList = conceptList,
    llmClient = llmClient,
    connection = connection,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    type = "included",
    minCount = minCount,
    previousResults = previousResults,
    excludedConcepts = excludedConcepts,
    belowMinimumCountApproach,
    additionalInformation = additionalInformation,
    clinicalContext = clinicalContext,
    excludedVocabularies = c(excludedVocabularies),
    domain = domain,
    bucketSize = bucketSize
  )

  return(llmResults)
}

.getFullConceptSet <- function(conceptName,
                               conceptIds,
                               domains,
                               conceptClasses = "",
                               connection,
                               connectionDetails,
                               cdmDatabaseSchema,
                               quickRun = FALSE,
                               llmClient,
                               minCount,
                               belowMinimumCountApproach,
                               exclusions = "TEST", #default is to test for exclusions - call with set to "none" if no testing wanted
                               additionalInformation = "",
                               excludedVocabularies = c("ICDO3"),
                               excludedConcepts,
                               clinicalContext =  "",
                               outputDirectory = outputDirectory,
                               tryNumber,
                               conditionForFiles,
                               phoebeExclusions = c(),
                               bucketSize = 20) {
  if(!is.null(getOption("databaseConnectorInteger64AsNumeric"))) {
    if(!getOption("databaseConnectorInteger64AsNumeric")) { #if set to false, change to true and put back at the end
      options(databaseConnectorInteger64AsNumeric = TRUE)
      on.exit(options(databaseConnectorInteger64AsNumeric = FALSE))
    }
  }


  connection4 <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection4))

  #test for likely very large (> 5K) concept sets
  sql <- paste0("select count(distinct c.concept_id) ",
                "from ", cdmDatabaseSchema, ".concept_ancestor ca ",
                "join ", cdmDatabaseSchema, ".concept c ",
                "on ca.descendant_concept_id  = c.concept_id ",
                "where ca.ancestor_concept_id in (", paste0(c(conceptIds), collapse = ","), ") ",
                "and c.domain_id in (", paste0("'", domains, "'", collapse = "," ), ") ",
                # "and c.concept_class_id in (", paste0("'", conceptClasses, "'", collapse = "," ), ") ",
                "and c.standard_concept = \"S\";")

  recordCount <- DatabaseConnector:: querySql(connection = connection4, sql, snakeCaseToCamelCase = TRUE)

  if(recordCount <= 5000) { # reasonable size concept set to test
    #test to see if any concepts should be explicitly excluded
    if(exclusions == "TEST") {
      exclusions <- .checkForExclusions(term = conceptName, llmClient = llmClient)
      if(exclusions$yesNo == "YES") {
        excludedConditions <- exclusions$exclusions
      } else {
        excludedConditions <- "none"
      }
    } else {
      excludedConditions <- exclusions
    }

    #create the concept set
    finalConceptSet <- .fullConceptSetCreation(conceptName = conceptName,
                                               originalConceptList = conceptIds,
                                               excludedConcepts = excludedConcepts,
                                               llmClient = llmClient,
                                               connectionDetails = connectionDetails,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                               minCount = minCount,
                                               belowMinimumCountApproach = belowMinimumCountApproach,
                                               outputDirectory = outputDirectory,
                                               additionalInformation = additionalInformation,
                                               excludedVocabularies = c("ICDO3"),
                                               clinicalContext = clinicalContext,
                                               bucketSize = bucketSize,
                                               conditionForFiles = conditionForFiles,
                                               tryNumber = tryNumber,
                                               phoebeExclusions = phoebeExclusions,
                                               quickRun = FALSE)

  } else { #too large to test - send message and use descendants only
    message(paste0("The number of concepts to test (", recordCount, ") exceeds the threshold of 2000.  Producing a concept set of concepts plus descendants"))
    finalConceptSet <- .createJsonforConceptsPlusDescendants(conceptIds,
                                                             connectionDetails = connectionDetails,
                                                             cdmDatabaseSchema = cdmDatabaseSchema)$expression
  }

  return(finalConceptSet)
}

.checkForExclusions <- function(term, llmClient) {
  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    term = ellmer::type_string(),
    yesNo = ellmer::type_enum(values = c("YES","NO")),
    rationale = ellmer::type_string(),
    exclusions = ellmer::type_string()
  ))

  prompt <- paste0("Does the term: ", term, " EXPLICITLY suggest there should be exclusions? ",
                   "Examples: 'psoriasis without arthritis' explicitly excludes arthritis whereas 'psoriatic arthritis' does not ",
                   "'nonrheumatic valve disorder' exclude rheumatic valve disorder ",
                   "If yes, create a COMPREHENSIVE set of Excluded terms - DO NOT include any associated billing codes, e.g., ICD10 codes",
                   "If yes, what are the exclusions? ",
                   "  {
                            \"term\": \"Name of term\",
                            \"yesNo\": \"YES or NO\",
                            \"rationale\": \"Rationale for exclusion\",
                            \"exclusions\": \"a COMPREHENSIVE set of Excluded terms\"
                            }")

  exclusions <- queryLLM(llmClient = llmClient, prompt = prompt, ellmerTypeObject = ellmerTypeObject)
  return(exclusions)
}

.createJsonforConceptsPlusDescendants <- function(conceptIds,
                                                  connectionDetails,
                                                  cdmDatabaseSchema) {
  # --- Params ----
  connectionDetails <- connectionDetails
  vocab_schema <- cdmDatabaseSchema
  concept_ids <- c(conceptIds)  # replace with your list

  # --- Fetch metadata for the concept_ids (only valid concepts) ---
  sql <- "
SELECT concept_id,
       concept_name,
       concept_code,
       domain_id,
       vocabulary_id,
       concept_class_id,
       standard_concept
FROM @vocab_schema.concept
WHERE concept_id IN (@concept_ids)
  AND (invalid_reason IS NULL OR invalid_reason = '')
"
  # SqlRender: pass the comma-separated list of ids
  sql <- SqlRender::render(sql,
                           vocab_schema = vocab_schema,
                           concept_ids = paste(concept_ids, collapse = ","))
  # Optional: translate to target dialect (if needed)
  # sql <- SqlRender::translate(sql, targetDialect = "postgresql")

  conn <- DatabaseConnector::connect(connectionDetails)
  meta <- DatabaseConnector::querySql(conn, sql)
  DatabaseConnector::disconnect(conn)

  # --- Build items: one item per input concept id, includeDescendants = TRUE ---
  items <- lapply(concept_ids, function(cid) {
    row <- meta[meta$concept_id == cid, , drop = FALSE]
    if (nrow(row) == 1) {
      concept_meta <- list(
        CONCEPT_ID = as.integer(row$concept_id),
        CONCEPT_NAME = row$concept_name,
        CONCEPT_CODE = row$concept_code,
        DOMAIN_ID = row$domain_id,
        VOCABULARY_ID = row$vocabulary_id,
        CONCEPT_CLASS_ID = row$concept_class_id,
        STANDARD_CONCEPT = row$standard_concept,
        STANDARD_CONCEPT_CAPTION = "Standard"
      )
    } else {
      # fallback if metadata missing (still valid for ATLAS import)
      concept_meta <- list(CONCEPT_ID = as.integer(cid),
                           CONCEPT_NAME = paste0("concept ", cid))
    }

    list(
      concept = concept_meta,
      isExcluded = FALSE,
      includeDescendants = TRUE,
      includeMapped = FALSE
    )
  })

  concept_set <- list(
    items = items
  )

  # convert to JSON (NA -> null)
  json_string <- jsonlite::toJSON(concept_set, pretty = TRUE, simplifyVector = FALSE, auto_unbox = TRUE)


  # return(json_string)
  return(concept_set)
}

.vectorSearchStandard <- function(term,
                                  domains = NULL,
                                  conceptClasses = NULL,
                                  limit = 10,
                                  maxRetries = 3,
                                  waitTime = 2) {
  #domains: Condition, Procedure, Drug, Visit, Measurement
  #conceptClasses: (c: Disorder, HCPCS, 	Clinical Observation), (p: Procedure, CPT4), (d: Ingredient, Clinical Drug, Branded Drug),
  # (v: Visit) (m: Clinical Observation, CPT4)

  conceptExclusionList <- c(4040400, 4040390) #exclude very broad terms
  term <- gsub("_",  " ", term)
  params <- list(
    q = term,
    #domain_id = paste(domains, collapse = ","),
    limit = limit
  )

  if (!is.null(domains)) {
    params$domain_id <- paste(domains, collapse = ",")
  }

  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }
  url <- "https://hecate.pantheon-hds.com/api/search_standard"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )

    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        data <- dplyr::bind_rows(data$concepts) |>
          SqlRender::snakeCaseToCamelCaseNames()
        data <- data[!(data$conceptId %in% conceptExclusionList),]
        return(cbind(data.frame(searchTerm = term, data)))
      } else {
        return(data.frame())
      }
    }
    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

.vectorSearch <- function(term,
                          domains,
                          conceptClasses = NULL,
                          vocabularyId,
                          standardConcept,
                          limit = 10,
                          maxRetries = 3,
                          waitTime = 2) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    vocabulary_id = vocabularyId,
    standard_concept = standardConcept,
    limit = limit
  )
  if (!is.null(conceptClasses)) {
    params$concept_class_id <- paste(conceptClasses, collapse = ",")
  }
  url <- "https://hecate.pantheon-hds.com/api/search"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )


    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        scoreData <- data[,c("concept_name", "score")]
        data <- dplyr::bind_rows(data$concepts)
        data <- merge(data, scoreData)

        data <- SqlRender::snakeCaseToCamelCaseNames(data)
      }

      return(data)
    }

    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

.vectorSearchAtc <- function(term,
                             domains,
                             vocabularyId,
                             limit = 10,
                             maxRetries = 3,
                             waitTime = 2) {
  params <- list(
    q = term,
    domain_id = paste(domains, collapse = ","),
    vocabulary_id = vocabularyId,
    standard_concept = "C",
    limit = limit
  )
  url <- "https://hecate.pantheon-hds.com/api/search"

  for (attempt in 1:maxRetries) {
    response <- tryCatch(
      {
        httr::GET(url, query = params)
      },
      error = function(e) {
        message(paste("Attempt", attempt, "failed with error:", e$message))
        return(NULL)
      }
    )


    if (!is.null(response) && httr::status_code(response) == 200) {
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content_text)

      if(length(data) > 0) {
        scoreData <- data[,c("concept_name", "score")]
        data <- dplyr::bind_rows(data$concepts)
        data <- merge(data, scoreData)

        data <- SqlRender::snakeCaseToCamelCaseNames(data)
      }

      return(data)
    }

    if (attempt < maxRetries) {
      message(sprintf(
        "Search failed for '%s' (Status: %s). Retrying in %s seconds...",
        term,
        if (is.null(response)) "Connection Error" else httr::status_code(response),
        waitTime
      ))
      Sys.sleep(waitTime)
    }
  }
  stop(sprintf("All %s attempts failed for term '%s'.", maxRetries, term))
}

.grabConcepts <- function(searchString,
                          llmClient,
                          domains,
                          classes,
                          excludedConcepts,
                          vectorSearchSize,
                          connectionDetails,
                          connection,
                          cdmDatabaseSchema,
                          additionalInformation,
                          clinicalContext,
                          minCount = 0,
                          belowMinimumCountApproach = "TEST ALL",
                          conditionForFiles,
                          tryNumber = 1,
                          outputDirectory,
                          phoebeExclusions) {

  if (file.exists(file.path(outputDirectory, paste0(conditionForFiles,
                                                    "_from_embVectors.csv")))) {
    # skip finding seeds if already exists
    message(paste0(
      "File ",
      file.path(outputDirectory, paste0(conditionForFiles,
                                        "_from_embVectors.csv")),
      " exists...skipping to next part of process."))
    prelimLlmResults <- utils::read.csv(file.path(outputDirectory, paste0(conditionForFiles,
                                                                          "_from_embVectors.csv")))
  } else {
    #get seed concepts from hecate
    seeds <- .vectorSearchStandard(term = searchString,
                                   domains = domains,
                                   conceptClasses = classes,
                                   limit = vectorSearchSize)

    conceptList <- seeds

    promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_generic_sensitive.txt", package = "Phenelope")
    #quick first pass - adjudicate hecate list to screen out bad fits
    message("\nTesting initial list for viable concept candidates...")
    prelimLlmResults <- .createRecommendListFromConcepts(query = searchString,
                                                         conceptList = conceptList$conceptId,
                                                         llmClient = llmClient,
                                                         prompt = promptToUse,
                                                         connectionDetails = connectionDetails,
                                                         connection = connection,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         excludedConcepts = "none",
                                                         additionalInformation = additionalInformation,
                                                         clinicalContext = clinicalContext,
                                                         bucketSize = 20)

    if(!is.null(prelimLlmResults)) {
      utils::write.csv(prelimLlmResults, file.path(outputDirectory, paste0(conditionForFiles,
                                                                           "_from_embVectors.csv")), row.names = F)
    }
  }
  #create full concept set using Phenelope (includes Phoebe and descendants)
  message("\nTesting viable concept candidates, their descendants, and PHOEBE recommendations...")
  conceptList <- as.numeric(c(prelimLlmResults$conceptId[prelimLlmResults$finalAnswer == "YES"]))
  if(length(conceptList) > 0) {
    llmResults <- .getFullConceptSet(conceptName = searchString,
                                     conceptIds = conceptList,
                                     llmClient = llmClient,
                                     minCount = minCount,
                                     belowMinimumCountApproach = belowMinimumCountApproach,
                                     domains = domains,
                                     excludedConcepts = excludedConcepts,
                                     conceptClasses = classes,
                                     additionalInformation = additionalInformation,
                                     connection = connection,
                                     connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     conditionForFiles = conditionForFiles,
                                     tryNumber = tryNumber,
                                     outputDirectory = outputDirectory,
                                     phoebeExclusions = phoebeExclusions,
                                     bucketSize = 20)
  } else {
    message("There are no viable concept candidates for this concept set.")
    return(NULL)
  }
}

getDrugClass <- function(drugName) {
  ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
    drugName = ellmer::type_string(),
    routeOfAdministrationYesNo = ellmer::type_enum(values = c("YES","NO")),
    routeOfAdministrationName = ellmer::type_string(),
    dosageFormYesNo = ellmer::type_enum(values = c("YES","NO")),
    dosageFormName = ellmer::type_string(),
    drugStrengthYesNo = ellmer::type_enum(values = c("YES","NO")),
    drugStrengthName = ellmer::type_string(),
    drugBrandYesNo = ellmer::type_enum(values = c("YES","NO")),
    drugBrandName = ellmer::type_string(),
    singleGroupDrug = ellmer::type_enum(values = c("SINGLE","GROUP")),
    singleGroupName = ellmer::type_string(),
    indicationSpecificYesNo = ellmer::type_enum(values = c("YES","NO")),
    indication = ellmer::type_string()
  ))

  promptUp <- system.file("prompts", "DetermineDrugCharacteristics.txt", package = "Phenelope")
  originalLines <- readLines(promptUp)

  drugString <- drugName

  updatedLines <- gsub("DRUG_NAME", drugString, originalLines)

  prompt <- paste(updatedLines, collapse = "\n")

  drugNameInformation <- queryLLM(llmClient = llmClient, prompt = prompt, ellmerTypeObject = ellmerTypeObject)

  if(drugNameInformation$routeOfAdministrationYesNo == "NO" &
     drugNameInformation$dosageFormYesNo == "NO" &
     drugNameInformation$drugStrengthYesNo ==  "NO" &
     drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Ingredient"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "NO" &
            drugNameInformation$drugStrengthYesNo ==  "NO" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Dose Group"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "NO" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Form"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "NO" &
            drugNameInformation$dosageFormYesNo == "NO" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug Comp"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            # drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "NO" ) {
    class <- "Clinical Drug, Quant Clinical Drug"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "NO" &
            drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug Form"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "NO" &
            drugNameInformation$dosageFormYesNo == "NO" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug Comp"
  } else if(drugNameInformation$routeOfAdministrationYesNo == "YES" &
            drugNameInformation$dosageFormYesNo == "YES" &
            drugNameInformation$drugStrengthYesNo ==  "YES" &
            drugNameInformation$drugBrandYesNo ==  "YES" ) {
    class <- "Branded Drug, Quant Branded Drug, Marketed Product"
  } else {
    class <- "Unknown"
  }

  drugNameInformation <- data.frame(drugClass = class, drugNameInformation)

  return(drugNameInformation)
}

.getDrugConceptSet <- function(searchString,
                              llmClientReasoning,
                              llmClientNonReasoning,
                              connectionDetails,
                              cdmDatabaseSchema,
                              additionalInformation = "",
                              outputDirectory,
                              clinicalContext = "") {

  connection3 <- suppressMessages(DatabaseConnector::connect(connectionDetails = connectionDetails))
  on.exit(DatabaseConnector::disconnect(connection3))

  promptUp <- system.file("prompts", "createDrugList.txt", package = "Phenelope")
  originalLines <- readLines(promptUp)

  domains <- c("Drug")
  drugClass <- getDrugClass(searchString)

  {
    ellmerTypeObject <- ellmer::type_array(ellmer::type_object(
      term = ellmer::type_string(),
      drugName = ellmer::type_string()))

    passes <- 5
    startNumber <- 100
    nextNumber <- 50
    llmSearchString <- searchString

    if(drugClass$singleGroupDrug == "SINGLE") { #can reduce level of search for a single drug search
      passes <- 3
      startNumber <- 5
      nextNumber <- 5
      llmSearchString <- drugClass$singleGroupName
    }

    {
      fullDrugList <- NULL
      cat(paste0("Finding list of drugs for ", searchString, "\n"))
      for(passUp in 1:passes) { # 3 passes for completeness sake
        updatedLines <- gsub("SEARCH_TERM", llmSearchString, originalLines)

        updatedLines <- gsub("CURRENT_LIST", paste0(fullDrugList$drugName, collapse = "; "), updatedLines)
        updatedLines <- gsub("START_NUMBER", startNumber, updatedLines)
        updatedLines <- gsub("NEXT_NUMBER", nextNumber, updatedLines)

        prompt <- paste(updatedLines, collapse = "\n")

        start <- Sys.time()
        drugList <- queryLLM(llmClient = llmClientReasoning, prompt = prompt, ellmerTypeObject = ellmerTypeObject)
        end <- Sys.time()
        elapsedSecs <- as.numeric(difftime(end, start, units = "secs"))
        if(passUp == 1) {initTime <- elapsedSecs}

        if(ncol(drugList) > 1) {
          drugList$passNumber <- passUp
          fullDrugList <- rbind(fullDrugList, drugList[, names(drugList) != "cost"])
        }

        fullDrugList <- unique(fullDrugList[, names(fullDrugList) != "cost"])
        # message (paste0("Search ", passUp, " of ", passes, " - Total drugs: ", nrow(fullDrugList)))
        cat(paste0("\rSearch ", passUp, " of ", passes, " - Total drugs: ", nrow(fullDrugList), "..."))

      }
      cat(paste0("done\n"))

      fullDrugList <- fullDrugList[, names(fullDrugList) != "cost"]
      drugList <- unique(fullDrugList)
    }
    utils::write.csv(fullDrugList, file.path(outputDirectory, paste0(searchString, "_from_LLM.csv")), row.names = F)

    limit <- 200
    standardConceptCode <- "S"

    if(drugClass$drugClass %in% c("Clinical Dose Group", "Branded Dose Group")) {
      standardConceptCode <- "C"
    }

    if(drugClass$drugClass %in% c("Ingredient")) {
      limit <- 1
    }

    conceptList <- NULL
    for(drugUp in seq_len(nrow(drugList))) {
      cat(paste0("\rFinding concept Ids for drug ", drugUp, " of ", nrow(drugList)))

      seeds <- .vectorSearch(term = drugList$drugName[[drugUp]],
                             domains = domains,
                             conceptClasses = drugClass$drugClass,
                             vocabularyId = "RxNorm, RxNorm Extension",
                             standardConcept = standardConceptCode,
                             limit = limit,
                             maxRetries = 3,
                             waitTime = 2)

      if(length(seeds)) {
        conceptList <- rbind(conceptList, seeds)
      }
    }

    if(length(conceptList)) {
      utils::write.csv(conceptList, file.path(outputDirectory, paste0(searchString, "_from_embVectors.csv")), row.names = F)
    }


    conceptList <- conceptList[,c("conceptId", "conceptName", "domainId", "vocabularyId", "conceptClassId", "standardConcept",
                                  "conceptCode")]
    conceptList <- unique(conceptList)
    conceptList <- conceptList[!is.null(conceptList$conceptId),]

    promptToUse <- system.file("prompts", "LLM_Prompt_for_PHOEBE_drug.txt", package = "Phenelope")

    #double check the list by passing to the llm using Phenelope
    if(length(conceptList) > 0) {
      llmConceptSet <- .createRecommendListFromConcepts(query = searchString,
                                                        conceptList = conceptList$conceptId,
                                                        llmClient = llmClientNonReasoning,
                                                        prompt = promptToUse,
                                                        connectionDetails = connectionDetails,
                                                        connection = connection3,
                                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                                        excludedConcepts = "none",
                                                        additionalInformation = additionalInformation,
                                                        clinicalContext = clinicalContext,
                                                        bucketSize = 20)
    } else {
      message(paste0("No concept Ids found for ", searchString, " perhaps try a different drug name."))
      return(NULL)
    }

    llmApprovedConcepts <- llmConceptSet[llmConceptSet$finalAnswer == "YES",]

    if(length(llmApprovedConcepts) == 0) {
      message(paste0("No LLM approved concept Ids found for ", searchString, " perhaps try a different drug name."))
      return(NULL)
    }
  }

  return(llmConceptSet)
}
