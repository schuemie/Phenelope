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

queryLlm <- function(prompt,
                     systemPrompt = "",
                     llmClient,
                     costTracker = NULL,
                     outputType = NULL) {

  llmClient$set_system_prompt(systemPrompt)

  maxRetries <- 10
  attempt <- 0
  success <- FALSE
  while (attempt <= maxRetries && !success) {
    tryCatch({
      attempt <- attempt + 1  # Increment the attempt count
      llmClient$set_turns(list())

      if (is.null(outputType)) {
        response <- llmClient$chat(prompt, echo = "none")
      } else {
        llmClient$set_turns(list())
        if (getOption("forceUnstructured", FALSE)) {
          # Currently, LM Studio doesn't play nice with ellmer as it relates to structured output.
          response <- llmClient$chat(prompt, echo = "none")
          response <- gsub("^\\s*```json|```\\s*$", "", response)
          response <- jsonlite::fromJSON(response)
        } else {
          response <- llmClient$chat_structured(prompt, echo = "none", type = outputType)
        }
      }
      success <- TRUE
      return(response)
    },
    error = function(e) {
      message("LLM attempt ", attempt, " failed: ", e$message)

      if (grepl("abort", e$message, ignore.case = TRUE)) {
        cat("Stopping the run as requested.\n")
        stop("Execution stopped by user.")
      }
      if (attempt >= maxRetries) {
        message("Reached attempt limit.")
        stop(e)
      }
    })
  }
  if (!is.null(costTracker)) {
    costTracker$amount <- costTracker$amount + llmClient$get_cost()
  }
  return(response)
}

getConceptInformation <- function(conceptIds, connection, vocabDatabaseSchema) {
  sql <- "
    SELECT concept_id,
      concept_name,
      vocabulary_id,
      domain_id,
      concept_class_id
    FROM @cdm_database_schema.concept
    WHERE concept_id IN (@concept_ids);
  "
  concepts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cdm_database_schema = vocabDatabaseSchema,
    concept_ids = conceptIds,
    snakeCaseToCamelCase = TRUE
  )
  return(concepts)
}

