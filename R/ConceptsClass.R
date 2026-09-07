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

asConcepts <- function(dataFrame, origin = NULL, status = NULL) {
  if (!is.null(origin)) {
    dataFrame <- dataFrame |>
      mutate(origin = !!origin)
  }
  if (!is.null(status)) {
    dataFrame <- dataFrame |>
      mutate(status = !!status)
  }
  class(dataFrame) <- c("Concepts", class(dataFrame))
  validateConcepts(dataFrame)
  return(dataFrame)
}

validateConcepts <- function(concepts) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(concepts, "Concepts", add = errorMessages)
  checkmate::assertNames(names(concepts),
                         must.include = c("conceptId",
                                          "conceptName",
                                          "vocabularyId",
                                          "domainId",
                                          "conceptClassId",
                                          "origin",
                                          "status"),
                         add = errorMessages)
  checkmate::assertSubset(concepts$origin,
                          choices = c("SEED",
                                      "DESCENDANT",
                                      "RECOMMENDED"),
                          add = errorMessages)
  checkmate::assertSubset(concepts$status,
                          choices = c("UNADJUDICATED",
                                      "APPROVED",
                                      "REJECTED"),
                          add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (any(duplicated(concepts$conceptId))) {
    stop("Cannot have duplicate concepts in a Concepts object.")
  }
}

asConceptSetExpression <- function(concepts, name, connection, vocabDatabaseSchema) {
  validateConcepts(concepts)

  approvedConceptIds <- concepts |>
    filter(.data$status == "APPROVED") |>
    pull(.data$conceptId)
  conceptSet <- Capr::cs(approvedConceptIds, name = name)

  conceptSet <- Capr::getConceptSetDetails(conceptSet, connection, vocabularyDatabaseSchema = vocabDatabaseSchema)
  return(conceptSet)
  # conceptSet <- jsonlite::fromJSON(Capr::as.json(conceptSet))
}

getConceptsFromIds <- function(conceptIds, origin = "SEED", status = "UNADJUDICATED", connection, vocabDatabaseSchema) {
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
  concepts <- asConcepts(concepts, origin = origin, status = status)
  return(concepts)
}
