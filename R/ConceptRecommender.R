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

#' The abstract concept recommender class
#` @export
ConceptRecommender <- R6::R6Class(
  "ConceptRecommender",
  public = list(

    #' Recommend concepts
    #'
    #' @param conceptIds A set of concept IDs to find related concepts for.
    #' @param domainSettings An object of type `DomainSettings` as created by `getDomainSettings()`.
    #' @template Connection
    #' @template VocabDatabaseSchema
    #' @template ExcludedVocabularyIds
    #'
    #' @returns
    #' An object of type `Concepts` containing the recommended concepts. Overlap with the input concepts is *not*
    #' removed.
    #'
    #' @export
    recommendConcepts = function(conceptIds, domainSettings, excludedVocabularyIds = NULL, connection, vocabDatabaseSchema) {}
  )
)

#' Concept recommender implementation using the Hecate Phoebe search.
#'
#` @export
HecateConceptRecomender <- R6::R6Class(
  "HecateConceptRecomender",
  inherit = ConceptRecommender,
  public = list(

    #' HecateConceptRecomender constructor
    #'
    #' @param minCount Minimum record count for concepts to be recommended.
    #' @param excludedVocabularies      Character vector of OHDSI Vocabulary names to exclude.
    #'
    #' @returns
    #' An object of type `HecateConceptRecomender`.
    #'
    #' @export
    initialize = function(minCount = 0, excludedVocabularies = c("ICDO3")) {
      private$minCount <- minCount
    },
    #' @description
    #' Recommends concepts using the Hecate Phoebe implementation.
    recommendConcepts = function(conceptIds, domainSettings, excludedVocabularyIds = NULL, connection, vocabDatabaseSchema) {
      message("  Adding descendants")
      descendants <- getDescendants(conceptIds = conceptIds,
                                    domainSettings = domainSettings,
                                    excludedVocabularyIds = excludedVocabularyIds,
                                    connection = connection,
                                    vocabDatabaseSchema = vocabDatabaseSchema)
      descendants <- descendants |>
        filter(!.data$conceptId %in% conceptIds)
      message("  - Found ", nrow(descendants), " additional concepts through descendants")

      message("  Getting Phoebe recommendations")
      recommendations <- getHecatePhoebeRecommendations(c(conceptIds, descendants$conceptId))
      recommendations <- recommendations |>
        filter(!.data$conceptId %in% c(conceptIds, descendants$conceptId))
      recommendations <- filterRecommendations(recommendations = recommendations,
                                               domainSettings = domainSettings,
                                               excludedVocabularyIds = excludedVocabularyIds,
                                               minCount = private$minCount,
                                               connection = connection,
                                               vocabDatabaseSchema = vocabDatabaseSchema)
      message("  - Found ", nrow(recommendations), " additional concepts through Phoebe recommendations")
      concepts <- bind_rows(
        descendants |>
          mutate(status = "DESCENDANT"),
        recommendations |>
          select("conceptId", "conceptName", "domainId", "conceptClassId", "vocabularyId") |>
          mutate(status = "RECOMMENDED")
      ) |>
        asConcepts()
      return(concepts)
    }
  ),
  private = list(
    minCount = NULL
  )
)

getHecatePhoebeRecommendations <- function(conceptIds) {
  phoebeUrlstring <- "https://hecate.pantheon-hds.com/api/concepts/phoebe/bulk"

  phoebeData <- list()

  start <- 1
  batchSize <- 500
  while (start <= length(conceptIds)) {
    end <- min(start + batchSize - 1, length(conceptIds))
    message("  Searching Phoebe for concepts ", start, " to ", end, " out of ", length(conceptIds))

    batch <- conceptIds[start:end]
    response <- httr::POST(phoebeUrlstring, body = list(ids = as.integer(batch)), encode = "json" )

    if (httr::status_code(response) == 200) {
      contentText <- httr::content(response, "text", encoding = "UTF-8")
      if (contentText == "[]") {
        data <- NULL
      } else {
        data <- jsonlite::fromJSON(contentText)
        data <- bind_rows(data$results)
        phoebeData[[length(phoebeData) + 1]] <- data
      }
    } else {
      stop(sprintf(
        "Error in phoebe search for concepts %s: %s",
        paste(batch, collapse = ", "),
        httr::status_code(response)
      ))
    }
    start <- end + 1
  }
  phoebeData <- bind_rows(phoebeData) |>
    SqlRender::snakeCaseToCamelCaseNames()
  return(phoebeData)
}

filterRecommendations <- function(recommendations, domainSettings, excludedVocabularyIds, minCount, connection, vocabDatabaseSchema) {
  conceptInformation <- getConceptInformation(conceptIds = unique(recommendations$conceptId),
                                              connection = connection,
                                              vocabDatabaseSchema = vocabDatabaseSchema)
  recommendations <- recommendations |>
    select("conceptId", "relationshipId", "recordCount") |>
    inner_join(conceptInformation |>
                 select("conceptId", "conceptName", "vocabularyId", "domainId", "conceptClassId"),
               by = join_by("conceptId"))
  if (!is.null(domainSettings$domainIds)) {
    recommendations <- recommendations |>
      filter(!.data$domainId %in% domainSettings$domainId)
  }
  if (!is.null(domainSettings$conceptClassIds)) {
    recommendations <- recommendations |>
      filter(!.data$conceptClassId %in% domainSettings$conceptClassIds)
  }
  if (!is.null(excludedVocabularyIds)) {
    recommendations <- recommendations |>
      filter(!.data$vocabularyId %in% excludedVocabularyIds)
  }
  if (!is.null(domainSettings$phoebeExclusions)) {
    recommendations <- recommendations |>
      filter(!.data$relationshipId %in% domainSettings$phoebeExclusions)
  }
  if (minCount > 0) {
    recommendations <- recommendations |>
      filter(.data$recordCount >= minCount)
  }
  recommendations <- recommendations |>
    select("conceptId", "conceptName", "vocabularyId", "domainId", "conceptClassId") |>
    distinct()
  return(recommendations)
}

getDescendants <- function(conceptIds, domainSettings, excludedVocabularyIds, connection, vocabDatabaseSchema) {
  sql <- "
    SELECT DISTINCT concept_id,
      concept_name,
      vocabulary_id,
      domain_id,
      concept_class_id
    FROM @cdm_database_schema.concept
    INNER JOIN @cdm_database_schema.concept_ancestor
      ON descendant_concept_id = concept_id
    WHERE ancestor_concept_id IN (@concept_ids)
    {@domain_ids != ''} ? { AND domain_id IN (@domain_ids)}
    {@concept_class_ids != ''} ? { AND concept_class_id IN (@concept_class_ids)}
    {@excluded_vocabulary_ids != ''} ? { AND vocabulary_id NOT IN (@concept_class_ids)}
    ;
  "
  descendants <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cdm_database_schema = vocabDatabaseSchema,
    concept_ids = conceptIds,
    domain_ids = paste(sprintf("'%s'", domainSettings$domainIds), collapse = ", "),
    concept_class_ids = paste(sprintf("'%s'", domainSettings$conceptClassIds), collapse = ", "),
    excluded_vocabulary_ids = paste(sprintf("'%s'", excludedVocabularyIds), collapse = ", "),
    snakeCaseToCamelCase = TRUE
  )
  return(descendants)
}
