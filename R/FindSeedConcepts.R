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

#' Create settings for finding seed concepts
#'
#' @param addSynonyms Generate synonyms of the concept target name before search for matching concepts?
#' @param maxN Maximum number of seed concepts to return.
#' @param minCount Minimum number of record counts for a concept to be included.
#' @param fuzzyVocabSearchType Type of fuzzy vocabulary search. Currently only 'HECATE' is supported.
#'
#' @returns
#' A settings object.
#'
#' @export
createFindSeedConceptSettings <- function(addSynonyms = FALSE,
                                          maxN = 25,
                                          minCount = 0,
                                          fuzzyVocabSearchType = "HECATE") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertLogical(addSynonyms, len = 1, add = errorMessages)
  checkmate::assertIntegerish(maxN, len = 1, lower = 1, add = errorMessages)
  checkmate::assertIntegerish(minCount, len = 1, lower = 0, add = errorMessages)
  checkmate::assertCharacter(fuzzyVocabSearchType, len = 1, add = errorMessages)
  checkmate::assertChoice(fuzzyVocabSearchType, choices = c("HECATE"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  return(
    structure(
      as.list(environment()),
      class = "FindSeedConceptSettings"
    )
  )
}

#' Find seed concepts
#'
#' @param name                    The name to use to find concepts.
#' @template LlmClient
#' @template CostTracker
#' @param findSeedConceptSettings A setting object as created by `createFindSeedConceptSettings()`.
#' @param domainIds               Optional: A character vector of domain IDs to restrict to.
#' @param conceptClassIds         Optional: A character vector of concept class IDs to restrict to.
#'
#' @description
#' The `llmClient` is only used when `addSynonyms` is `TRUE` in the settings.#'
#'
#' @returns
#' Returns an object of type Concepts with the seed concepts.
#'
#' @export
findSeedConcepts <- function(name,
                             llmClient = NULL,
                             costTracker = NULL,
                             findSeedConceptSettings = createFindSeedConceptSettings(),
                             domainIds = NULL,
                             conceptClassIds = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, len = 1, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", null.ok = TRUE, add = errorMessages)
  # TODO: add more checks
  checkmate::reportAssertions(collection = errorMessages)
  if (findSeedConceptSettings$addSynonyms) {
    message("  Adding synonyms to name")
    names <- unique(c(name, getSynonyms(name, llmClient, costTracker)))
    message("  - added ", length(names) - 1, "synonyms")
  } else {
    names <- name
  }

  message("  Searching for seed concepts")
  searchResults <- list()
  for (i in seq_along(names)) {
    searchResults[[i]] <- searchConcepts(
      term = names[i],
      domainIds = domainIds,
      conceptClassIds = conceptClassIds,
      maxN = findSeedConceptSettings$maxN,
      fuzzyVocabSearchType = findSeedConceptSettings$fuzzyVocabSearchType
    ) |>
      filter(.data$recordCount >= findSeedConceptSettings$minCount)
  }
  searchResults <- mergeRankings(searchResults)
  seedConcepts <- searchResults |>
    slice_head(n = findSeedConceptSettings$maxN)
  message("  - Found ", nrow(seedConcepts), " seed concepts through fuzzy vocab search")

  concepts <- asConcepts(seedConcepts, status = "SEED")
  return(concepts)
}

getSynonyms <- function(name, llmClient, costTracker) {
  # promptFile <- "inst/prompts/Synonyms.txt"
  promptFile <- system.file("prompts", "Synonyms.txt", package = "Phenelope")
  prompt <- paste(readLines(promptFile), collapse = "\n")
  prompt <- gsub("%name%", name, prompt)
  outputType <- ellmer::type_object(synonyms = ellmer::type_array(ellmer::type_string()))
  synonyms <- queryLlm(
    prompt = prompt,
    llmClient = llmClient,
    costTracker = costTracker,
    outputType = outputType
  )
  synonyms <- synonyms$synonyms
  return(synonyms)
}

mergeRankings <- function(searchResults) {
  scoredSearchResults <- lapply(searchResults, function(df) {
    df |>
      mutate(
        rank = row_number(),
        score = n() - .data$rank + 1
      )
  })

  overallRanking <- bind_rows(scoredSearchResults) |>
    group_by(.data$conceptId, .data$conceptName, .data$vocabularyId, .data$domainId, .data$conceptClassId) |>
    summarise(
      totalScore = sum(.data$score),
      avgerageRank = mean(.data$rank),
      .groups = "drop"
    ) |>
    arrange(desc(.data$totalScore), .data$avgerageRank) |>
    select("conceptId", "conceptName", "vocabularyId","domainId", "conceptClassId")
  return(overallRanking)
}

