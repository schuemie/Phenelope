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

#' An abstract class for a concept adjudicator.
#'
#` @export
ConceptAdjudicator <- R6::R6Class(
  "ConceptAdjudicator",
  public = list(
    #' Adjudicate concepts
    #'
    #' @template Concepts
    #' @template LlmClient
    #' @template CostTracker
    #'
    #' @returns
    #' The same concepts as provided in the input, but with their `status` tag set to either 'APPROVED' or 'REJECTED',
    #' and an additional `rationale` column capturing the LLM rationale.
    #'
    #' @export
    adjudicateConcepts = function(concepts, llmClient, costTracker) {}
  )
)

#' The default concept adjudicator using the LLM and prompts.
#'
#` @export
DefaultConceptAdjudicator <- R6::R6Class(
  "DefaultConceptAdjudicator",
  inherit = ConceptAdjudicator,
  public = list(
    #' DefaultConceptAdjudicator constructor
    #'
    #' @param batchSize            The number of concepts to adjudicate at once.
    #' @param nForQuickScreen      The minimum number of concepts to trigger the quick screening.
    #' @param quickScreenBatchSize The number of concepts to enter quick screening at once.
    #'
    #' @returns
    #' This is the constructor.
    #'
    #' @export
    initialize = function(batchSize = 20,
                         nForQuickScreen = 100,
                         quickScreenBatchSize = 200) {
      private$batchSize = batchSize
      private$nForQuickScreen = nForQuickScreen
      private$quickScreenBatchSize = quickScreenBatchSize
    },
    #' @description
    #' Adjudicates concepts using the default LLM implementation.
    adjudicateConcepts = function(concepts, llmClient, costTracker) {
      validateConcepts(concepts)
      if (nrow(concepts) >= private$nForQuickScreen) {
        concepts <- quickScreen(concepts = concepts,
                                            name = name,
                                            clinicalDefinition = clinicalDefinition,
                                            batchSize = private$quickScreenBatchSize,
                                            llmClient = llmClient,
                                            costTracker = costTracker)
      }
      remainingConcepts <- concepts |>
        filter(.data$status != "REJECTED")

      if (nrow(remainingConcepts) > 0) {
        remainingConcepts <- adjudicate(concepts = remainingConcepts,
                                           name = name,
                                           clinicalDefinition = clinicalDefinition,
                                           batchSize = private$batchSize,
                                           llmClient = llmClient,
                                           costTracker = costTracker)
        concepts <- bind_rows(
          concepts |>
            filter(.data$status == "REJECTED"),
          remainingConcepts
        )
      }
      return(concepts)
    }
  ),
  private = list(
    batchSize = NULL,
    nForQuickScreen = NULL,
    quickScreenBatchSize = NULL
  )
)

quickScreen <- function(concepts, name, clinicalDefinition, batchSize, llmClient, costTracker) {
  # systemPromptFile <- "inst/prompts/QuickScreenSystem.txt"
  systemPromptFile <- system.file("prompts", "QuickScreenSystem.txt", package = "Phenelope")
  systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")

  # promptTemplateFile <- "inst/prompts/QuickScreen.txt"
  promptTemplateFile <- system.file("prompts", "QuickScreen.txt", package = "Phenelope")
  promptTemplate <- paste(readLines(promptTemplateFile), collapse = "\n")

  outputType <- ellmer::type_array(
    ellmer::type_object(
      conceptId = ellmer::type_integer(),
      conceptName = ellmer::type_string(),
      rationale = ellmer::type_string()
    )
  )

  conceptsToRemove <- list()
  start <- 1
  while (start <= nrow(concepts)) {
    end <- min(start + batchSize - 1, nrow(concepts))
    message("  Quick screen for concepts ", start, " to ", end, " out of ", nrow(concepts))
    batch <- concepts[start:end, ]
    prompt <- gsub("%name%", name, promptTemplate)
    if (is.null(clinicalDefinition) || clinicalDefinition == "") {
      # Delete entire line with %definition%
      prompt <- gsub("\n[^\n]*%definition%[^\n]*\n", "", prompt)
    } else {
      prompt <- gsub("%definition%", clinicalDefinition, prompt)
    }
    json <- batch |>
      select("conceptId", "conceptName") |>
      jsonlite::toJSON(pretty = TRUE)
    prompt <- gsub("%concepts%", json, prompt)

    conceptsToRemove[[length(conceptsToRemove) + 1]] <- queryLlm(prompt = prompt,
                                                                 systemPrompt = systemPrompt,
                                                                 llmClient = llmClient,
                                                                 costTracker,
                                                                 outputType = outputType)
    start <- end + 1
  }
  conceptsToRemove <- bind_rows(conceptsToRemove)

  concepts <- bind_rows(
    concepts |>
      filter(!.data$conceptId %in% conceptsToRemove$conceptId),
    concepts |>
      suppressWarnings(select(-one_of("rationale"))) |>
      inner_join(conceptsToRemove |>
                   select("conceptId", "rationale"),
                 by = join_by("conceptId")) |>
      mutate(status = "REJECTED")
  )

  return(concepts)
}

adjudicate <- function(concepts, name, clinicalDefinition, batchSize, llmClient, costTracker) {
  # systemPromptFile <- "inst/prompts/AdjudicationSystem.txt"
  systemPromptFile <- system.file("prompts", "AdjudicationSystem.txt", package = "Phenelope")
  systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")

  # promptTemplateFile <- "inst/prompts/Adjudication.txt"
  promptTemplateFile <- system.file("prompts", "Adjudication.txt", package = "Phenelope")
  promptTemplate <- paste(readLines(promptTemplateFile), collapse = "\n")

  outputType <- ellmer::type_array(
    ellmer::type_object(
      conceptId = ellmer::type_integer(),
      conceptName = ellmer::type_string(),
      decision = ellmer::type_enum(c("KEEP", "REMOVE")),
      rationale = ellmer::type_string()
    )
  )

  adjudicatedConcepts <- list()
  start <- 1
  while (start <= nrow(concepts)) {
    end <- min(start + batchSize - 1, nrow(concepts))
    message("  Adjudicating concepts ", start, " to ", end, " out of ", nrow(concepts))
    batch <- concepts[start:end, ]
    prompt <- gsub("%name%", name, promptTemplate)
    if (is.null(clinicalDefinition) || clinicalDefinition == "") {
      # Delete entire line with %definition%
      prompt <- gsub("\n[^\n]*%definition%[^\n]*\n", "", prompt)
    } else {
      prompt <- gsub("%definition%", clinicalDefinition, prompt)
    }
    json <- batch |>
      select("conceptId", "conceptName") |>
      jsonlite::toJSON(pretty = TRUE)
    prompt <- gsub("%concepts%", json, prompt)

    adjudicatedConcepts[[length(adjudicatedConcepts) + 1]] <- queryLlm(prompt = prompt,
                                                                       systemPrompt = systemPrompt,
                                                                       llmClient = llmClient,
                                                                       costTracker,
                                                                       outputType = outputType)
    start <- end + 1
  }
  adjudicatedConcepts <- bind_rows(adjudicatedConcepts)

  concepts <- concepts |>
      suppressWarnings(select(-one_of("rationale"))) |>
      inner_join(adjudicatedConcepts |>
                   select("conceptId", "decision", "rationale"),
                 by = join_by("conceptId")) |>
      mutate(status = if_else(.data$decision == "KEEP", "APPROVED", "REJECTED")) |>
    select(-"decision")

  return(concepts)
}
