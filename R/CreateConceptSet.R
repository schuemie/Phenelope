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

#' Create a concept set
#'
#' @param name                      The name of the concept set to create. This should be an informative name reflecting
#'                                  the concept set target (the idea expressed by the concept set).
#' @param seedConceptIds            Optional: a set of one or more concept IDs that resemble the concept set target.
#' @param clinicalDefinition        Optional: a clinical definition of the concept set target.
#' @template LlmClient
#' @template ConnectionDetails
#' @template VocabDatabaseSchema
#' @template TempEmulationSchema
#' @param cacheFolder               Optional: Folder where intermediary results can be stored.
#' @template ExcludedVocabularyIds
#' @param findSeedConceptSettings   A settings object for finding seed concepts (when not provided in `seedConceptIds`)
#'                                  as created using `createFindSeedConceptSettings()`.
#' @param conceptRecommender        An object of class `ConceptRecommender` for recommending additional concepts.
#' @param conceptAdjudicator        An object of class `ConceptAdjudicator` for adjudicating recommended concepts.
#' @param condenseConceptSet        Condense the resulting concept set?
#'
#' @returns
#' A concept set expression
#'
#' @export
createConceptSet <- function(
    name,
    seedConceptIds = NULL,
    clinicalDefinition = NULL,
    llmClient,
    connectionDetails,
    vocabDatabaseSchema,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    cacheFolder = NULL,
    excludedVocabularyIds = c("ICDO3"),
    findSeedConceptSettings = createFindSeedConceptSettings(),
    conceptRecommender = HecateConceptRecomender$new(),
    conceptAdjudicator = DefaultConceptAdjudicator$new(),
    condenseConceptSet = TRUE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(name, len = 1, add = errorMessages)
  checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertIntegerish(seedConceptIds, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(vocabDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(cacheFolder, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(excludedVocabularyIds, add = errorMessages)
  checkmate::assertClass(findSeedConceptSettings, "FindSeedConceptSettings", add = errorMessages)
  checkmate::assertR6(conceptRecommender, "ConceptRecommender", add = errorMessages)
  checkmate::assertR6(conceptAdjudicator, "ConceptAdjudicator", add = errorMessages)
  checkmate::assertLogical(condenseConceptSet, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  start <- Sys.time()

  if (!is.null(cacheFolder) && !dir.exists(cacheFolder)) {
    dir.create(cacheFolder, recursive = TRUE)
  }

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  costTracker <- new.env()
  costTracker$amount <- 0

  message("Determining concept set domain")
  domain <- getDomain(name, llmClient, costTracker)
  domainSettings <- getdomainSettings(domain)
  message("- Domain: ", domain)

  if (!is.null(seedConceptIds) && length(seedConceptIds) > 0) {
    concepts <- getConceptInformation(conceptIds = seedConceptIds,
                                      connection = connection,
                                      vocabDatabaseSchema = vocabDatabaseSchema)
  } else {
    message("Finding seed concepts")
    concepts <- findSeedConcepts(name = name,
                                 llmClient = llmClient,
                                 costTracker = costTracker,
                                 connection = connection,
                                 vocabDatabaseSchema = vocabDatabaseSchema,
                                 findSeedConceptSettings = findSeedConceptSettings,
                                 domainIds = domainSettings$domainIds,
                                 conceptClassIds = domainSettings$conceptClassIds)
    message("- Found total of ", nrow(concepts), " seed concept IDs")
  }

  for (iteration in 1:2) {
    message("Starting iteration ", iteration)

    message("Getting recommendations")
    if (iteration == 1) {
      conceptIds <- concepts |>
        pull(.data$conceptId)
    } else {
      conceptIds <- concepts |>
        filter(.data$status %in% c("APPROVED")) |>
        pull(.data$conceptId)
    }
    recommendedConcepts <- conceptRecommender$recommendConcepts(conceptIds = conceptIds,
                                                                domainSettings = domainSettings,
                                                                excludedVocabularyIds = excludedVocabularyIds,
                                                                connection = connection,
                                                                vocabDatabaseSchema = vocabDatabaseSchema)
    recommendedConcepts <- recommendedConcepts |>
      filter(!.data$conceptId %in% concepts$conceptId)
    concepts <- concepts |>
      bind_rows(recommendedConcepts)
    message("- ", nrow(recommendedConcepts), " concepts added by recommender")

    message("Adjudicating recommendations")
    conceptsToAdjudicate <- concepts |>
      filter(.data$status %in% c("SEED", "DESCENDANT", "RECOMMENDED"))
    adjudicatedConcepts <- conceptAdjudicator$adjudicateConcepts(concepts = conceptsToAdjudicate,
                                                                 llmClient = llmClient,
                                                                 costTracker = costTracker)
    message("- Approved ", sum(adjudicatedConcepts$status == "APPROVED"), " of ", nrow(adjudicatedConcepts), " new concepts")

    concepts <- concepts |>
      filter(!.data$conceptId %in% adjudicatedConcepts$conceptId) |>
      bind_rows(adjudicatedConcepts)
  }

  conceptSetExpression <- asConceptSetExpression(concepts = concepts,
                                                 name = name,
                                                 connection = connection,
                                                 vocabDatabaseSchema = vocabDatabaseSchema)
  if (condenseConceptSet) {
    message("Condensing concept set expression")
    conceptSetExpression <- doCondense(conceptSetExpression = conceptSetExpression,
                                       connection = connection,
                                       vocabDatabaseSchema = vocabDatabaseSchema,
                                       tempEmulationSchema = tempEmulationSchema,
                                       excludedVocabularyIds = excludedVocabularyIds)
  }
  delta <- Sys.time() - start
  message("Creating concept set took ", signif(delta, 3), " ", attr(delta, "units"), " and cost $", costTracker$amount, ".")
  return(conceptSetExpression)
}

getDomain <- function(name, llmClient, costTracker) {
  prompt <- "
    Determine what domain category, DRUG, CONDITION, PROCEDURE, MEASUREMENT, VISIT, or DEVICE, the following term belongs to: %name%

    Output JSON only, using the following format:
    {
      \"domain\": \"Domain name\"
    }
  "
  prompt <- gsub("%name%", name, prompt)
  outputType <- ellmer::type_object(
    domain = ellmer::type_enum(values = c("DRUG","CONDITION","PROCEDURE","VISIT","DEVICE", "MEASUREMENT"))
  )
  domain <- queryLlm(prompt,
                     llmClient = llmClient,
                     costTracker = costTracker,
                     outputType = outputType)
  domain <- domain$domain
  return(domain)
}

#' Get the settings for a specific domain.
#'
#' @param domain The name of a domain (all caps), e.g. 'CONDITION'.
#'
#' @returns
#' An object of type `DomainSettings`.
#'
#' @export
getdomainSettings <- function(domain) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(domain, len = 1, add = errorMessages)
  checkmate::assertChoice(domain, choices = c("CONDITION",
                                              "PROCEDURE",
                                              "MEASUREMENT",
                                              "VISIT",
                                              "DEVICE",
                                              "DRUG"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (domain == "CONDITION") {
    domainSettings <- list(
      domainIds = c("Condition", "Observation"),
      conceptClassIds = c("Disorder", "HCPCS", 	"Clinical Observation", "Clinical Finding"),
      phoebeExclusions = c(),
      vectorSearchSize = 25
    )
  } else if (domain == "PROCEDURE") {
    domainSettings <- list(
      domainIds = c("Procedure","Device", "Observation"),
      conceptClassIds = c("Procedure", "CPT4", "Clinical Observation"),
      phoebeExclusions = c("Ontology-parent"),
      vectorSearchSize = 200
    )
  } else if (domain == "MEASUREMENT") {
    domainSettings <- list(
      domainIds = c("Measurement", "Observation"),
      conceptClassIds = c("CPT4", "Clinical Observation", "Procedure", "Lab Test"),
      phoebeExclusions = c("Ontology-parent"),
      vectorSearchSize = 200
    )
  } else if (domain == "VISIT") {
    domainSettings <- list(
      domainIds = c("Visit", "Provider", "Procedure", "Observation"),
      conceptClassIds = c("Visit"),
      phoebeExclusions = c("Ontology-parent"),
      vectorSearchSize = 200
    )
  } else if (domain == "DEVICE") {
    domainSettings <- list(
      domainIds = c("Procedure", "Device", "Observation"),
      conceptClassIds = c("Physical Object"),
      phoebeExclusions = c("Ontology-parent"),
      vectorSearchSize = 200
    )
  } else if (domain == "DRUG") {
    stop("The DRUG domain is currently not supported")
  }
  class(domainSettings) <- "DomainSettings"
  return(domainSettings)
}

doCondense <- function(conceptSetExpression, connection, vocabDatabaseSchema, tempEmulationSchema, excludedVocabularyIds) {
  conceptSetData <- fetchCondenserConceptSetData(
    conceptSetExpression = conceptSetExpression,
    connection = connection,
    cdmDatabaseSchema = vocabDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    excludedVocabularies = excludedVocabularyIds
  )
  condensedConceptSet <- condenseConceptSet(conceptSetData)
  return(condensedConceptSet)
}
