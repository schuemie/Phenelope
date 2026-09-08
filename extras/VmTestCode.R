library(Phenelope)

llmClientO3 <- ellmer::chat_azure_openai(
  endpoint = keyring::key_get("genai_openai_endpoint"),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

cdmDatabaseSchema <- "merative_ccae.cdm_merative_ccae_v3789"

cd <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")


targetName <- "Acute liver failure"
targetClinicalDefinition <- "Acute liver failure is a rare but life-threatening syndrome characterized by the rapid deterioration of hepatocellular function, manifesting as significant coagulopathy and hepatic encephalopathy of any grade, developing within 28 days of the onset of jaundice or initial hepatic symptoms in an individual without evidence of pre-existing chronic liver disease or cirrhosis. The syndrome arises from a direct, primary insult to hepatocytes — including viral, toxic, drug-induced, autoimmune, metabolic, or indeterminate causes — and is conceptually distinct from liver dysfunction occurring as a secondary consequence of hemodynamic compromise (e.g., ischemic hepatitis, shock liver), systemic sepsis, or passive hepatic congestion from right-sided heart failure, all of which are explicitly excluded."

conceptSet <- createConceptSet(
  name = targetName,
  clinicalDefinition = targetClinicalDefinition,
  llmClient = llmClientO3,
  connectionDetails = cd,
  vocabDatabaseSchema = cdmDatabaseSchema,
  cacheFolder = "cacheAlf"
)
