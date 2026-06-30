select concept_id as concept_id, concept_name
from (VALUES @concept_list ) v(id)
JOIN @cdm_database_schema.concept c
  ON c.concept_id = v.id
--and upper(domain_id) in ('CONDITION', 'OBSERVATION', 'PROCEDURE', 'DRUG', 'MEASUREMENT')
where invalid_reason is NULL;


