library(haven)
library(tidyverse)
rise_data <- read_sas(
  "/Users/zihengzhang/Downloads/200B/200b-hw/project/rise.sas7bdat") |>
  select(-PSS_BL, -ISI_SCORE_BL, -PSQI_BL, -SF36V1_PCS_BL, -SF36V1_MCS_BL, 
         -FSI_SEVERITY_BL) |>
  select(ID, AGE_BL, RACE5, INCOME3, EDUC3, EMPLOY3, MARRIED2, STAGEDX_PG, 
         SURGTYPE_ENROLLMENT_BINARY, BMI_BL, CHARLSON, CTQ_WALKER_CAT_BL, 
         SCID_PHMDD, ALC_DRINKS_PAST_YEAR, CESD_BL) |>
  mutate(RACE5 = as_factor(RACE5), INCOME3 = as_factor(INCOME3), 
         EDUC3 = as_factor(EDUC3), EMPLOY3 = as_factor(EMPLOY3), 
         MARRIED2 = as_factor(MARRIED2), STAGEDX_PG = as_factor(STAGEDX_PG), 
         SURGTYPE_ENROLLMENT_BINARY = as_factor(SURGTYPE_ENROLLMENT_BINARY), 
         CTQ_WALKER_CAT_BL = as_factor(CTQ_WALKER_CAT_BL), 
         SCID_PHMDD = as_factor(SCID_PHMDD))

rows_with_missing <- !complete.cases(rise_data)
rise_data[rows_with_missing, ] |> print(n = sum(rows_with_missing), width = Inf)

rise_model <- lm(CESD_BL ~ AGE_BL + RACE5 + INCOME3 + EDUC3 + EMPLOY3 + 
                   MARRIED2 + STAGEDX_PG + SURGTYPE_ENROLLMENT_BINARY + 
                   BMI_BL + CHARLSON + CTQ_WALKER_CAT_BL + SCID_PHMDD + GODIN2 +
                   ALC_DRINKS_PAST_YEAR, data = rise_data)

