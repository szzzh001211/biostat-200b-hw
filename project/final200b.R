library(haven)
library(tidyverse)

data <- read_sas('./rise.sas7bdat') |>
  select(-c(PSS_BL, ISI_SCORE_BL, PSQI_BL, SF36V1_PCS_BL, 
            SF36V1_MCS_BL, FSI_SEVERITY_BL, RACE4, HISPANIC,
            MFSI_TOT_BL, MAAS_BL, MENOSTATFINAL_BL, AGE_DX)) |>
  mutate(EMPLOY3 = ifelse(EMPLOY3 == "3 Not employed",
                       "0 Not employed", "1 Employed"),
         STAGEDX_PG = ifelse(STAGEDX_PG == 8, 3, STAGEDX_PG),
         SURGTYPE_ENROLLMENT_BINARY = ifelse(SURGTYPE_ENROLLMENT_BINARY == "",
                                             "no surgery",
                                             SURGTYPE_ENROLLMENT_BINARY),
         INCOME3 = ifelse(INCOME3 == "", NA, INCOME3)) |>
  mutate(MARRIED2 = as.factor(MARRIED2),
         EDUC3 = as.factor(EDUC3),
         EMPLOY3 = as.factor(EMPLOY3),
         INCOME3 = as.factor(INCOME3),
         CTQ_WALKER_CAT_BL = as.factor(CTQ_WALKER_CAT_BL),
         STAGEDX_PG = as.factor(STAGEDX_PG),
         SURGTYPE_ENROLLMENT_BINARY = as.factor(SURGTYPE_ENROLLMENT_BINARY),
         SCID_PHMDD = as.factor(SCID_PHMDD),
         RACE5 = as.factor(RACE5)) |>
  as_tibble()

hist(data$AGE_BL) #
hist(data$CESD_BL)
hist(log(data$CESD_BL+1))
hist(data$ALC_DRINKS_PAST_YEAR)
data2 <- data |>
  filter(ALC_DRINKS_PAST_YEAR <1500)
hist(log(data2$ALC_DRINKS_PAST_YEAR+1))
hist(data$BMI_BL) #
qqnorm(log(data$BMI_BL))

data2$CESD_BL <- log(data2$CESD_BL+1)
data2 <- na.omit(data2)
fit <- lm(CESD_BL ~ ., data = data2)
step(fit)

fit2 <- lm(CESD_BL ~ AGE_BL + MARRIED2 + INCOME3 + CTQ_WALKER_CAT_BL + CHARLSON + 
             ALC_DRINKS_PAST_YEAR + SURGTYPE_ENROLLMENT_BINARY + SCID_PHMDD + 
             RACE5, data = data2)
summary(fit2)
