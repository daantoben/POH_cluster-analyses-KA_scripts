# SET-UP =======================================================================
# Load libraries
library(haven)           # reads spss files
library(here)            # for standardized relative pathing
library(tidyverse)       # basic data wrangling

# Load data
t0 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t0.rds"))
promis <- read_rds(here("1.data", "2.processed", "2.PROMIS", "promis.rds")) %>% 
  as.data.frame()
rtw <- read_rds(here("1.data", "2.processed", "RTW.rds")) %>%
  as.data.frame()



# Change all e's in PatiëntID to z's for the rtw dataset
# so that they match the PROMIS PatiëntIDs outputted by the PROMIS scoring service
# Which was changed because Excel makes 10000000 out of PatientID 10e6. See script PROMIS_prep.r
rtw$PatiëntID <- str_replace_all(rtw$PatiëntID, "e", "z")



# DATA WRANGLING ===============================================================
# Select variables
t0 <- t0 %>% select(
  c(PatiëntID, OK_dt, OK_soort, OK_primair, Prog_hip, Randomisatie, Prog_gender, Prog_length, Prog_weight,
    Prog_age, Prog_education, Prog_living_situation, Prog_hours_last_month,
    Prog_employment, Prog_expectations_time_RTW, VAS_pain_now, VAS_pain_worst,
    VAS_pain_average, QoL_mobility, QoL_selfcare, QoL_daily_activities, QoL_pain,
    QoL_fear, QoL_health_scale, MVI_fit_1:MVI_physical_fit_1
    )
  )

promis <- promis %>%
  select(respondent_id, time, tscore)

# Rename variables
t0 <- t0 %>%
  rename(
    "respondent_id" = `PatiëntID`
  )

rtw <- rtw %>%
  rename(
    "respondent_id" = `PatiëntID`
  )

# Widen promis dataset for merging
promis <- promis %>% 
  pivot_wider(names_from = time,
              values_from = tscore)

# Join dataframes together based on the now identical respondent_id
RCT <- left_join(t0, promis, by='respondent_id') %>%
  left_join(., rtw, by='respondent_id') 

rm(t0, promis, rtw)

# Rename variables of joint dataframe
RCT <- RCT %>%
  rename(
    "surgery" = `OK_soort`,
    "prior_hip" = `Prog_hip`,
    "randomization" = `Randomisatie`,
    "sex" = `Prog_gender`,
    "length" = `Prog_length`,
    "weight" = `Prog_weight`,
    "age" = `Prog_age`,
    "education" = `Prog_education`,
    "living_sit" = `Prog_living_situation`,
    "work_hrs_mnth" = `Prog_hours_last_month`,
    "employment" = `Prog_employment`,
    "expectations_rtw" =`Prog_expectations_time_RTW`,
    "vas_pain_now" = `VAS_pain_now`,
    "vas_pain_worst" = `VAS_pain_worst`,
    "vas_pain_avg" = `VAS_pain_average`,
    "qol_mob" = `QoL_mobility`,
    "qol_sc" = `QoL_selfcare`,
    "qol_da" = `QoL_daily_activities`,
    "qol_pain" = `QoL_pain`,
    "qol_fear" = `QoL_fear`,
    "qol_health" = `QoL_health_scale`,
    "MVI_1" = `MVI_fit_1`,
    "MVI_2" = `MVI_low_energy_1`,
    "MVI_3" = `MVI_active_1`,
    "MVI_4" = `MVI_excited_1`,
    "MVI_5" = `MVI_tired_1`,
    "MVI_6" = `MVI_productive_1`,
    "MVI_7" = `MVI_focussed_1`,
    "MVI_8" = `MVI_physical_strong_1`,
    "MVI_9" = `MVI_notlookingforward_1`,
    "MVI_10" = `MVI_not_effective_1`,
    "MVI_11" = `MVI_concentrate_1`,
    "MVI_12" = `MVI_well_rested_1`,
    "MVI_13" = `MVI_attention_diff_1`,
    "MVI_14" = `MVI_physical_notfit_1`,
    "MVI_15" = `MVI_plans_1`,
    "MVI_16" = `MVI_tired_fast_1`,
    "MVI_17" = `MVI_not_productive_1`,
    "MVI_18" = `MVI_not_excited_1`,
    "MVI_19" = `MVI_distracted_1`,
    "MVI_20" = `MVI_physical_fit_1`,
    "pf_base" =  `0`,
    "pf_4wks" = `1`,
    "pf_6wks" = `2`,
    "pf_8wks" = `3`,
    "pf_3mnths" = `4`,
    "pf_4mnths" = `5`,
    "pf_5mnths" = `6`,
    "pf_6mnths" = `7`,
    "pf_9mnths" = `8`,
    "pf_12mnths" = `9`
  )

# Remove 6 respondents that were excluded from Carlien's study, see her flow chart
RCT <- RCT %>%
  filter(respondent_id != "1a10" & 
           respondent_id !=  "1g39" & 
           respondent_id != "7d23" & 
           respondent_id != "10b1" &
           respondent_id != "6c3" &
           respondent_id !="4a4")

# Compute BMI and place it after weight
RCT$bmi <- round(RCT$weight/(RCT$length/100)^2, digits = 1) 
RCT <- RCT %>% relocate(bmi, .after = weight)

RCT$bmi[RCT$bmi == 331.3] <- 33.1 # <- erroneous BMI, changed the decimal point same as Carlien

# Recode variables
RCT$prior_hip <- as_factor(RCT$prior_hip) %>%
  recode(`Ja, aan de linkerkant` = "Yes, leftside",
         `Ja, aan de rechterkant` = "Yes, rightside",
         `Ja, beide kanten` = "Yes, both sides",
         `Nee` = "No")

RCT$education <- as_factor(RCT$education) %>%
  recode(`Geen opleiding gevolgd of afgemaakt` = "low", 
         `Lager (beroeps) onderwijs` = "low",
         `MULO/MAVO/MBO/VMBO` = "medium",
         `HAVO/MMS/HBS/Gymnasium/VWO` = "medium",
         `Hoger beroepsonderwijs (HBO)` = "high",
         `Universiteit of hoger` = "high")

RCT$surgery <- as_factor(RCT$surgery)

RCT$sex <- as_factor(RCT$sex) %>%
  recode("Man" = "Male",
         "Vrouw" = "Female")

RCT$living_sit <- as_factor(RCT$living_sit) %>%
  recode(`Alleenstaand` = "Live alone",
         `Samenwonend met partner/gezin` = "Live with partner/family",
         `Samenwonend met kinderen (zonder partner)` = "Live with kids (without partner)")

RCT$employment <- as_factor(RCT$employment) %>%
  recode(
    "Volledig in loondienst" = "Salaried",
    "Volledig als zelfstandig ondernemer" = "Self-employed",
    "Zowel in loondienst als zelfstandig ondernemer" = "Both salaried and self-employed"
    )

RCT$expectations_rtw <- as_factor(RCT$expectations_rtw) %>%
  recode("Ja" = "Yes",
         "Nee" = "No")

RCT$randomization <- as_factor(RCT$randomization) %>%
  recode("Controle" = "Control",
         "Interventie" = "Intervention")

RCT$qol_mob <- as_factor(RCT$qol_mob)
RCT$qol_sc <- as_factor(RCT$qol_sc)
RCT$qol_da <- as_factor(RCT$qol_da)
RCT$qol_pain <- as_factor(RCT$qol_pain)
RCT$qol_fear <- as_factor(RCT$qol_fear)

# Insert Wol_health values as Carlien did, export went wrong
RCT$qol_health[RCT$respondent_id == "1a1"] <- 58
RCT$qol_health[RCT$respondent_id == "1a2"] <- 59
RCT$qol_health[RCT$respondent_id == "1a3"] <- 90
RCT$qol_health[RCT$respondent_id == "1a4"] <- 60
RCT$qol_health[RCT$respondent_id == "1a5"] <- 32
RCT$qol_health[RCT$respondent_id == "2a1"] <- 51
RCT$qol_health[RCT$respondent_id == "2a2"] <- 16
RCT$qol_health[RCT$respondent_id == "2z3"] <- 53
RCT$qol_health[RCT$respondent_id == "5a1"] <- 60
RCT$qol_health[RCT$respondent_id == "1a6"] <- 60
RCT$qol_health[RCT$respondent_id == "5c2"] <- 70

# Scoring the MVI questionnaire according to:
# https://meetinstrumentenzorg.nl/instrumenten/multidimensional-fatigue-index/ 
# and 
# https://www.embloom.nl/content/mvi/.
# Reverse score selected items
reverse_items <- c("MVI_2", "MVI_5", "MVI_9", "MVI_10", "MVI_13", "MVI_14",
                   "MVI_16", "MVI_17", "MVI_18", "MVI_19")

RCT[ , reverse_items] = 6 - RCT[ , reverse_items]

# Compute domain scores
RCT$mvi_gen_fatigue <- RCT$MVI_1 + RCT$MVI_5 + RCT$MVI_12 + RCT$MVI_16
RCT$mvi_phys_fatigue <- RCT$MVI_2 + RCT$MVI_8 + RCT$MVI_14 + RCT$MVI_20
RCT$mvi_cog_fatigue <- RCT$MVI_7 + RCT$MVI_11 + RCT$MVI_13 + RCT$MVI_19
RCT$mvi_reduc_activity <- RCT$MVI_3 + RCT$MVI_6 + RCT$MVI_10 + RCT$MVI_17
RCT$mvi_reduc_motivation <- RCT$MVI_4 + RCT$MVI_9 + RCT$MVI_15 + RCT$MVI_18

# Compute total score
RCT$mvi_total <- (RCT$mvi_gen_fatigue + RCT$mvi_phys_fatigue + RCT$mvi_cog_fatigue +
  RCT$mvi_reduc_activity + RCT$mvi_reduc_motivation)/5

# Remove MVI items
RCT <- RCT %>% select(-c(MVI_1: MVI_20))

# Reformat RTW from a character to a d-m-Y date class
RCT$RTW_start_date <- as.Date(RCT$RTW_start_date, format = "%d-%m-%Y")
RCT$RTW_start_date_full <- as.Date(RCT$RTW_start_date_full, format = "%d-%m-%Y")

# Compute RTW by subtracting the surgery date from the return to work date
RCT$RTW <- RCT$RTW_start_date - RCT$OK_dt
RCT$RTW_full <- RCT$RTW_start_date_full - RCT$OK_dt

sum(RCT$RTW < 0, na.rm = TRUE) # Should output 0
sum(RCT$RTW_full < 0, na.rm = TRUE) # Should output 2

sum(is.na(RCT$RTW))
sum(is.na(RCT$RTW_full))

# LASTLY
# hlme() requires numeric subject ids. So we need to create these whilst mapping them to
# the original respondent_ids
RCT <- RCT %>%
  mutate(numeric_id = as.numeric(factor(respondent_id))) %>%
  relocate(numeric_id, .after = respondent_id)

# If we need to go back from there:
# RCT <- RCT %>% 
# mutate(respondent_id = factor(numeric_id, labels = unique(respondent_id)))

# Save dataframe as rds file
write_rds(RCT, file = here("1.data", "2.processed", "3.cleaned", "RCT_new.rds"))

