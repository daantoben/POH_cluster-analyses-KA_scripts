# INTRODUCTION -----------------------------------------------------------------
# 05-07-2024
# This script loads Carlien's SPSS files and processes the PROMIS scores so they
# can be turned turned into t-scores through the scoring service 
# https://www.assessmentcenter.net/ac_scoringservice
# https://www.dutchflemishpromis.nl/images/upload/files/Handleiding%20Scoring%20Service%20v4.pdf




# SET-UP -----------------------------------------------------------------------
library(here)            # for relative pathways
library(tidyverse)       # for basic data wrangling
library(writexl)         # to export into Excel formats

# Load data
t0 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t0.rds"))
t1 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t1.rds"))
t2 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t2.rds"))
t3 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t3.rds"))
t4 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t4.rds"))
t5 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t5.rds"))
t6 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t6.rds"))
t7 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t7.rds"))
t8 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t8.rds"))
t9 <- read_rds(here("1.data", "2.processed", "1.changed-respondent_ids", "t9.rds"))

# Select variables
pattern <- "^PF\\w{1}\\d+\\w*\\d*$"
matched_columns <- names(t1)[grepl(pattern, names(t1))]
filtered_columns <- matched_columns[!grepl("dt", matched_columns)]

# Select the filtered columns
t0 <- t0 %>% select(c(1, matches(pattern)))
t1 <- t1 %>% select(c(1, all_of(filtered_columns)))
t2 <- t2 %>% select(c(1, all_of(filtered_columns)))
t3 <- t3 %>% select(c(1, all_of(filtered_columns)))
t4 <- t4 %>% select(c(1, all_of(filtered_columns)))
t5 <- t5 %>% select(c(1, all_of(filtered_columns)))
t6 <- t6 %>% select(c(1, all_of(filtered_columns)))
t7 <- t7 %>% select(c(1, all_of(filtered_columns)))
t8 <- t8 %>% select(c(1, all_of(filtered_columns)))
t9 <- t9 %>% select(c(1, all_of(filtered_columns)))

# Add measurement time column to dataframes so that this information stays preserved
# when we combine them
t0$Assmnt <- 0
t1$Assmnt <- 1
t2$Assmnt <- 2
t3$Assmnt <- 3
t4$Assmnt <- 4
t5$Assmnt <- 5
t6$Assmnt <- 6
t7$Assmnt <- 7
t8$Assmnt <- 8
t9$Assmnt <- 9

# Bind rows together, creating the long format the scoring service requires
combined_df <- rbind(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)

# Shuffling and renaming columns to suit scoring service requirements
combined_df <- rename(combined_df, Pin = PatiëntID)
combined_df <- combined_df %>% relocate(Assmnt, .after = Pin)


# Save in csv format
write.csv(combined_df, file = here(
  "1.data", "2.processed", "2.PROMIS", "input_physicalfunction.csv"), row.names=F)




# Debug check
table(apply(X = is.na(t0), MARGIN = 1, FUN = sum) <29)
table(apply(X = is.na(t9), MARGIN = 1, FUN = sum) <29)

