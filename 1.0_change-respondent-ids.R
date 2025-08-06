# INTRODUCTION -----------------------------------------------------------------
# 12-03-2025

# The PROMIS scoring service requires csv files as input
# Unfortunately, the original PatiëntID values include codes with e, like 10e6
# Excel interprets such figures as 10^6 and outputs 10000000... Stupid Excel...
# So, we're going to change all occurences of e to z. 


# This script loads Carlien's original SPSS files and rewrites the PatiëntID values
# to replace "e" values with "z" values so that the Excel file used to score the PROMIS
# scores doesn't transform 10e6 to 10000000...



# SET-UP -----------------------------------------------------------------------
# Load libraries
library(haven)           # reads spss files
library(tidyverse)       # basic data wrangling
library(stringr)         # to change character strings
library(here)            # for relative pathways

# Load data and ZAP THE LABELS OMG WTF IS UP WITH HAVEN_LABELLED VARIABLES THAT MAKES THEM 
# IIIIIIIIIMPOOOOOOSSIBLE TO WRANGLE!!! 
# SHOULDVE NEVER SWITCHED FROM foreign to haven. Foreign never had this issue!!! :(
t0 <- zap_labels(read_spss(here("1.data", "1.original", "T0 clean.sav")))
t1 <- zap_labels(read_spss(here("1.data", "1.original", "T1 clean.sav")))
t2 <- zap_labels(read_spss(here("1.data", "1.original", "T2 clean.sav")))
t3 <- zap_labels(read_spss(here("1.data", "1.original", "T3 clean.sav")))
t4 <- zap_labels(read_spss(here("1.data", "1.original", "T4 clean.sav")))
t5 <- zap_labels(read_spss(here("1.data", "1.original", "T5 clean.sav")))
t6 <- zap_labels(read_spss(here("1.data", "1.original", "T6 clean.sav")))
t7 <- zap_labels(read_spss(here("1.data", "1.original", "T7 clean.sav")))
t8 <- zap_labels(read_spss(here("1.data", "1.original", "T8 clean.sav")))
t9 <- zap_labels(read_spss(here("1.data", "1.original", "T9 clean.sav")))

# Change "e" to "z"
t0$PatiëntID <- str_replace_all(t0$PatiëntID, "e", "z")
t1$PatiëntID <- str_replace_all(t1$PatiëntID, "e", "z")
t2$PatiëntID <- str_replace_all(t2$PatiëntID, "e", "z")
t3$PatiëntID <- str_replace_all(t3$PatiëntID, "e", "z")
t4$PatiëntID <- str_replace_all(t4$PatiëntID, "e", "z")
t5$PatiëntID <- str_replace_all(t5$PatiëntID, "e", "z")
t6$PatiëntID <- str_replace_all(t6$PatiëntID, "e", "z")
t7$PatiëntID <- str_replace_all(t7$PatiëntID, "e", "z")
t8$PatiëntID <- str_replace_all(t8$PatiëntID, "e", "z")
t9$PatiëntID <- str_replace_all(t9$PatiëntID, "e", "z")

# Save files
write_rds(t0, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t0.rds"))
write_rds(t1, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t1.rds"))
write_rds(t2, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t2.rds"))
write_rds(t3, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t3.rds"))
write_rds(t4, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t4.rds"))
write_rds(t5, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t5.rds"))
write_rds(t6, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t6.rds"))
write_rds(t7, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t7.rds"))
write_rds(t8, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t8.rds"))
write_rds(t9, file = here("1.data", "2.processed", "1.changed-respondent_ids", "t9.rds"))