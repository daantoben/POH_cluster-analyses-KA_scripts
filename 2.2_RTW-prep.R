# INTRODUCTION -----------------------------------------------------------------
# 13-05-2024
# This script loads Carlien's original SPSS files in order to concetanate RTW data



# SET-UP -----------------------------------------------------------------------
# Set library path and load libraries
library(haven)           # reads spss files
library(tidyverse)       # basic data wrangling
library(writexl)         # exports Excel formats
library(stringr)         # for those pesky whitespaces in PatiëntID

# Load data
t0 <- read_spss(here("1.data", "1.original", "T0 clean.sav"))
t1 <- read_spss(here("1.data", "1.original", "T1 clean.sav"))
t2 <- read_spss(here("1.data", "1.original", "T2 clean.sav"))
t3 <- read_spss(here("1.data", "1.original", "T3 clean.sav"))
t4 <- read_spss(here("1.data", "1.original", "T4 clean.sav"))
t5 <- read_spss(here("1.data", "1.original", "T5 clean.sav"))
t6 <- read_spss(here("1.data", "1.original", "T6 clean.sav"))
t7 <- read_spss(here("1.data", "1.original", "T7 clean.sav"))
t8 <- read_spss(here("1.data", "1.original", "T8 clean.sav"))
t9 <- read_spss(here("1.data", "1.original", "T9 clean.sav"))




# RTW GLEANING -----------------------------------------------------------------
# Select RTW variables
t1 <- t1 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t2 <- t2 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t3 <- t3 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t4 <- t4 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t5 <- t5 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t6 <- t6 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t7 <- t7 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t8 <- t8 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)
t9 <- t9 %>% select(PatiëntID, RTW_start_date, RTW_start_date_full)

# Remove empty spaces from PatiëntID
t1$PatiëntID <- str_replace_all(t1$PatiëntID, fixed(" "), "")
t2$PatiëntID <- str_replace_all(t2$PatiëntID, fixed(" "), "")
t3$PatiëntID <- str_replace_all(t3$PatiëntID, fixed(" "), "")
t4$PatiëntID <- str_replace_all(t4$PatiëntID, fixed(" "), "")
t5$PatiëntID <- str_replace_all(t5$PatiëntID, fixed(" "), "")
t6$PatiëntID <- str_replace_all(t6$PatiëntID, fixed(" "), "")
t7$PatiëntID <- str_replace_all(t7$PatiëntID, fixed(" "), "")
t8$PatiëntID <- str_replace_all(t8$PatiëntID, fixed(" "), "")
t9$PatiëntID <- str_replace_all(t9$PatiëntID, fixed(" "), "")

# Sort all t dataframes according to PatiëntID otherwise coalesce messes up? Stupid coalesce...
t1 <- t1[order(t1[[1]]), ]
t2 <- t2[order(t2[[1]]), ]
t3 <- t3[order(t3[[1]]), ]
t4 <- t4[order(t4[[1]]), ]
t5 <- t5[order(t5[[1]]), ]
t6 <- t6[order(t6[[1]]), ]
t7 <- t7[order(t7[[1]]), ]
t8 <- t8[order(t8[[1]]), ]
t9 <- t9[order(t9[[1]]), ]

# Coalesce RTW dates across dataframes
t1$RTW_start_date  <- coalesce(t1$RTW_start_date, t2$RTW_start_date, t3$RTW_start_date,
                               t4$RTW_start_date, t5$RTW_start_date, t6$RTW_start_date,
                               t7$RTW_start_date, t8$RTW_start_date, t9$RTW_start_date)

t1$RTW_start_date_full  <- coalesce(t1$RTW_start_date_full, t2$RTW_start_date_full, 
                                    t3$RTW_start_date_full,t4$RTW_start_date_full, 
                                    t5$RTW_start_date_full, t6$RTW_start_date_full,
                                    t7$RTW_start_date_full, t8$RTW_start_date_full, 
                                    t9$RTW_start_date_full)
# Remove the hour timestamp
t1$RTW_start_date <- format(as.POSIXct(t1$RTW_start_date,format='%Y/%m/%d %H:%M:%S'),format='%d-%m-%Y')
t1$RTW_start_date_full <- format(as.POSIXct(t1$RTW_start_date_full,format='%Y/%m/%d %H:%M:%S'),format='%d-%m-%Y')




# Save
write_xlsx(t1, here("1.data", "2.processed", "RTW.xlsx"))
write_rds(t1, here("1.data", "2.processed", "RTW.rds"))
