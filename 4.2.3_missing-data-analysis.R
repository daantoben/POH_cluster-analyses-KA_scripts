# This script takes the data prior to the filtering of NA values in the kmedoids and 
# LCGA script and compares it to the dataset after filtering to compare them.
# It also does some missing data analysis exploration 
# And it computes a Little's MCAR test.


# SET-UP =======================================================================
# Libraries
library(dplyr)              # for data wrangling - everything else
library(tidyr)              # for data wrangling - stretching data, like wide/long
library(forcats)            # for wrangling with factors
library(readr)              # for reading and writing rds files
library(here)               # for relative pathways
library(crosstable)
library(gtsummary)
library(naniar)
library(finalfit)

# Load data
RCT_pre <- read_rds(file =  here("1.data", "2.processed", "3.cleaned", "RCT_new.rds"))
RCT_post <- read_rds(file =  here("1.data", "2.processed", "4.analyzed", "RCT_wclusters.rds"))
RCT_long <- read_rds(file =  here("1.data", "2.processed", "4.analyzed", "RCT_wclusters.rds"))

# Widen 
RCT_post <- RCT_post %>% select(-time_cont) %>% 
  pivot_wider(names_from = time, values_from = tscore)

RCT <- RCT_long %>% 
  select(-time_cont) %>%
  pivot_wider(names_from = time, values_from = tscore)

# Extract filtered rows
resp_na <- setdiff(RCT_pre$respondent_id, RCT_post$respondent_id)

# Identify filtered rows in the original dataset using the resp_na string
RCT_pre <- RCT_pre %>%
  mutate(na = if_else(respondent_id %in% resp_na, 1, 0))

# Create a descriptive table comparing variables between included and excluded patients
RCT_pre %>% 
  tbl_summary(
    by = na,
    include = c(surgery, sex, age, bmi, prior_hip, mvi_total, employment,
                education),
    missing = "no",
    statistic = list(
      surgery ~ "{n} ({p}%)",
      sex ~ "{n} ({p}%)",
      age ~ "{mean} ({sd})",
      bmi ~ "{median} ({p25}-{p75})",
      prior_hip ~ "{n} ({p}%)",
      mvi_total ~ "{median} ({p25}-{p75})",
      employment ~ "{n} ({p}%)",
      education ~ "{n} ({p}%)"
    )
  ) %>%
  add_p() %>%
  add_overall()

# Visualize it
gg_miss_var(RCT)

# Test it with t-tests
RCT_t <- RCT %>% rename(
  pf_4w = `4 weeks`,
  pf_6w = `6 weeks`,
  pf_8w = `8 weeks`,
  pf_4m = `4 months`,
  pf_5m = `5 months`,
  pf_6m = `6 months`,
  pf_9m = `9 months`,
  pf_12m = `12 months`
)

explanatory = c("vas_pain_avg", "surgery", "age", "sex", "education", "expectations_rtw",
                "mvi_total", "OK_primair", "bmi")

# for pf_4w
dependent = "pf_4w"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r")) # For illustrative
# purpose only. Could skip but looks nice
# Almost a difference in pre-op fatigue... makes sense given the short follow-up. Pre-op fatigue
# might very well impact people's ability/motivation to fill in a questionnaire at 4 weeks
# follow -up

# for pf_6w
dependent = "pf_6w"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))

# for pf_8w
dependent = "pf_8w"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))

# for pf_4m
dependent = "pf_4m"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))
# pre-op Pain is different, higher in missing individuals 

# for pf_5m
dependent = "pf_5m"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))
# pre-op fatigue again...

# for pf_6m
dependent = "pf_6m"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))

# for pf_9m
dependent = "pf_9m"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))
# surgery type this time. More UKPs... maybe because they don't bother so late in follow-up
# to fill out a questionnaire? 

# for pf_12m
dependent = "pf_12m"
RCT_t %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))
# pre-op pain.... more pain in missing group.
# And fatigue again. More fatigue = more missing.

# MCAR test it
RCT_little <- RCT_t %>% select("vas_pain_avg", "surgery", "age", "sex", "education", "expectations_rtw",
                               "mvi_total", "OK_primair", "bmi", "baseline", "pf_4w", "pf_6w", "pf_8w",
                               "pf_4m", "pf_5m", "pf_6m", "pf_9m", "pf_12m")
mcar_test(RCT_little)
