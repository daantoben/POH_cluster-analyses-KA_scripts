# NOTES ========================================================================
# This script loads the cleaned dataset, performs the same filtering of missing values
# as the kmedoids script, and subsequently runs a butload of lcga models. It also computes BLRTs 
# comparing the fit between every consecuritve model. And it saves the results.

# This script was run in MyDRE due to the computing time

# SET-UP =======================================================================
# Libraries
library(dplyr)              # for data wrangling - everything else
library(forcats)            # for handling factors
library(tidyr)              # for data wrangling - stretching data, like wide/long
library(readr)              # for reading and writing rds files
library(here)               # for relative pathways
library(lcmm)               # for LCGA
library(ggplot2)            # for data visualization
library(hrbrthemes)         # for publication-ready themes
library(viridis)
library(splines)


# Load data
RCT <- read_rds(file =  here("1.data", "2.processed", "3.cleaned", "RCT_new.rds"))




# LCGA PREP ====================================================================
# Remove cases with too many missing promis values -----------------------------
# Promis columns
allpf_RCT <- c(
  "pf_base", "pf_4wks", "pf_6wks", "pf_8wks", "pf_3mnths", "pf_4mnths", 
  "pf_5mnths", "pf_6mnths", "pf_9mnths", "pf_12mnths"
)

# Promis columns close to surgery
startpf_RCT <- c("pf_4wks", "pf_6wks", "pf_8wks")

# Step 1: Remove cases missing the baseline value
RCT1 <- RCT %>% filter(!is.na(pf_base))

# Step 2: Remove cases missing all three starting promis values
RCT2 <- RCT1 %>% filter(rowSums(is.na(select(., all_of(startpf_RCT)))) != 3)

# Step 3: remove cases with >=6 missing values across all promis values
RCT3 <- RCT2 %>% filter(rowSums(is.na(select(., all_of(allpf_RCT)))) < 6)

# Step 4: remove cases with 4 consecutive missing values
na4_consecutive <- apply(RCT3[, allpf_RCT], 1, function(row) {
  any(rle(is.na(row))$lengths[rle(is.na(row))$values] >= 4)
})
RCT4 <- RCT3[!na4_consecutive, ]

# Make a numeric variable out of respondent_id. LCGA only accepts numeric subject ids.
RCT4$numeric_id = as.numeric(as.factor(RCT4$respondent_id))

# Elongate data 
RCT_long <- RCT4 %>%
  pivot_longer(cols = pf_base:pf_12mnths,
               names_to = "time",
               values_to = "tscore")

RCT_long$time <- as_factor(RCT_long$time) %>%
  recode(
    `pf_base` = "baseline",
    `pf_4wks` = "4 weeks",
    `pf_6wks` = "6 weeks",
    `pf_8wks` = "8 weeks",
    `pf_3mnths` = "3 months",
    `pf_4mnths` = "4 months",
    `pf_5mnths` = "5 months",
    `pf_6mnths` = "6 months",
    `pf_9mnths` = "9 months",
    `pf_12mnths` = "12 months"
  )

# Make time a continuous variable denoting time in months
# A month is 4.3 weeks
RCT_long$time_cont <- RCT_long$time
RCT_long$time_cont <- round(c(0, 4/4.3, 6/4.3, 8/4.3, 3, 4, 5, 6, 9 ,12)[as.numeric(RCT_long$time)], 2)




# LCGA =========================================================================
# Set seed
set.seed(2002)

# Step 1: Create a 1-class unconditional model ---------------------------------
lcga1 <- hlme(tscore ~ ns(time_cont, df=3), subject = "numeric_id", ng = 1, data = RCT_long)

# Step 2: Create k-class iterations based on the 1-class model -----------------
# Gridsearch iterates the algorithm over different starting values and subsequently
# selects the best model. The starting values are also saved, and will be used in 
# later bootstrapping.
lcga2 <- gridsearch(rep = 200, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 2, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

nlcga3 <- gridsearch(rep = 200, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 3, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga4 <- gridsearch(rep = 200, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 4, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga5 <- gridsearch(rep = 200, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 5, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga6 <- gridsearch(rep = 200, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 6, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga7 <- gridsearch(rep = 200, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 7, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))



# Compare models using fit indices
summarytable(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6, lcga7)
summaryplot(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6, lcga7, which = "BIC")
summaryplot(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6, lcga7, which = "entropy")

# Compare models using LRT
LR_2class_lcga <- -2 * (lcga1$loglik - lcga2$loglik)
pchisq(LR_2class_lcga, df = 1, lower.tail = FALSE)

LR_3class_lcga <- -2 * (lcga2$loglik - lcga3$loglik)
pchisq(LR_3class_lcga, df = 1, lower.tail = FALSE)

LR_4class_lcga <- -2 * (lcga3$loglik - lcga4$loglik)
pchisq(LR_4class_lcga, df = 1, lower.tail = FALSE)

LR_5class_lcga <- -2 * (lcga4$loglik - lcga5$loglik)
pchisq(LR_5class_lcga, df = 1, lower.tail = FALSE)

LR_6class_lcga <- -2 * (lcga5$loglik - lcga6$loglik)
pchisq(LR_6class_lcga, df = 1, lower.tail = FALSE)

LR_7class_lcga <- -2 * (lcga6$loglik - lcga7$loglik)
pchisq(LR_7class_lcga, df = 1, lower.tail = FALSE)







# SAVE DATA OBJECTS ============================================================
# Dataset
write_rds(RCT_long, file = here("1.data", "2.processed", "4.analyzed", "RCT_long.rds"))

# LCGA
save(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6, lcga7,
     file = here("1.data", "2.processed", "4.analyzed", "lcga7_new.RData"))

# BLRT
save(BLRT_1v2_lcga, BLRT_2v3_lcga, BLRT_3v4_lcga, BLRT_4v5_lcga, BLRT_5v6_lcga, BLRT_6v7_lcga, 
     file = here("1.data", "2.processed", "4.analyzed", "BLRT_lcga.RData"))


