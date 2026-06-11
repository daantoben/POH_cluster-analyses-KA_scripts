# This script develops a number of successive models, aimed at 
# 1) the relationship between the cluster labels and Return to Work
# 2) the relationship between preoperative patient characteristics and the cluster labels

# Load libraries
library(dplyr)              # for data wrangling
library(tidyr)              # for data wrangling
library(forcats)            # for data wrangling
library(readr)              # for reading and writing rds files
library(here)               # for relative pathways
library(crosstable)         # for... crostables
library(ggplot2)            # for data visualization
library(RColorBrewer)       # for colours
library(haven)              # to load Carlien's RTW spss file
library(DescTools)          # for PseudoR2 values
library(nnet)               # to fit multinomial logistic regression models
library(stringr)            # for string manipulation
library(gtsummary)          # to make nice tables
library(survival)           # for survival analysis
library(survminer)          # also for survival analysis



# Load data
RCT_long <- read_rds(file =  here("1.data", "2.processed", "4.analyzed", "RCT_wclusters.rds"))
load(file=here("1.data", "2.processed", "4.analyzed", "lcga7_new.RData"))

# Relabel clusters and set moderate to reference value
RCT_long$lcga3 <- factor(
  RCT_long$lcga3,
  levels = c("A", "B", "C"),
  labels = c("high recovery",
             "low recovery",
             "moderate recovery")
)

# set reference level for modelling
RCT_long$lcga3 <- relevel(RCT_long$lcga3, ref = "moderate recovery")


# Widen
RCT <- RCT_long %>%
  select(-time_cont) %>%
  pivot_wider(names_from = time, values_from = tscore)

RCT$qol_health <- as.numeric(as.character(RCT$qol_health))

# Relevel variables
RCT$lcga3 <- relevel(RCT$lcga3, ref = "moderate recovery")
RCT$sex <- relevel(RCT$sex, ref = "Female")
RCT$surgery <- relevel(RCT$surgery, ref = "TKP") 

# Create regression weights denoting cluster uncertainty
postprobs <- lcga3$pprob

RCT <- merge(
  RCT,
  postprobs %>% select(-class),
  by = "numeric_id"
)

RCT$reg_weights <- apply(RCT[, grep("^prob", names(RCT))], 1, max)


# MODELLING ====================================================================
# Model 1: Cox regression clusters -> RTW

# First, factorize the censor variable
RCT$StatusRTWfull <- as.numeric(as_factor(RCT$StatusRTWfull)) -1

# Now build a Cox regression model
model1 <- coxph(
  Surv(RTWfulldays, StatusRTWfull) ~ lcga3,
  data = RCT,
  weights = reg_weights
)

surv_tbl <- tbl_regression(model1, intercept = TRUE) %>%
  bold_labels() %>%
  italicize_levels()

# Extract the fit for Kaplan Meier curves
RCT$lcga3 <- factor(
  RCT$lcga3,
  levels = c("high recovery",
             "moderate recovery",
             "low recovery")
)

sfit <- survfit(Surv(RTWfulldays, StatusRTWfull) ~ lcga3, data = RCT)

# Plot the kaplan meeier plot
colours <- c(
  "high recovery" = "#FC4E07",
  "moderate recovery" = "#00AFBB",
  "low recovery" = "#E7B800"
)

ggsurvplot(
  sfit,
  data = RCT,
  palette = colours,
  conf.int = TRUE,
  xlim = c(0, 365),
  xlab = "Return to Work",
  risk.table = TRUE,
  risk.table.col = "strata",
  risk.table.height = 0.25,
  risk.table.y.text = FALSE,
  ggtheme = theme_bw(),
  legend.title = "Cluster")


# Compute the test for proportional hazards
fit <- cox.zph(model2)
ggcoxzph(fit)



# Model 2: Multinomial regression 
# Running the model with weights
model2 <- multinom(lcga3 ~ age + sex + bmi + surgery + vas_pain_avg + 
                     qol_health + expectations_rtw + StatusRTWfull, data = RCT, 
                   weights = reg_weights)

# Extracting an R2 value
PseudoR2(model2, which = c("McFadden", "CoxSnell", "Nagelkerke"))

# Format the model into a table
multinom_tbl <- tbl_regression(model2, intercept = T,
               label = list(
                 age ~ "Age",
                 sex ~ "Sex",
                 bmi ~ "BMI",
                 surgery ~ "Surgery type",
                 vas_pain_avg ~ "Average pain past week",
                 qol_health ~ "EQ-5D VAS health score",
                 expectations_rtw ~ "Recovery expectations",
                 StatusRTWfull ~ "Censored from RTW")
) %>%
  bold_labels() %>%
  italicize_levels()







