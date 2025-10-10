# This script develops a number of successive models, aimed at 
# 1) the relationship between the cluster labels and Return to Work
# 2) the relationship between preoperative patient characteristics and the cluster labels

# Load libraries
library(dplyr)              # for data wrangling
library(tidyr)              # for data wrangling
library(forcats)            # for data wrangling
library(readr)              # for reading and writing rds files
library(here)               # for relative pathways
library(crosstable)
library(ggplot2)            # for data visualization
library(RColorBrewer) 
library(haven)              # to load Carlien's RTW spss file
library(DescTools)          # for PseudoR2 values



# Load data
RCT_long <- read_rds(file =  here("1.data", "2.processed", "4.analyzed", "RCT_wclusters.rds"))
load(file=here("1.data", "2.processed", "4.analyzed", "lcga7_new.RData"))
for_rtw <- read_spss(file =  here("1.data", "1.original", "RTW COX analysebestand.sav"))

# Widen
RCT <- RCT_long %>% 
  select(-time_cont) %>%
  pivot_wider(names_from = time, values_from = tscore)

# Add most up-to-date version of RTW, cleaned by Carlien, to RCT.
# First, harmonize respondent_id
for_rtw$respondent_id <- str_replace_all(for_rtw$PatiëntID, "e", "z")
# Then join them by respondent_id
RCT <- RCT %>%
  left_join(for_rtw %>% select(respondent_id, StatusRTWfull, RTWfulldays), by = "respondent_id")


# Now its modelling time
# 1) the relationship between the cluster labels and Return to Work
model1 <- lm(RTWfulldays ~ relevel(lcga4, ref = "C"), data = RCT)
summary(model1)
plot(model1)

# With censorship in it
model2 <- lm(RTWfulldays ~ relevel(lcga4, ref = "C") + StatusRTWfull, data = RCT)
summary(model2)
plot(model2)

# Check the descriptives
crosstable(RCT, StatusRTWfull, by = lcga4) %>%
  as_flextable()

RCT %>% filter(StatusRTWfull == 0) %>%select(RTWfulldays)

# With three-class solution
model3 <- lm(RTWfulldays ~ relevel(lcga3, ref = "C"), data = RCT)
summary(model3)
plot(model3)

# With  censorship in it
model4 <- lm(RTWfulldays ~ relevel(lcga3, ref = "C") + StatusRTWfull, data = RCT)
summary(model4)
plot(model4)

# 2) the relationship between preoperative patient characteristics and the cluster labels
model5 <- multinom(lcga3 ~ age + sex + bmi + surgery + vas_pain_avg + 
               qol_health + expectations_rtw + StatusRTWfull, data = RCT)
summary(model5)
exp(coef(model5))

z <- summary(model5)$coefficients/summary(model5)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Naive model
model0 <- multinom(lcga3 ~ 1, data = RCT)
summary(model0)

# Get the postprobs
postprobs <- lcga3$pprob

RCT <- merge(RCT, postprobs, by = "numeric_id")
RCT$class <- as_factor(RCT$class) %>%
  recode(`1` = "A",
         `2` = "B",
         `3` = "C")
RCT$class == RCT$lcga3

# Oops, now with weights because lcga3 class membership isn't 100% accurate
postprobs <- lcga3$pprob
RCT <- merge(RCT, postprobs %>% select(c(-class)), by = "numeric_id")
RCT$class <- as_factor(RCT$class) %>%
  recode(`1` = "A",
         `2` = "B",
         `3` = "C")

# Making a weight variable
RCT$reg_weights <- apply(RCT[, grep("^prob", names(RCT))], 1, max)

# Rerunning the model with weights
model5 <- multinom(lcga3 ~ age + sex + bmi + surgery + vas_pain_avg + 
                     qol_health + expectations_rtw + StatusRTWfull, data = RCT, 
                   weights = reg_weights)

# Extracting an R2 value
PseudoR2(model5, which = c("McFadden", "CoxSnell", "Nagelkerke"))

# Format the model into a table
tbl_regression(model5, intercept = T,
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

# But step 1) doesn't really suit linear regression. Its censored time-series data...
# use Cox regression, duuh
library(survival)
library(survminer)

# First, factorize the censor variable
RCT$StatusRTWfull <- as.numeric(as_factor(RCT$StatusRTWfull)) -1

# Now build a Cox regression model
model6 <- coxph(Surv(RTWfulldays, StatusRTWfull) ~ class, data = RCT, weights = reg_weights)
summary(model6)

# Extract the fit for Kaplan Meier curves
sfit <- survfit(Surv(RTWfulldays, StatusRTWfull) ~ lcga3, data = RCT)

# Reorder the levels of lcga3 so that they go from high, to moderate, to low
RCT$lcga3 <- factor(RCT$lcga3,
                    levels = c("Good recovery", "Moderate recovery", "Poor recovery"))

# Plot the kaplan meeier plot
ggsurvplot(sfit, data = RCT, 
           palette = c(
             "#1B9E77", "#7570B3", "#D95F02"
             ),
           conf.int = T,
           xlim = c(0,365),
           xlab = "Return to Work",
           risk.table = T,
           risk.table.col = "strata",
           risk.table.height = 0.25,
           legend.labs = c(
             "Stable high function", "Stable moderate function", "Persistent dysfunction"),
           risk.table.y.text = FALSE,
           ggtheme = theme_bw()
  )

# Compute the test for proportional hazards
fit <- cox.zph(model6)
ggcoxzph(fit)

# Make a dichotomous variable for time so that you can split the model where, visually,
# the lines seem to intersect
RCT$tdich <- ifelse(RCT$RTWfulldays <= 80, 1, 0)

# Model the stratified relationship
model7 <- coxph(Surv(RTWfulldays, StatusRTWfull) ~ lcga3 + tdich:lcga3, 
                data = RCT, weights = reg_weights)
model7
# Nah, the interaction isn't significant. Nor was the ggcoxzph test




# Junk, a better plot for the clusters over time. 
RCT_long %>%
  ggplot(aes(x=time_cont, y=tscore, group=respondent_id)) +
  stat_summary(                       # median lines
    aes(group=lcga3, colour = lcga3), 
    geom = "line", 
    fun = "median", 
    linewidth=2.5) +
  geom_errorbar(                      
    aes(group = lcga3, colour = lcga3), 
    stat = "summary",
    fun.min = function(z) {quantile(z,0.25)},
    fun.max = function(z) {quantile(z,0.75)},
    fun = "median",
    width = 0.2     
  ) +
  scale_colour_manual(values = colours, breaks = c(
    "A", "C", "B"),
    labels = c(
      "Stable high function", "Stable moderate function", "Persistent dysfunction")) +
  scale_fill_manual(values = colours) +
  labs(
    x = "Time after surgery", 
    y = "Physical functioning",
    colour = "Clusters"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10),
        legend.position="top",
        plot.title = element_text(size=12),
        panel.grid = element_blank()
  )


