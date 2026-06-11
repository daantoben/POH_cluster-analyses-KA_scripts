# ==============================================================================
# SETUP
# ==============================================================================
# Load libraries
library(dplyr)
library(tidyr)
library(lcmm)
library(survival)
library(survminer)
library(ggplot2)
library(here)
library(gtsummary)
library(gt)
library(knitr)
library(readr)
library(nnet)
library(forcats)

# Load data
RCT_long <- read_rds(file =  here("1.data", "2.processed", "4.analyzed", "RCT_wclusters.rds"))

load(file=here("1.data", "2.processed", "4.analyzed", "BLRT_lcga.RData"))
load(file=here("1.data", "2.processed", "4.analyzed", "lcga7_new.RData"))




# ==============================================================================
# DATA WRANGLING
# ==============================================================================

RCT_long$lcga3 <- forcats::recode(RCT_long$lcga3,
                                  A = "high recovery",
                                  B = "low recovery",
                                  C = "moderate recovery")

RCT_long$lcga3 <- factor(
  RCT_long$lcga3,
  levels = c("moderate recovery", "high recovery", "low recovery")
)

# Wide format
RCT <- RCT_long %>%
  select(-time_cont) %>%
  pivot_wider(names_from = time, values_from = tscore)

# Ensure numeric
RCT <- RCT %>%
  mutate(
    qol_health = as.numeric(as.character(qol_health)),
    vas_pain_avg = as.numeric(as.character(vas_pain_avg))
  )




# ==============================================================================
# TABLE 1: PATIENT CHARACTERISTICS
# ==============================================================================
RCT %>%
  tbl_summary(
    by = lcga3,
    include = c(
      age, sex, bmi, surgery, OK_primair, education,
      mvi_total, vas_pain_avg, qol_health, expectations_rtw,
      employment, work_hrs_mnth, RTWfulldays, StatusRTWfull
    ),
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      bmi ~ "BMI",
      surgery ~ "Surgery type",
      OK_primair ~ "Previous knee arthroplasty",
      education ~ "Educational attainment",
      mvi_total ~ "MVI total score",
      vas_pain_avg ~ "Average pain (past week)",
      qol_health ~ "EQ-5D VAS health score",
      expectations_rtw ~ "Recovery expectations",
      employment ~ "Employment type",
      work_hrs_mnth ~ "Work hours per month",
      RTWfulldays ~ "Time to RTW (days)",
      StatusRTWfull ~ "RTW status"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels()




# ==============================================================================
# TABLE 2: CLUSTER FIT STATISTICS 
# ==============================================================================
# Define each fit statistic
Models <- 1:7

AIC <- c(lcga1$AIC, lcga2$AIC, lcga3$AIC, lcga4$AIC,
         lcga5$AIC, lcga6$AIC, lcga7$AIC)

LL <- c(lcga1$loglik, lcga2$loglik, lcga3$loglik,
        lcga4$loglik, lcga5$loglik, lcga6$loglik, lcga7$loglik)

BIC <- c(lcga1$BIC, lcga2$BIC, lcga3$BIC,
         lcga4$BIC, lcga5$BIC, lcga6$BIC, lcga7$BIC)

AIC <- c(lcga1$AIC, lcga2$AIC, lcga3$AIC,
         lcga4$AIC, lcga5$AIC, lcga6$AIC, lcga7$AIC)

fit_df <- data.frame(Models, LL, AIC, BIC)
 
gt(fit_df)




# ==============================================================================
# FIGURE 1: CLUSTER TRAJECTORIES 
# ==============================================================================
# Stipulate colours
colours <- c("high recovery" = "#FC4E07",
             "moderate recovery" = "#00AFBB",
             "low recovery" = "#E7B800",
             "low-moderate recovery" = "#52854C",
             "high-moderate recovery" = "#00AFBB",
             "steep growth recovery" = "#293352")

# Change time to weeks
RCT_long <- RCT_long %>%
  mutate(time_weeks = time_cont * 4.345)

# Create the plot
ggplot(RCT_long, aes(time_weeks, tscore, group = respondent_id)) +
  stat_summary(
    aes(group = lcga3, colour = lcga3),
    geom = "line",
    fun = median,
    linewidth = 1.5
  ) +
  stat_summary(
    aes(group = lcga3, colour = lcga3),
    geom = "errorbar",
    fun.min = ~quantile(.x, 0.25),
    fun.max = ~quantile(.x, 0.75),
    fun = median,
    width = 0.2
  ) +
  scale_colour_manual(values = colours) +
  labs(
    x = "Time (weeks)",
    y = "PROMIS-PF T-score",
    colour = "Cluster"
  ) +
  theme_bw()




# ==============================================================================
# TABLE 3: PHYSICAL FUNCTION T-SCORES PER CLUSTER
# ==============================================================================
tbl_3 <- RCT %>% 
  tbl_summary(
    by = lcga3,
    include = c(
      baseline,
      `4 weeks`,
      `6 weeks`,
      `8 weeks`,
      `3 months`,
      `4 months`,
      `5 months`,
      `6 months`,
      `9 months`,
      `12 months`
    ),
    label = list(
      baseline ~ "Baseline",
      `4 weeks` ~ "4 weeks follow-up",
      `6 weeks` ~ "6 weeks follow-up",
      `8 weeks` ~ "8 weeks follow-up",
      `3 months` ~ "3 months follow-up",
      `4 months` ~ "4 months follow-up",
      `5 months` ~ "5 months follow-up",
      `6 months` ~ "6 months follow-up",
      `9 months` ~ "9 months follow-up",
      `12 months` ~ "12 months follow-up"
    ),
    missing = "no",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})"
    )
  ) %>%
  bold_labels()

tbl_3




# ==============================================================================
# TABLE 4: COX REGRESSION 
# ==============================================================================
# Create regression weights denoting cluster uncertainty
postprobs <- lcga3$pprob
RCT <- RCT %>%
  left_join(postprobs %>% select(numeric_id, class, starts_with("prob")),
            by = "numeric_id") %>%
  mutate(
    reg_weights = pmax(prob1, prob2, prob3, na.rm = TRUE),
    event = ifelse(StatusRTWfull == "wel", 1,
                        ifelse(StatusRTWfull == "niet", 0, NA))
  )

# Now build a Cox regression model
coxph(Surv(RTWfulldays, StatusRTWfull) ~ lcga3, data = RCT, weights = reg_weights)
summary(model1)

# Survival table
tbl_regression(model1, intercept = T
) %>%
  bold_labels() %>%
  italicize_levels()

# in hazard ratio form for the second column of the table
# HR
exp(c(0.72, -0.77))

# 95% CI
exp(c(0.41, 1.0,
      -1.3, -0.29))




# ==============================================================================
# TABLE 5: MULTINOMIAL REGRESSION 
# ==============================================================================
multinom_model <-  multinom(
  lcga3 ~ age + sex + bmi + surgery +
    vas_pain_avg + qol_health +
    expectations_rtw + StatusRTWfull,
  data = RCT,
  weights = reg_weights
)

# Format the model into a table
tbl_regression(
  multinom_model,
  exponentiate = TRUE,
  label = list(
    age ~ "Age",
    sex ~ "Sex",
    bmi ~ "BMI",
    surgery ~ "Surgery type",
    vas_pain_avg ~ "Pain",
    qol_health ~ "EQ-5D",
    expectations_rtw ~ "Expectations",
    StatusRTWfull ~ "RTW status"
  )
) %>%
  bold_labels() %>%
  italicize_levels()




# ==============================================================================
# APPENDIX C: CLUSTER PLOTS STRATIFIED BY SEX
# ==============================================================================
# Load stratified LCGA data objects
load(file = here("1.data", "2.processed", "4.analyzed", "lcga_sex.RData"))

# Add them to stratified dataframes
add_lcga_class <- function(data, model, name = "lcga3") {
  model$pprob %>%
    dplyr::select(numeric_id, class) %>%
    dplyr::rename(!!name := class) %>%
    dplyr::left_join(data, by = "numeric_id")
}

RCT_long_male   <- add_lcga_class(RCT_long_male, lcga3_male)
RCT_long_female <- add_lcga_class(RCT_long_female, lcga3_female)


# Standardize cluster labels
# Men
RCT_long_male$lcga3 <- recode(
  RCT_long_male$lcga3,
  `1` = "high recovery",
  `2` = "low recovery",
  `3` = "moderate recovery"
)

RCT_long_male$lcga3 <- factor(RCT_long_male$lcga3, 
                              levels = c("high recovery", "moderate recovery", "low recovery"))

# Women
RCT_long_female$lcga3 <- recode(
  RCT_long_female$lcga3,
  `1` = "moderate recovery",
  `2` = "low recovery",
  `3` = "high recovery"
)

RCT_long_female$lcga3 <- factor(RCT_long_female$lcga3, 
                                levels = c("high recovery", "moderate recovery", "low recovery"))

# COLOURS (CONSISTENT ACROSS ALL PLOTS)
colours <- c(
  "high recovery" = "#FC4E07",
  "moderate recovery" = "#00AFBB",
  "low recovery" = "#E7B800"
)

# Change time to weeks
RCT_long_male <- RCT_long_male %>%
  mutate(time_weeks = time_cont * 4.345)

RCT_long_female <- RCT_long_female %>%
  mutate(time_weeks = time_cont * 4.345)

# Male plot with table
RCT_long_male %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id)) +
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
  scale_colour_manual(values = colours) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "clusters"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )

RCT_long_female %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id)) +
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
  scale_colour_manual(values = colours) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "clusters"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )



# ==============================================================================
# APPENDIX D: ALL CLUSTER PLOTS
# ==============================================================================
RCT_long %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id)) +
  stat_summary(                       # median lines
    aes(group=lcga1, colour = lcga1), 
    geom = "line", 
    fun = "median", 
    linewidth=2.5) +
  geom_errorbar(                      
    aes(group = lcga1, colour = lcga1), 
    stat = "summary",
    fun.min = function(z) {quantile(z,0.25)},
    fun.max = function(z) {quantile(z,0.75)},
    fun = "median",
    width = 0.2     
  ) +
  scale_colour_manual(values = colours) +
  coord_cartesian(ylim = c(28, 61)) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "patterns"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )

# Plot two-cluster solution
RCT_long$lcga2 <- recode(RCT_long$lcga2, A = "high recovery", B = "moderate recovery")

RCT_long %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id, colour = lcga2)) +
  geom_line(alpha = 0.3) +
  stat_summary(                       # median lines
    aes(group=lcga2), 
    geom = "line", 
    fun = "median", 
    linewidth=2.5) +
  geom_errorbar(                      
    aes(group = lcga2), 
    stat = "summary",
    fun.min = function(z) {quantile(z,0.25)},
    fun.max = function(z) {quantile(z,0.75)},
    fun = "median",
    width = 0.2     
  ) +
  scale_colour_manual(values = colours) +
  coord_cartesian(ylim = c(28, 61)) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "patterns"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )

# Plot three-cluster solution
RCT_long$lcga3 <- recode(RCT_long$lcga3, 
                         A = "high recovery",
                         B = "low recovery",
                         C = "moderate recovery")

RCT_long %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id)) +
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
  scale_colour_manual(values = colours) +
  coord_cartesian(ylim = c(28, 61)) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "patterns"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )



# Plot four-cluster solution
RCT_long$lcga4 <- recode(RCT_long$lcga4, 
                         A = "steep growth recovery", 
                         B = "low recovery",
                         C = "moderate recovery",
                         D = "high recovery")

RCT_long$lcga4 <- factor(RCT_long$lcga4, levels = c(
  "steep growth recovery", "high recovery", "moderate recovery", "low recovery"))

RCT_long %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id)) +
  stat_summary(                       # median lines
    aes(group=lcga4, colour = lcga4), 
    geom = "line", 
    fun = "median", 
    linewidth=2.5) +
  geom_errorbar(                      
    aes(group = lcga4, colour = lcga4), 
    stat = "summary",
    fun.min = function(z) {quantile(z,0.25)},
    fun.max = function(z) {quantile(z,0.75)},
    fun = "median",
    width = 0.2     
  ) +
  scale_colour_manual(values = colours) +
  coord_cartesian(ylim = c(28, 61)) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "patterns"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )

# Plot five-cluster solution
RCT_long$lcga5 <- recode(RCT_long$lcga5, 
                         A = "steep growth recovery", 
                         B = "low recovery",
                         C = "low-moderate recovery",
                         D = "high recovery",
                         E = "moderate recovery")

RCT_long$lcga5 <- factor(RCT_long$lcga5, levels = c(
  "steep growth recovery", "high recovery", "moderate recovery", "low-moderate recovery", "low recovery"))

RCT_long %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id, colour = lcga5)) +
  stat_summary(                       # median lines
    aes(group=lcga5), 
    geom = "line", 
    fun = "median", 
    linewidth=2.5) +
  geom_errorbar(                      
    aes(group = lcga5), 
    stat = "summary",
    fun.min = function(z) {quantile(z,0.25)},
    fun.max = function(z) {quantile(z,0.75)},
    fun = "median",
    width = 0.2     
  ) +
  scale_colour_manual(values = colours) +
  coord_cartesian(ylim = c(28, 61)) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "patterns"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )

# Plot six-cluster solution
RCT_long$lcga6 <- recode(RCT_long$lcga6, 
                         A = "steep growth recovery", 
                         B = "low recovery",
                         C = "low-moderate recovery",
                         D = "high recovery",
                         E = "moderate recovery",
                         F = "relapse")

RCT_long$lcga6 <- factor(RCT_long$lcga6, levels = c(
  "steep growth recovery", "high recovery", "moderate recovery", 
  "low-moderate recovery", "low recovery", "relapse"))

RCT_long %>%
  ggplot(aes(x=time_weeks, y=tscore, group=respondent_id)) +
  stat_summary(                       # median lines
    aes(group=lcga6, colour = lcga6), 
    geom = "line", 
    fun = "median", 
    linewidth=2.5) +
  geom_errorbar(                      
    aes(group = lcga6, colour = lcga6), 
    stat = "summary",
    fun.min = function(z) {quantile(z,0.25)},
    fun.max = function(z) {quantile(z,0.75)},
    fun = "median",
    width = 0.2     
  ) +
  scale_colour_manual(values = colours) +
  coord_cartesian(ylim = c(28, 61)) +
  labs(
    x = "Time (weeks) after surgery", 
    y = "PROMIS-PF T-scores",
    colour = "patterns"
  ) +
  theme_bw() +
  theme(legend.position="top",
        plot.title = element_text(size=12)
  )




# ==============================================================================
# APPENDIX E: KAPLAN MEIER PLOT
# ==============================================================================
sfit <- survfit(Surv(RTWfulldays, event) ~ lcga3, data = RCT)


# Plot the kaplan meeier plot
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




# ==============================================================================
# APPENDIX F: COMPARISON OF INCLUDED AND EXCLUDED PATIENTS
# ==============================================================================
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
  mutate(excluded = respondent_id %in% resp_na)

# Create a descriptive table comparing variables between included and excluded patients
RCT_pre %>% 
  tbl_summary(
    by = excluded,
    include = c(
      age,
      sex,
      bmi,
      surgery,
      OK_primair,
      education,  
      mvi_total,
      vas_pain_avg,
      qol_health,
      expectations_rtw,
      employment,
      work_hrs_mnth,
      RTWfulldays,
      StatusRTWfull
    ),
    label = list(
      age ~ "Age",
      sex ~ "Sex",
      bmi ~ "BMI",
      surgery ~ "Surgery type",
      OK_primair ~ "Prior knee arthroplasty surgery",
      education ~ "Educational attainment",
      mvi_total ~ "MVI total score",
      vas_pain_avg ~ "Average pain past week",
      qol_health ~ "EQ-5D VAS health score",
      expectations_rtw ~ "Recovery expectations",
      employment ~ "terms of employment",
      work_hrs_mnth ~ "work hours/week",
      RTWfulldays ~ "Return to work; days",
      StatusRTWfull ~ "Censored from RTW"
    ),
    missing = "no",
    statistic = list(
      age ~ "{mean} ({sd})",
      sex ~ "{n} ({p}%)",
      bmi ~ "{median} ({p25}-{p75})",
      surgery ~ "{n} ({p}%)",
      OK_primair ~ "{n} ({p}%)",
      education ~ "{n} ({p}%)",
      mvi_total ~ "{median} ({p25}-{p75})",
      vas_pain_avg ~ "{mean} ({sd})",
      qol_health ~ "{mean} ({sd})",
      expectations_rtw ~ "{n} ({p}%)",
      employment ~ "{n} ({p}%)",
      work_hrs_mnth ~ "{median} ({p25}-{p75})",
      RTWfulldays ~ "{median} ({p25}-{p75})",
      StatusRTWfull ~ "{n} ({p}%)"
    )
  ) %>%
  add_p(pvalue_fun = label_style_pvalue(digits = 2)) %>%
  add_overall() %>%
  bold_labels()



