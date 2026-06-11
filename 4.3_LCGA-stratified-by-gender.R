# NOTES ========================================================================
# This script loads the cleaned dataset, performs the same filtering of missing values
# as the kmedoids script, and runs cluster analysis on a dataset stratified by sex



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
library(purrr)


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

# Step 1: remove cases with >=6 missing values across all promis values
RCT1 <- RCT %>% filter(rowSums(is.na(select(., all_of(allpf_RCT)))) < 6)

# Step 2: Remove cases missing the baseline value
RCT2 <- RCT1 %>% filter(!is.na(pf_base))

# Step 3: Remove cases missing all three starting promis values
RCT3 <- RCT2 %>% filter(rowSums(is.na(select(., all_of(startpf_RCT)))) != 3)

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

# Stratify by gender
RCT_long_male <- RCT_long %>% filter(sex == "Male")
RCT_long_female <- RCT_long %>% filter(sex == "Female")


# LCGA =========================================================================
# Set seed
set.seed(2002)

# Step 1: Create a 1-class unconditional model ---------------------------------
lcga1_male <- hlme(tscore ~ ns(time_cont, df=3), subject = "numeric_id", ng = 1, data = RCT_long_male)
lcga1_female <- hlme(tscore ~ ns(time_cont, df=3), subject = "numeric_id", ng = 1, data = RCT_long_female)

# Step 2: Create k-class iterations based on the 1-class model -----------------
# Gridsearch iterates the algorithm over different starting values and subsequently
# selects the best model. The starting values are also saved, and will be used in 
# later bootstrapping.
lcga3_male <- gridsearch(rep = 3, maxit = 70, minit= lcga1_male,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 3, 
                             data = RCT_long_male, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1_male))


lcga3_female <- gridsearch(rep = 3, maxit = 70, minit= lcga1_female,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 3, 
                             data = RCT_long_female, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1_female))



# Compare models using fit indices
summarytable(lcga1_male, lcga1_female, lcga3_male, lcga3_female)
summaryplot(lcga1_male, lcga3_male, which = "BIC")
summaryplot(lcga1_female, lcga3_female, which = "BIC")
summaryplot(lcga1_male, lcga3_male, which = "entropy")
summaryplot(lcga1_female, lcga3_female, which = "entropy")

summarytable(lcga3, lcga3_male, lcga3_female, 
             which = c("G", "loglik", "npm", "BIC", "entropy", "%class"))

table(RCT_long_male$lcga3)


# SAVE DATA OBJECTS ============================================================
save(lcga1_male, lcga1_female, lcga3_male, lcga3_female,
     file = here("1.data", "2.processed", "4.analyzed", "lcga_sex.RData"))




# ============================================================================
# VISUALIZE CLUSTERS
# ============================================================================
add_lcga_class <- function(data, model, name = "lcga3") {
  model$pprob %>%
    dplyr::select(numeric_id, class) %>%
    dplyr::rename(!!name := class) %>%
    dplyr::left_join(data, by = "numeric_id")
}

RCT_long_male   <- add_lcga_class(RCT_long_male, lcga3_male)
RCT_long_female <- add_lcga_class(RCT_long_female, lcga3_female)


# STANDARDISE CLUSTER LABELS
# Male
RCT_long_male$lcga3 <- recode(
  RCT_long_male$lcga3,
  `1` = "high recovery",
  `2` = "low recovery",
  `3` = "moderate recovery"
  )

RCT_long_male$lcga3 <- factor(RCT_long_male$lcga3, 
                              levels = c("high recovery", "moderate recovery", "low recovery"))

# Female
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




