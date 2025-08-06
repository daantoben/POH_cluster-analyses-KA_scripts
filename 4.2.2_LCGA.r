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




# LCGA =========================================================================
# Set seed
set.seed(2002)

# Step 1: Create a 1-class unconditional model ---------------------------------
lcga1 <- hlme(tscore ~ ns(time_cont, df=3), subject = "numeric_id", ng = 1, data = RCT_long)

# Step 2: Create k-class iterations based on the 1-class model -----------------
# Gridsearch iterates the algorithm over different starting values and subsequently
# selects the best model. The starting values are also saved, and will be used in 
# later bootstrapping.
lcga2 <- gridsearch(rep = 3, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 2, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga3 <- gridsearch(rep = 3, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 3, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga4 <- gridsearch(rep = 3, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 4, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga5 <- gridsearch(rep = 3, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 5, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga6 <- gridsearch(rep = 3, maxit = 70, minit= lcga1,
                    m = hlme(tscore ~  ns(time_cont, df=3), 
                             subject = "numeric_id", 
                             ng = 6, 
                             data = RCT_long, 
                             mixture = ~ ns(time_cont, df=3), 
                             B=lcga1))

lcga7 <- gridsearch(rep = 3, maxit = 70, minit= lcga1,
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




# Step 3: Rerun models with freely estimated growth factors --------------------
# This is implemented with the random=~1 argument
gmm_int1  <- hlme(tscore ~ ns(time_cont, df=3), subject = "numeric_id", random = ~1, ng = 1, data = RCT_long)

gmm_int2 <- gridsearch(rep = 3, maxit = 70, minit= gmm_int1,
                       m = hlme(tscore ~  ns(time_cont, df=3), 
                                subject = "numeric_id", 
                                ng = 2, 
                                data = RCT_long, 
                                random = ~ 1, 
                                mixture = ~ ns(time_cont, df=3), 
                                B=gmm_int1))

gmm_int3 <- gridsearch(rep = 3, maxit = 70, minit= gmm_int1,
                       m = hlme(tscore ~  ns(time_cont, df=3), 
                                subject = "numeric_id", 
                                ng = 3, 
                                data = RCT_long, 
                                random = ~ 1, 
                                mixture = ~ ns(time_cont, df=3), 
                                B=gmm_int1))

gmm_int4 <- gridsearch(rep = 3, maxit = 70, minit= gmm_int1,
                       m = hlme(tscore ~  ns(time_cont, df=3), 
                                subject = "numeric_id", 
                                ng = 4, 
                                data = RCT_long, 
                                random = ~ 1, 
                                mixture = ~ ns(time_cont, df=3), 
                                B=gmm_int1))

gmm_int5 <- gridsearch(rep = 3, maxit = 70, minit= gmm_int1,
                       m = hlme(tscore ~  ns(time_cont, df=3), 
                                subject = "numeric_id", 
                                ng = 5, 
                                data = RCT_long, 
                                random = ~ 1, 
                                mixture = ~ ns(time_cont, df=3), 
                                B=gmm_int1))

gmm_int6 <- gridsearch(rep = 3, maxit = 70, minit= gmm_int1,
                       m = hlme(tscore ~  ns(time_cont, df=3), 
                                subject = "numeric_id", 
                                ng = 6, 
                                data = RCT_long, 
                                random = ~ 1, 
                                mixture = ~ ns(time_cont, df=3), 
                                B=gmm_int1))

gmm_int7 <- gridsearch(rep = 3, maxit = 70, minit= gmm_int1,
                       m = hlme(tscore ~  ns(time_cont, df=3), 
                                subject = "numeric_id", 
                                ng = 7, 
                                data = RCT_long, 
                                random = ~ 1, 
                                mixture = ~ ns(time_cont, df=3), 
                                B=gmm_int1))

# Compare models using fit indices
summarytable(gmm_int1, gmm_int2, gmm_int3, gmm_int4, gmm_int5, gmm_int6, gmm_int7)
summaryplot(gmm_int1, gmm_int2, gmm_int3, gmm_int4, gmm_int5, gmm_int6, gmm_int7, which = "BIC")
summaryplot(gmm_int1, gmm_int2, gmm_int3, gmm_int4, gmm_int5, gmm_int6, gmm_int7, which = "entropy")

# Compare models using LRT
LR_2class_gmm <- -2 * (gmm_int1$loglik - gmm_int2$loglik)
pchisq(LR_2class_gmm, df = 1, lower.tail = FALSE)

LR_3class_gmm <- -2 * (gmm_int2$loglik - gmm_int3$loglik)
pchisq(LR_3class_gmm, df = 1, lower.tail = FALSE)

LR_4class_gmm <- -2 * (gmm_int3$loglik - gmm_int4$loglik)
pchisq(LR_4class_gmm, df = 1, lower.tail = FALSE)

LR_5class_gmm <- -2 * (gmm_int4$loglik - gmm_int5$loglik)
pchisq(LR_5class_gmm, df = 1, lower.tail = FALSE)

LR_6class_gmm <- -2 * (gmm_int5$loglik - gmm_int6$loglik)
pchisq(LR_6class_gmm, df = 1, lower.tail = FALSE)

LR_7class_gmm <- -2 * (gmm_int6$loglik - gmm_int7$loglik)
pchisq(LR_7class_gmm, df = 1, lower.tail = FALSE)


# Compare models using Bootstrapped LRT
# In order to bootstrap, we must write a function which resamples the data with
# replacement B times, refit the cluster models on the bootstrapped data, and calculate the
# LRT statistic for each k versus k-1 model. Finally, the p-value is the proportion of B
# in which the test statistic exceeds the original LR.

# The function:
# DON'T USE THIS FUNCTION FOR K=2 CAUSE THE MIXTURE=~TIME ARGUMENT DOESN'T WORK FOR 
# ONE-CLASS MODELS. INSTEAD, REMOVE THE ARGUMENT AND RUN IT WITHOUT IT. 
bootstrap_LRT <- function(data, k, B) {
  # Make a vector to hold the LRT stats
  LRT_stats <- numeric(B)
  
  for (i in 1:B) {
    # Print progress
    cat("Bootstrap iteration:", i, "of", B, "\n")
    # Sample unique respondents to preserve longitudinal structure
    sampled_ids <- sample(unique(data$numeric_id), replace = TRUE)
    boot_data <- data[data$numeric_id %in% sampled_ids, ]
    
    # Re-fit models on bootstrap sample
    model1_boot <- tryCatch({ # tryCatch makes sure a model not converging doesn;t break the rest of the loop
      hlme(tscore ~ ns(time_cont, df=3), 
           subject = "numeric_id", 
           B=get(paste0("gmm_int", k-1))$best, # use the starting values from the original models
           ng = k-1, 
           data = boot_data,
           mixture = ~ ns(time_cont, df=3),
           random = ~1)
    }, error = function(e) NULL)   # part of tryCatch, returns NULL if a model doesn't converge/work
    
    model2_boot <- tryCatch({
      hlme(tscore ~ ns(time_cont, df=3), 
           subject = "numeric_id", 
           B=get(paste0("gmm_int", k))$best,
           ng = k, 
           data = boot_data,
           mixture = ~ ns(time_cont, df=3),
           random = ~1)
    }, error = function(e) NULL)   
    
    # Check if models converged
    if (!is.null(model1_boot) && !is.null(model2_boot) && 
        model1_boot$conv == 1 && model2_boot$conv == 1) {
      
      # Calculate LRT statistic for bootstrap sample
      LRT_stats[i] <- -2 * (model1_boot$loglik - model2_boot$loglik)
    } else {
      LRT_stats[i] <- NA
    }
  }
  
  # Return LRT statistics
  return(na.omit(LRT_stats))
}

# Run bootstrap FUNCTION and save LRT values
# For LCGA
BLRT_1v2_lcga <- bootstrap_LRT(data=RCT_long, k=2, B=1000)

BLRT_2v3_lcga <- bootstrap_LRT(data=RCT_long, k=3, B=1000)

BLRT_3v4_lcga <- bootstrap_LRT(data=RCT_long, k=4, B=1000)

BLRT_4v5_lcga <- bootstrap_LRT(data=RCT_long, k=5, B=1000)

BLRT_5v6_lcga <- bootstrap_LRT(data=RCT_long, k=6, B=1000)

BLRT_6v7_lcga <- bootstrap_LRT(data=RCT_long, k=7, B=1000)



# For GMM_int 
BLRT_1v2_gmmint <- bootstrap_LRT(data=RCT_long, k=2, B=1000)

BLRT_2v3_gmmint <- bootstrap_LRT(data=RCT_long, k=3, B=1000)

BLRT_3v4_gmmint <- bootstrap_LRT(data=RCT_long, k=4, B=1000)

BLRT_4v5_gmmint <- bootstrap_LRT(data=RCT_long, k=5, B=1000)

BLRT_5v6_gmmint <- bootstrap_LRT(data=RCT_long, k=6, B=1000)

BLRT_6v7_gmmint <- bootstrap_LRT(data=RCT_long, k=7, B=1000)


# gmm_int vs lcga
four_lcgavgmmint <- bootstrap_LRT(data=RCT_long, k=4, B=1000)
LR4_lcga_gmm <- -2 * (lcga4$loglik - gmm_int4$loglik)
mean(four_lcgavgmmint >= LR4_lcga_gmm)


# SAVE DATA OBJECTS ============================================================
# Dataset
write_rds(RCT_long, file = here("1.data", "2.processed", "4.analyzed", "RCT_long.rds"))

# LCGA
save(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6, lcga7,
     file = here("1.data", "2.processed", "4.analyzed", "lcga7_new.RData"))

# GMM_int
save(gmm_int1, gmm_int2, gmm_int3, gmm_int4, gmm_int5, gmm_int6, gmm_int7,
     file = here("1.data", "2.processed", "4.analyzed", "gmm7_int_new.RData"))

# GMM_slo

# BLRT
save(BLRT_1v2_lcga, BLRT_2v3_lcga, BLRT_3v4_lcga, BLRT_4v5_lcga, BLRT_5v6_lcga, BLRT_6v7_lcga, 
     file = here("1.data", "2.processed", "4.analyzed", "BLRT_lcga.RData"))

save(BLRT_1v2_gmmint, BLRT_2v3_gmmint, BLRT_3v4_gmmint, BLRT_4v5_gmmint, BLRT_5v6_gmmint, BLRT_6v7_gmmint,
     file = here("1.data", "2.processed", "4.analyzed", "BLRT_gmmint.RData"))

save(four_lcgavgmmint,
     file = here("1.data", "2.processed", "4.analyzed", "BLRT_lcgavgmmint.RData"))
