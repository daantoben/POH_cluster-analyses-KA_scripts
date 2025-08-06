# NOTES ========================================================================
# This script takes the cleaned dataset, filters out specific missing values,
# runs a longitudinal kemdoids algorithm and saves the results


# SET-UP =======================================================================
# Libraries
library(dplyr)              # for data wrangling - everything else
library(tidyr)              # for data wrangling - stretching data, like wide/long
library(readr)              # for reading and writing rds files
library(here)               # for relative pathways
library(kml)                # for kmedoids


# Load data
RCT <- read_rds(file =  here("1.data", "2.processed", "3.cleaned", "RCT_new.rds"))


# KML ==========================================================================
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




# KML SETTINGS -----------------------------------------------------------------
# Set seed
set.seed(2002)

# Set the median as centroid and initial values at random generation
custom <- parALGO(centerMethod = function(x){median(x, na.rm = TRUE)},
                  startingCond = "randomK")

# Write function to extract the lowest criteria from kmed object
extract_criteria <- function(model, criterion) {
  sapply(2:6, function(k) {
    cluster_set <- slot(model, paste0("c", k))  # Use slot function to access slots
    min(sapply(1:20, function(i) cluster_set[[i]]@criterionValues[[criterion]]))
  })
}



# KML WITH NA REMOVED ----------------------------------------------------------
# Create k-medoids specific dataset
RCT_kmed4 <- RCT4 %>%
  select(respondent_id, all_of(allpf_RCT)) %>% as.data.frame()

# Create ClusterLongData object for kml to work on
cld_RCT4 <- cld(traj = RCT_kmed4, timeInData = 2:11)

# Run k-medoids
kml(cld_RCT4, nbClusters=2:8, nbRedrawing = 25, toPlot = "both", parAlgo = custom)

# Final model
choice(cld_RCT4)

# Plot all criteria
plotAllCriterion(cld_RCT4)

# Plot individual cluster membership
plot(cld_RCT4, 5, parTraj = parTRAJ(col = "clusters"), toPlot = "traj")

# Apply criteria extraction function
kmed_BIC4 <- extract_criteria(cld_RCT4, "BIC")*-1
kmed_AIC4 <- extract_criteria(cld_RCT4, "AIC")*-1

plot(kmed_BIC4)
plot(kmed_AIC4)

# BIC/AIC say 4 class model, not 2.
plot(cld_RCT4, 4, toPlot = "traj")

# Extract clusters
RCT4$kclass2 <- getClusters(cld_RCT4, 2)
RCT4$kclass3 <- getClusters(cld_RCT4, 3)
RCT4$kclass4 <- getClusters(cld_RCT4, 4)
RCT4$kclass5 <- getClusters(cld_RCT4, 5)
RCT4$kclass6 <- getClusters(cld_RCT4, 6)

# Save objects
save(cld_RCT4, file = here("1.data", "2.processed", "4.analyzed", "kmedoids.RData"))
write_rds(RCT4, file = here("1.data", "2.processed", "4.analyzed", "RCT4.rds"))




# KML WITH ONLY GENERAL NA REMOVAL ---------------------------------------------
# Create k-medoids specific dataset
RCT_kmed1 <- RCT1 %>%
  select(respondent_id, all_of(allpf_RCT)) %>% as.data.frame()

# Create ClusterLongData object for kml to work on
cld_RCT1 <- cld(traj = RCT_kmed1, timeInData = 2:11)

# Run k-medoids
kml(cld_RCT1, nbRedrawing = 20, toPlot = "both", parAlgo = custom)

# Final model
choice(cld_RCT1)

# Plot all criteria
plotAllCriterion(cld_RCT1)

# Plot individual cluster membership
plot(cld_RCT1, 2, parTraj = parTRAJ(col = "clusters"), toPlot = "traj")

# Apply criteria extraction function
kmed_BIC1 <- extract_criteria(cld_RCT1, "BIC")*-1
kmed_AIC1 <- extract_criteria(cld_RCT1, "AIC")*-1

plot(kmed_BIC1)
plot(kmed_AIC1)

# BIC/AIC say 3 class model, not 2.
plot(cld_RCT1, 3, toPlot = "traj")

# Extract clusters
RCT4$kclass2 <- getClusters(cld_RCT4, 2)
RCT4$kclass3 <- getClusters(cld_RCT4, 3)
RCT4$kclass4 <- getClusters(cld_RCT4, 4)
RCT4$kclass5 <- getClusters(cld_RCT4, 5)
RCT4$kclass6 <- getClusters(cld_RCT4, 6)


# SAVE DATA OBJECTS ============================================================
save(cld_RCT4, file = here("1.data", "2.processed", "4.analyzed", "kmedoids_new.RData"))
write_rds(RCT4, file = here("1.data", "2.processed", "4.analyzed", "RCT4.rds"))

