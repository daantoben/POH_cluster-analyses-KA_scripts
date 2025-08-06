# SET-UP -----------------------------------------------------------------------
# Load libraries
library(tidyverse)       # basic data wrangling
library(here)            # for standardized relative path reading

# Load data
promis <- read_csv(here("1.data", "2.processed", "2.PROMIS", "output_physicalfunction.csv")) %>% 
  as.data.frame()

# Wrangle data
# Remove obsolete columns
promis <- promis[-c(1:4),-3]

# Provide sensible names
promis <- promis %>%
  rename(
    "respondent_id" = `Report Generated: 3/11/2025 3:27:38 AM`,
    "time" = `...2`,
    "raw_score" = `...4`,
    "theta" = `...5`,
    "tscore" = `...6`,
    "standard_error" = `...7`,
    "scored_cnt" = `...8`,
    "item_cnt" = `...9`
  )

# Save as an rds file
write_rds(promis, file = here("1.data", "2.processed", "2.PROMIS", "promis.rds"))

          