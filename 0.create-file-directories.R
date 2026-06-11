# SET-UP -----------------------------------------------------------------------
# Load libraries

# CREATE DIRECTORIES -----------------------------------------------------------
# For original datafiles
dir.create("1.data/1.original", recursive = T)

# For processed datafiles - changed respondent ids
dir.create("1.data/2.processed/1.changed-respondent_ids", recursive = T)

# For processed datafiles - PROMIS
dir.create("1.data/2.processed/2.PROMIS", recursive = T)

# For processed datafiles - cleaned data
dir.create("1.data/2.processed/3.cleaned", recursive = T)

# For processed datafiles - analyzed data
dir.create("1.data/2.processed/4.analyzed", recursive = T)