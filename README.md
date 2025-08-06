# POH_cluster-analyses-KA_scripts
Repository for the cluster analysis project for POH in Amsterdam UMC. The repository holds R scripts used to load, process, and analyze RCT data to investigate heterogeneity in recovery after knee/hip arthroplasty.

The scripts are run in the following order:

0 = 0.create-file-directories.R
1 = 1.0_change-respondent-ids.R
2 = 2.0_PROMIS-prep.R
3 = 2.1_PROMIS-output-to-R.R
4 = 3.0_cleaning-promis-output-&-spss-files
5 = 4.2.1_kmedoids.R
6 = 4.2.2_LCGA.R <= DON'T RUN, TAKES AN AWFUL LOT OF COMPUTATION
7 = 4.2.3_missing-data-analysis.R
8 = 4.2.4_Regression_analyses.R
9 = 4.3.1_post-analysis-data-cleaning.R
10 = Quarto_final_v4.R
