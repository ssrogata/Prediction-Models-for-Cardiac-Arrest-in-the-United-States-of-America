# Prediction-Models-for-Cardiac-Arrest-in-the-United-States-of-America
This repository contains the R scripts (uploaded version) used to develop and test the prediction models presented in the paper.

## Computational Environment
Analyses were conducted using R version 4.2.3 (2023-03-15) under Ubuntu 22.04.2 LTS (64-bit).  
Random number generation was performed using the L’Ecuyer-CMRG method.

### Main R Packages Used
The main R packages used were:
fixest_0.11.1
, Hmisc_5.0-1
, rlang_1.1.6
, ggsci_3.0.0
, doFuture_1.1.2
, future_1.67.0
, bonsai_0.4.0
, workflowsets_1.1.1
, workflows_1.2.0
, tune_1.3.0
, tidyr_1.3.1
, rsample_1.3.1
, recipes_1.3.1
, purrr_1.1.0
, parsnip_1.3.2
, modeldata_1.5.0
, infer_1.0.9
, dials_1.4.1
, scales_1.4.0
, tidymodels_1.3.0
, conflicted_1.2.0
, tibble_3.2.1
, yardstick_1.3.2
, fstcore_0.9.14
, SHAPforxgboost_0.1.3
, r2d3_0.2.6
, iBreakDown_2.1.2
, DALEX_2.4.3
, InvariantCausalPrediction_0.8
, mboost_2.9-10
, stabs_0.6-4
, glmnet_4.1-7
, Matrix_1.5-4
, xgboost_1.7.7.1
, caret_6.0-94
, lattice_0.21-8
, epitools_0.5-10.1
, mgcv_1.8-42
, nlme_3.1-162
, pROC_1.18.0
, tableone_0.13.2
, doParallel_1.0.17
, iterators_1.0.14
, foreach_1.5.2
, tictoc_1.1
, survival_3.5-5
, fst_0.9.8
, lubridate_1.9.2
, car_3.1-2
, carData_3.0-5
, psych_2.3.3
, broom_1.0.9
, ggplot2_3.5.2
, stringr_1.5.0
, dplyr_1.1.4
, data.table_1.14.8

## Installation Guide and demo
Please install R version 4.2.3 from the [CRAN website](https://cran.r-project.org/bin/windows/base/old/4.2.3/).  

The demo dataset was designed to mimic the preprocessed data used in the actual analysis, representing the data after scaling and other preprocessing steps were completed but before model training.

Then, run the following script in R to execute the main programs for developing and testing the prediction models described in the paper:

```r
#------　Merge demo datasets for the training datasets
library(fst)
library(dplyr)

# 
files <- list.files("./input/split_fst", pattern = "data_train_.*\\.fst$", full.names = TRUE)
files <- sort(files)  
#
merged_data <- lapply(files, read_fst) %>% bind_rows()
merged_data$Time <- as.Date(merged_data$Time)
#
write_fst(merged_data, "./input/data_train_for_prediction_model_scaled.fst")


#------　Merge demo datasets for the testing dataset
files <- list.files("./input/split_fst", pattern = "data_test_.*\\.fst$", full.names = TRUE)
files <- sort(files) 
#
merged_data <- lapply(files, read_fst) %>% bind_rows()
merged_data$Time <- as.Date(merged_data$Time) 
#
write_fst(merged_data, "./input/data_test_for_prediction_model_scaled.fst")

#------　Run the programs to develop and assess prediction models
source("./R_master_program.r")
```

