#--- ICP
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_002_ICP.r"
source(program_name_now)


#--- Make prediction models
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_003_make_bam.r"
source(program_name_now)

#
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_004_make_catboost_randomforest.r"
source(program_name_now)

#
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_005_make_xgboost.r"
source(program_name_now)

#
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_006_make_xgboost_ICP.r"
source(program_name_now)

#
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_007_make_xgboost_ICP_x_day_ahed.r"
source(program_name_now)




#--- Assess prediction models
rm(list=ls(all=TRUE))
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_008_assess_all_model_performance.r"
TEST_DATA_TYPE <- "External_place" # "External_place" or "Internal_place"
source(program_name_now)


#
rm(list=ls(all=TRUE))
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_008_assess_all_model_performance.r"
TEST_DATA_TYPE <- "Internal_place" # "External_place" or "Internal_place"
source(program_name_now)



#
rm(list=ls(all=TRUE))
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_009_assess_all_model_performance_day_ahead.r"
TEST_DATA_TYPE <- "External_place" # "External_place" or "Internal_place"
source(program_name_now)


#
rm(list=ls(all=TRUE))
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_009_assess_all_model_performance_day_ahead.r"
TEST_DATA_TYPE <- "Internal_place" # "External_place" or "Internal_place"
source(program_name_now)



#
rm(list=ls(all=TRUE))
setwd("/path/to/your/home/directory")
program_name_now <- "./programs/R_010_SHAP_xgboost_icp.r"
TEST_DATA_TYPE <- "Internal_place" # "External_place" or "Internal_place"
source(program_name_now)


