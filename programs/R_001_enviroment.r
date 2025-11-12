#------------------#------------------#------------------
#------------------ Settings
#------------------#------------------#------------------
#------------------
# Packages
#------------------
list_of_packages <- c(
	"data.table"
	, "dplyr"
	, "stringr"
	, "ggplot2"
	, "broom"
	, "psych"
	, "car"
	, "lubridate"
	, "fst"
	, "survival"
	, "tictoc"
	, "doParallel"
	, "tableone"
	
	, "pROC"
	, "mgcv"
	, "epitools"
	, "caret"
	, "xgboost"
	, "InvariantCausalPrediction"
	, "DALEX"
	, "iBreakDown"
	, "r2d3"
	, "SHAPforxgboost"
	, "Matrix"
	)

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages))install.packages(new_packages)

for(iii in list_of_packages){
	library(iii, character.only = T)
	}



#------------------
## directory
#------------------
list.files()
if(dir.exists(paste0(getwd(), "/input")) == FALSE) {dir.create(paste0(getwd(), "/input"))}
if(dir.exists(paste0(getwd(), "/output")) == FALSE) {dir.create(paste0(getwd(), "/output"))}
if(dir.exists(paste0(getwd(), "/output/tmp")) == FALSE) {dir.create(paste0(getwd(), "/output/tmp"))}
if(dir.exists(paste0(getwd(), "/models")) == FALSE) {dir.create(paste0(getwd(), "/models"))}
if(dir.exists(paste0(getwd(), "/programs")) == FALSE) {dir.create(paste0(getwd(), "/programs"))}



#-------------------------
## dataset and id position
#-------------------------
ID_NAME_IN_DATA <- "EMS_AgencyID"
ID_POSITION_IN_COL <- 1


# slide days
TIME_FRAME <- c(3, 7)
Year_test_start <- 2018



#-------------------------
# Specify outcome and features
#-------------------------
OUTCOME_NAME <- "Cases"
Features <- c(
	##
	  "Meanprecip_imp"
	, "Meantemp_imp" 
	, "Meanrh_imp"               
	, "Meanwind_imp"      
	, "Dif_temp_imp"      
	, "Dif_precip_imp"      
	, "Dif_wind_imp"      
	, "Dif_rh_imp"      

	##
	, "Medianage_ordinal_imp"
	, "PercentMale_ordinal_imp"
	, "Black_prop_ordinal_imp"   
	, "Asian_prop_ordinal_imp"
	, "HighSchoolDiplomaorHigher_prop_ordinal_imp"
	, "Percentbelowpovertylevel_ordinal_imp"	
	
	##
	, "Year_imp"
	
	, "Jan_imp"
	, "Feb_imp"
	, "Mar_imp"
	, "Apr_imp"
	, "May_imp"
	, "Jun_imp"
	, "Jly_imp"
	, "Aug_imp"
	, "Sep_imp"
	, "Oct_imp"
	, "Nov_imp"
	, "Dec_imp"
	
	##
	, "Mon_imp"
	, "Tue_imp"
	, "Wed_imp"
	, "Thu_imp"
	, "Fri_imp"
	, "Sat_imp"
	, "Sun_imp"
	)
Features <- unique(Features)


#------------------
# selected predicted values
#------------------
pred_cols <- c(
  "Pred_bam_ref", 
  "Pred_rf", 
  "Pred_catboost", 
  "Pred_xgboost", 
  "Pred_xgboost_ICP_pop_pov_edu"
  )
  
pred_cols_clbplot <- c("Pred_xgboost", "Pred_xgboost_ICP_pop_pov_edu")

pred_cols_slide <- c("Pred_xgboost_ICP_pop_pov_edu")


#------------------
# seed & parallel
#------------------
PARALLEL <- FALSE

if(PARALLEL == TRUE){
	library(doParallel)
	NCL <- detectCores(all.tests = FALSE, logical = TRUE) - 1
	cl <- makeCluster(NCL)
	registerDoParallel(cl)
	set.seed(12345, kind = "L'Ecuyer-CMRG")
	print("seed and parallel were set")
	} else {
	set.seed(1234)
	print("seed was set, but not paralleled")
	}




