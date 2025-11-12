#-----------------------------------
# ICP 
#-----------------------------------
rm(list=ls(all=TRUE))

#----------------
# Environment
#----------------
Features_ICP_w <- c(
	#### weather
	  "Meanprecip_imp"
	, "Meantemp_imp" 
	, "Meanrh_imp"               
	, "Meanwind_imp"      
	, "Dif_temp_imp"      
	, "Dif_precip_imp"      
	, "Dif_wind_imp"      
	, "Dif_rh_imp"      
	)
	
Features_ICP_c <- c(
	"Year_imp"
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
	, "Holiday_all_imp"
	)
	
Features_ICP_g <- c(
	"TotalPopulation_log_1e5_imp"
	, "Medianage_ordinal_imp"
	, "PercentMale_ordinal_imp"
	, "Black_prop_ordinal_imp"   
	, "Asian_prop_ordinal_imp"
	, "HighSchoolDiplomaorHigher_prop_ordinal_imp"
	, "Percentbelowpovertylevel_ordinal_imp"
	)

Feature_sets <- list(
  Features_ICP_weather = Features_ICP_w,
  Features_ICP_chrono = Features_ICP_c,
  Features_ICP_geo = Features_ICP_g
)


for (feature_set_name in names(Feature_sets)) {
	
	source("./programs/R_001_enviroment.r")
	source("./programs/R_001_enviroment_only_function.r")

	Features_ICP <- Feature_sets[[feature_set_name]]
	
	##------------ Read data
	Data_train <- read_fst("./input/data_train_for_prediction_model_scaled.fst", as.data.table = TRUE)
	setnames(Data_train, OUTCOME_NAME, "Outcome")
	
	# Enviroment variable
	Data_train[, temp := 
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.1), 1,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.2), 2,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.3), 3,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.4), 4,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.5), 5,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.6), 6,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.7), 7,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.8), 8,
		ifelse(TotalPopulation_log_1e5_imp <= quantile(TotalPopulation_log_1e5_imp, p = 0.9), 9, 
		10))))))))), ]
		
	Data_train[, temp2 := 
		ifelse(Percentbelowpovertylevel_ordinal_imp == unique(Data_train$Percentbelowpovertylevel_ordinal_imp)%>%sort(.)%>%.[1], "pov_low",
		ifelse(Percentbelowpovertylevel_ordinal_imp == unique(Data_train$Percentbelowpovertylevel_ordinal_imp)%>%sort(.)%>%.[2], "pov_mid",
		ifelse(Percentbelowpovertylevel_ordinal_imp == unique(Data_train$Percentbelowpovertylevel_ordinal_imp)%>%sort(.)%>%.[3], "pov_high",
		"other"))), ]
		
	Data_train[, temp3 := 
		ifelse(HighSchoolDiplomaorHigher_prop_ordinal_imp == unique(Data_train$HighSchoolDiplomaorHigher_prop_ordinal_imp)%>%sort(.)%>%.[1], "edu_low",
		ifelse(HighSchoolDiplomaorHigher_prop_ordinal_imp == unique(Data_train$HighSchoolDiplomaorHigher_prop_ordinal_imp)%>%sort(.)%>%.[2], "edu_mid",
		ifelse(HighSchoolDiplomaorHigher_prop_ordinal_imp == unique(Data_train$HighSchoolDiplomaorHigher_prop_ordinal_imp)%>%sort(.)%>%.[3], "edu_high",
		"other"))), ]
	
	Data_train[, ENVIRONMENT_VAR := interaction(temp, temp2, temp3, drop = TRUE)]
	
	ExpInd_use <- list()
	for(iii in 1:length(unique(Data_train$ENVIRONMENT_VAR))){
		ExpInd_use[[iii]] <- which(Data_train$ENVIRONMENT_VAR == unique(Data_train$ENVIRONMENT_VAR)[iii])
	}
	
	
	# Variables
	Y_use <- as.numeric(Data_train$Outcome) 
	X_use <- as.data.frame(Data_train[, Features_ICP, with = FALSE])
	
	# ICP
	icp_hidden <- hiddenICP(
		X = as.matrix(X_use),
		Y = Y_use,
		ExpInd = ExpInd_use,
		alpha = 0.1, 
		mode = "asymptotic", 
		intercept = FALSE
		)
	
	## 
	print(feature_set_name)
	print(icp_hidden)
	summary(icp_hidden)
}


  