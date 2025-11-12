#-----------------------------------
# Make xgboost model with features selected by ICP
#-----------------------------------
rm(list=ls(all=TRUE))
source("./programs/R_001_enviroment.r")
source("./programs/R_001_enviroment_only_function.r")
library(conflicted)
conflicts_prefer(data.table::first)


#-----------------------------------
# Prediction settings
#-----------------------------------
# Metric
My_metric <- "rmse" #Kappa" #" #Accuracy RMSE

# GPU
GPU <- FALSE

# PARALLEL
PARALLEL <- TRUE

if(PARALLEL == TRUE){
	library(doParallel)
	NCL <- detectCores(all.tests = FALSE, logical = TRUE) - 2
	cl <- makeCluster(NCL)
	registerDoParallel(cl)
	set.seed(2345, kind = "L'Ecuyer-CMRG")
	print("seed and parallel were set")
} else {
	set.seed(234)
	print("seed was set, but not paralleled")
}



##------------ Read data
Data_train <- read_fst("./input/data_train_for_prediction_model_scaled.fst", as.data.table = TRUE)
Data_train[, TotalPopulation_log_1e5_xgboost := log(TotalPopulation / 1e5), ]
setnames(Data_train, OUTCOME_NAME, "Outcome")
dim(Data_train)
Data_train[, table(Outcome)]


## CV by manual
VAL_ALL <- TRUE #VALが一つ=FALSE,or複数=TRUE
Data_train[, valset := 
	ifelse(n_val_imp <= 20, 0, 
	ifelse(n_val_imp <= 40, 1,
	ifelse(n_val_imp <= 60, 2,
	ifelse(n_val_imp <= 80, 3,
	4)))), ]
	Data_train[, table(valset), ]
	
	

#----------------  formula
# Updating "Features" object based on the results of ICP
Features <- c(
	"Meanprecip_imp" 
	, "Meantemp_imp"   
	, "Meanrh_imp"     
	, "Meanwind_imp"   
	, "Dif_temp_imp"   
	, "Dif_precip_imp" 
	, "Dif_wind_imp"   
	, "Dif_rh_imp"     
	, "Year_imp"
	, "Jan_imp"
	, "Feb_imp"
	, "Medianage_ordinal_imp"                        
	, "PercentMale_ordinal_imp"                      
	, "Black_prop_ordinal_imp"                       
	, "Asian_prop_ordinal_imp"                       
	, "HighSchoolDiplomaorHigher_prop_ordinal_imp"   
	, "Percentbelowpovertylevel_ordinal_imp"                   
)





##------------##------------##------------
## xgboost
##------------##------------##------------
### Train
Data_train_dm <- xgb.DMatrix(
	data = Data_train[, Features, with = FALSE] %>% as.matrix(),
	label= Data_train[, Outcome, ]
	)
setinfo(Data_train_dm, "base_margin", Data_train[, TotalPopulation_log_1e5_xgboost, ])
Data_train_dm

 




##--------------##-------------##----------------
## Parameter tuning
##--------------##-------------##----------------
#----------------#----------------#----------------
#---------------- 1 round
#----------------#----------------#----------------
Tune_params <- expand.grid(
	# 1
	max_depth = seq(3, 9, 2),
	min_child_weight = seq(1, 5, 1),
	#2
	gamma = 0,
	#3
	colsample_bytree = 0.8,
	subsample = 0.8,
	#4
	alpha = 0,
	lambda = 1,
	#5
	eta = 0.1,
	# 6
	iii_val = if(VAL_ALL == TRUE){0:max(unique(Data_train$valset))}else{unique(Data_train$valset) %>% max(.)}
	) %>% as.data.table(.)
Tune_params[, NO := 1:.N, ]








###------- Repeat from here
Hoge_parameters <- c()
tic()

for(iii in 1:nrow(Tune_params)){

	### Train data
	Data_train_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
		)
	setinfo(Data_train_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, TotalPopulation_log_1e5_xgboost, ])
	Data_train_dm
	
	
	### Validation data
	Data_val_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
		)
	setinfo(Data_val_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, TotalPopulation_log_1e5_xgboost, ])
	Data_val_dm

	
	# Select parameters
	param <- list(tree_method = if(GPU == TRUE){"gpu_hist"}else{"hist"},
		# 1
		max_depth = Tune_params[iii, max_depth],
		min_child_weight = Tune_params[iii, min_child_weight],
		#2
		gamma = Tune_params[iii, gamma],
		#3
		colsample_bytree = Tune_params[iii, colsample_bytree],
		subsample = Tune_params[iii, subsample],
		#4
		alpha = Tune_params[iii, alpha],
		lambda = Tune_params[iii, lambda],
		#5
		eta = Tune_params[iii, eta]
		)
	
	
	# Models
	Model <- xgb.train(
		# Structure
		objective = "count:poisson",
		booster="gbtree",
		data = Data_train_dm, 
		
		# Parameters
		nrounds = 3000,
		early_stopping_rounds = 10 / Tune_params[iii, eta],
		params = param, 
	
		# Display and evaluation
		verbose = FALSE,
		eval_metric = My_metric,
		watchlist = list(Validation = Data_val_dm)
		)
		
	
	# Save performance
	Temp <- data.table(
		NO = Tune_params[iii, NO],
		
		VALSET = Tune_params[iii, iii_val],
	
		Nround = Model$best_iteration,
	
		RMSE_train = RMSE(
			Pred = predict(Model, newdata = Data_train_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
			),
	
		RMSE_val = RMSE(
			Pred = predict(Model, newdata = Data_val_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
			)
		)
		
	
	Hoge_parameters <- rbind(Hoge_parameters, Temp)	
}

toc() 


Hoge_parameters <- data.table(Hoge_parameters)
setorder(Hoge_parameters, RMSE_val) 
Hoge_parameters

### Save results
fwrite(Hoge_parameters, paste0("./output/tmp/Table_xgboost_ICP_pop_pov_edu_params_round_r1.csv"))
Next_parameters <- Tune_params[NO == Hoge_parameters[RMSE_val == min(RMSE_val), first(NO), ], , ]
Next_parameters









#----------------#----------------#----------------
#---------------- 2 round
#----------------#----------------#----------------
Tune_params <- expand.grid(
	# 1
	max_depth = Next_parameters[, max_depth, ],
	min_child_weight = Next_parameters[, min_child_weight, ],
	#2
	gamma = seq(0, 0.01, 0.001),
	#3
	colsample_bytree = Next_parameters[, colsample_bytree, ],
	subsample = Next_parameters[, subsample, ],
	#4
	alpha = Next_parameters[, alpha, ],
	lambda = Next_parameters[, lambda, ],
	#5
	eta = Next_parameters[, eta, ],
	# 6
	iii_val = if(VAL_ALL == TRUE){0:max(unique(Data_train$valset))}else{unique(Data_train$valset) %>% max(.)}
	) %>% as.data.table(.)
Tune_params[, NO := 1:.N, ]





###------- Repeat from here
Hoge_parameters <- c()
tic()

for(iii in 1:nrow(Tune_params)){
	
	### Train data
	Data_train_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
		)
	setinfo(Data_train_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, TotalPopulation_log_1e5_xgboost, ])
	Data_train_dm
	
	
	### Validation data
	Data_val_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
		)
	setinfo(Data_val_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, TotalPopulation_log_1e5_xgboost, ])
	Data_val_dm
		
	
	# Select parameters
	param <- list(tree_method = if(GPU == TRUE){"gpu_hist"}else{"hist"},
		# 1
		max_depth = Tune_params[iii, max_depth],
		min_child_weight = Tune_params[iii, min_child_weight],
		#2
		gamma = Tune_params[iii, gamma],
		#3
		colsample_bytree = Tune_params[iii, colsample_bytree],
		subsample = Tune_params[iii, subsample],
		#4
		alpha = Tune_params[iii, alpha],
		lambda = Tune_params[iii, lambda],
		#5
		eta = Tune_params[iii, eta]
		)
	
	
	# Models
	Model <- xgb.train(
		# Structure
		objective = "count:poisson",
		booster="gbtree",
		data = Data_train_dm, 
		
		# Parameters
		nrounds = 3000,
		early_stopping_rounds = 10 / Tune_params[iii, eta],
		params = param, 
	
		# Display and evaluation
		verbose = FALSE,
		eval_metric = My_metric,
		watchlist = list(Validation = Data_val_dm)
		)
		
	
	# Save performance
	Temp <- data.table(
		NO = Tune_params[iii, NO],
		
		VALSET = Tune_params[iii, iii_val],
	
		Nround = Model$best_iteration,
	
		RMSE_train = RMSE(
			Pred = predict(Model, newdata = Data_train_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
			),
	
		RMSE_val = RMSE(
			Pred = predict(Model, newdata = Data_val_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
			)
		)
		
	
	Hoge_parameters <- rbind(Hoge_parameters, Temp)

}
toc() 

Hoge_parameters <- data.table(Hoge_parameters)
setorder(Hoge_parameters, RMSE_val) 
Hoge_parameters

### Save results
fwrite(Hoge_parameters, paste0("./output/tmp/Table_xgboost_ICP_pop_pov_edu_params_round_r2.csv"))
Next_parameters <- Tune_params[NO == Hoge_parameters[RMSE_val == min(RMSE_val), first(NO), ], , ]
Next_parameters










#----------------#----------------#----------------
#---------------- 3 round
#----------------#----------------#----------------
Tune_params <- expand.grid(
	# 1
	max_depth = Next_parameters[, max_depth, ],
	min_child_weight = Next_parameters[, min_child_weight, ],
	#2
	gamma = Next_parameters[, gamma, ],
	#3
	colsample_bytree = seq(0.6, 1, by = 0.1),
	subsample = seq(0.6, 1, by = 0.1),
	#4
	alpha = Next_parameters[, alpha, ],
	lambda = Next_parameters[, lambda, ],
	#5
	eta = Next_parameters[, eta, ],
	# 6
	iii_val = if(VAL_ALL == TRUE){0:max(unique(Data_train$valset))}else{unique(Data_train$valset) %>% max(.)}
	) %>% as.data.table(.)
Tune_params[, NO := 1:.N, ]





###------- Repeat from here
Hoge_parameters <- c()
tic()

for(iii in 1:nrow(Tune_params)){
	
	### Train data
	Data_train_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
		)
	setinfo(Data_train_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, TotalPopulation_log_1e5_xgboost, ])
	Data_train_dm
	
	
	
	### Validation data
	Data_val_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
		)
	setinfo(Data_val_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, TotalPopulation_log_1e5_xgboost, ])
	Data_val_dm
	
	
	
	
	# Select parameters
	param <- list(tree_method = if(GPU == TRUE){"gpu_hist"}else{"hist"},
		# 1
		max_depth = Tune_params[iii, max_depth],
		min_child_weight = Tune_params[iii, min_child_weight],
		#2
		gamma = Tune_params[iii, gamma],
		#3
		colsample_bytree = Tune_params[iii, colsample_bytree],
		subsample = Tune_params[iii, subsample],
		#4
		alpha = Tune_params[iii, alpha],
		lambda = Tune_params[iii, lambda],
		#5
		eta = Tune_params[iii, eta]
		)
	
	
	# Models
	Model <- xgb.train(
		# Structure
		objective = "count:poisson",
		booster="gbtree",
		data = Data_train_dm, 
		
		# Parameters
		nrounds = 3000,
		early_stopping_rounds = 10 / Tune_params[iii, eta],
		params = param, 
	
		# Display and evaluation
		verbose = FALSE,
		eval_metric = My_metric,
		watchlist = list(Validation = Data_val_dm)
		)
	
	
	
	# Save performance
	Temp <- data.table(
		NO = Tune_params[iii, NO],
	
		VALSET = Tune_params[iii, iii_val],
	
		Nround = Model$best_iteration,
	
		RMSE_train = RMSE(
			Pred = predict(Model, newdata = Data_train_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
			),
	
		RMSE_val = RMSE(
			Pred = predict(Model, newdata = Data_val_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
			)
		)
		
	
	Hoge_parameters <- rbind(Hoge_parameters, Temp)

}
toc()

Hoge_parameters <- data.table(Hoge_parameters)
setorder(Hoge_parameters, RMSE_val) 
Hoge_parameters

### Save results
fwrite(Hoge_parameters, paste0("./output/tmp/Table_xgboost_ICP_pop_pov_edu_params_round_r3.csv"))
Next_parameters <- Tune_params[NO == Hoge_parameters[RMSE_val == min(RMSE_val), first(NO), ], , ]
Next_parameters







#----------------#----------------#----------------
#---------------- 4 round
#----------------#----------------#----------------
Tune_params <- expand.grid(
	# 1
	max_depth = Next_parameters[, max_depth, ],
	min_child_weight = Next_parameters[, min_child_weight, ],
	#2
	gamma = Next_parameters[, gamma, ],
	#3
	colsample_bytree = Next_parameters[, colsample_bytree, ],
	subsample = Next_parameters[, subsample, ],
	#4
	alpha = c(1e-5, 1e-2, 0, 0.1, 1, 100),
	lambda = c(1e-5, 1e-2, 0, 0.1, 1, 100),
	#5
	eta = Next_parameters[, eta, ],
	# 6
	iii_val = if(VAL_ALL == TRUE){0:max(unique(Data_train$valset))}else{unique(Data_train$valset) %>% max(.)}
	) %>% as.data.table(.)
Tune_params[, NO := 1:.N, ]





###------- Repeat from here
Hoge_parameters <- c()
tic()

for(iii in 1:nrow(Tune_params)){
	
	### Train data
	Data_train_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
		)
	setinfo(Data_train_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, TotalPopulation_log_1e5_xgboost, ])
	Data_train_dm

	
	### Validation data
	Data_val_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
		)
	setinfo(Data_val_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, TotalPopulation_log_1e5_xgboost, ])
	Data_val_dm

	
	# Select parameters
	param <- list(tree_method = if(GPU == TRUE){"gpu_hist"}else{"hist"},
		# 1
		max_depth = Tune_params[iii, max_depth],
		min_child_weight = Tune_params[iii, min_child_weight],
		#2
		gamma = Tune_params[iii, gamma],
		#3
		colsample_bytree = Tune_params[iii, colsample_bytree],
		subsample = Tune_params[iii, subsample],
		#4
		alpha = Tune_params[iii, alpha],
		lambda = Tune_params[iii, lambda],
		#5
		eta = Tune_params[iii, eta]
		)
	
	
	# Models
	Model <- xgb.train(
		# Structure
		objective = "count:poisson",
		booster="gbtree",
		data = Data_train_dm, 
		
		# Parameters
		nrounds = 3000,
		early_stopping_rounds = 10 / Tune_params[iii, eta],
		params = param, 
	
		# Display and evaluation
		verbose = FALSE,
		eval_metric = My_metric,
		watchlist = list(Validation = Data_val_dm)
		)
	
	
	# Save performance
	Temp <- data.table(
		NO = Tune_params[iii, NO],
	
		VALSET = Tune_params[iii, iii_val],
	
		Nround = Model$best_iteration,
	
		RMSE_train = RMSE(
			Pred = predict(Model, newdata = Data_train_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
			),
	
		RMSE_val = RMSE(
			Pred = predict(Model, newdata = Data_val_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
			)
		)
		
	
	Hoge_parameters <- rbind(Hoge_parameters, Temp)
	
}
toc()

Hoge_parameters <- data.table(Hoge_parameters)
setorder(Hoge_parameters, RMSE_val) 
Hoge_parameters

### Save results
fwrite(Hoge_parameters, paste0("./output/tmp/Table_xgboost_ICP_pop_pov_edu_params_round_r4.csv"))
Next_parameters <- Tune_params[NO == Hoge_parameters[RMSE_val == min(RMSE_val), first(NO), ], , ]
Next_parameters




#----------------#----------------#----------------
#---------------- 5 round
#----------------#----------------#----------------
Tune_params <- expand.grid(
	# 1
	max_depth = Next_parameters[, max_depth, ],
	min_child_weight = Next_parameters[, min_child_weight, ],
	#2
	gamma = Next_parameters[, gamma, ],
	#3
	colsample_bytree = Next_parameters[, colsample_bytree, ],
	subsample = Next_parameters[, subsample, ],
	#4
	alpha = Next_parameters[, alpha, ],
	lambda = Next_parameters[, lambda, ],
	#5
	eta = seq(0.01, 0.1, by = 0.01),
	# 6
	iii_val = if(VAL_ALL == TRUE){0:max(unique(Data_train$valset))}else{unique(Data_train$valset) %>% max(.)}
	) %>% as.data.table(.)
Tune_params[, NO := 1:.N, ]










###------- Repeat from here
Hoge_parameters <- c()
tic()

for(iii in 1:nrow(Tune_params)){
	
	### Train data
	Data_train_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
		)
	setinfo(Data_train_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, TotalPopulation_log_1e5_xgboost, ])
	Data_train_dm
	
	
	### Validation data
	Data_val_dm <- xgb.DMatrix(
		data = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Features, with = FALSE] %>% as.matrix(),
		label= Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
		)
	setinfo(Data_val_dm, "base_margin", Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, TotalPopulation_log_1e5_xgboost, ])
	Data_val_dm
	
	
	# Select parameters
	param <- list(tree_method = if(GPU == TRUE){"gpu_hist"}else{"hist"},
		# 1
		max_depth = Tune_params[iii, max_depth],
		min_child_weight = Tune_params[iii, min_child_weight],
		#2
		gamma = Tune_params[iii, gamma],
		#3
		colsample_bytree = Tune_params[iii, colsample_bytree],
		subsample = Tune_params[iii, subsample],
		#4
		alpha = Tune_params[iii, alpha],
		lambda = Tune_params[iii, lambda],
		#5
		eta = Tune_params[iii, eta]
		)
	
	
	# Models
	Model <- xgb.train(
		# Structure
		objective = "count:poisson",
		booster="gbtree",
		data = Data_train_dm, 
		
		# Parameters
		nrounds = 3000,
		early_stopping_rounds = 10 / Tune_params[iii, eta],
		params = param, 
	
		# Display and evaluation
		verbose = FALSE,
		eval_metric = My_metric,
		watchlist = list(Validation = Data_val_dm)
		)
	
	
	# Save performance
	Temp <- data.table(
		NO = Tune_params[iii, NO],
	
		VALSET = Tune_params[iii, iii_val],
	
		Nround = Model$best_iteration,
	
		RMSE_train = RMSE(
			Pred = predict(Model, newdata = Data_train_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == FALSE, Outcome, ]
			),
	
		RMSE_val = RMSE(
			Pred = predict(Model, newdata = Data_val_dm), 
			Obs = Data_train[(valset %in% Tune_params[iii, iii_val]) == TRUE, Outcome, ]
			)
		)
	
	Hoge_parameters <- rbind(Hoge_parameters, Temp)

}

toc()

Hoge_parameters <- data.table(Hoge_parameters)
setorder(Hoge_parameters, RMSE_val) 
Hoge_parameters

### Save results
fwrite(Hoge_parameters, paste0("./output/tmp/Table_xgboost_ICP_pop_pov_edu_params_round_r5.csv"))
Next_parameters <- Tune_params[NO == Hoge_parameters[RMSE_val == min(RMSE_val), first(NO), ], , ]
Next_parameters


Next_parameters[, Nround := Hoge_parameters[RMSE_val == min(RMSE_val), Nround, ], ]
fwrite(Next_parameters, paste0("./output/Table_xgboost_ICP_pop_pov_edu_params_round_best.csv"))







#----------------#----------------#----------------
#---------------- Re-develop the best model
#----------------#----------------#----------------
Best_list <- fread(paste0("./output/Table_xgboost_ICP_pop_pov_edu_params_round_best.csv"))
Best_list


# Set parameters
param <- list(tree_method = if(GPU == TRUE){"gpu_hist"}else{"hist"},
	# 1
	max_depth = Best_list[, max_depth, ],
	min_child_weight = Best_list[, min_child_weight, ],
	#2
	gamma = Best_list[, gamma, ],
	#3
	colsample_bytree = Best_list[, colsample_bytree, ],
	subsample = Best_list[, subsample, ],
	#4
	alpha = Best_list[, alpha, ],
	lambda = Best_list[, lambda, ],
	#5
	eta = Best_list[, eta, ]
	)





### Train
Data_train_dm <- xgb.DMatrix(
	data = Data_train[(valset %in% Best_list[, iii_val]) == FALSE, Features, with = FALSE] %>% as.matrix(),
	label= Data_train[(valset %in% Best_list[, iii_val]) == FALSE, Outcome, ]
	)
setinfo(Data_train_dm, "base_margin", Data_train[(valset %in% Best_list[, iii_val]) == FALSE, TotalPopulation_log_1e5_xgboost, ])
Data_train_dm



### Validation
Data_val_dm <- xgb.DMatrix(
	data = Data_train[(valset %in% Best_list[, iii_val]) == TRUE, Features, with = FALSE] %>% as.matrix(),
	label= Data_train[(valset %in% Best_list[, iii_val]) == TRUE, Outcome, ]
	)
setinfo(Data_val_dm, "base_margin", Data_train[(valset %in% Best_list[, iii_val]) == TRUE, TotalPopulation_log_1e5_xgboost, ])
Data_val_dm



tic()
# Models
Model <- xgb.train(
	# Structure
	objective = "count:poisson",
	booster="gbtree",
	data = Data_train_dm, 
	
	# Parameters
	nrounds = 3000,
	early_stopping_rounds = 10 / Best_list[, eta, ],
	params = param, 

	# Display and evaluation
	verbose = FALSE,
	eval_metric = My_metric,
	watchlist = list(Validation = Data_val_dm)
	)
saveRDS(Model, paste0("./models/XGBoost_ICP_pop_pov_edu_best.rds")) 
toc()

Model$best_iteration



stopCluster(cl = cl)


importance <- xgb.importance(model = Model)
importance
fwrite(importance, paste0("./output/xgboost_ICP_pop_pov_edu_feature_importance_.csv"))

