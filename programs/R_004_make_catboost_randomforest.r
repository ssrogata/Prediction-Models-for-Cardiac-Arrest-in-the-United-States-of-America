#-----------------------------------
# Make catboost and random forest
#-----------------------------------
rm(list=ls(all=TRUE))
source("./programs/R_001_enviroment.r")
source("./programs/R_001_enviroment_only_function.r")
library(tidymodels)
library(bonsai) 
library(doFuture)

#----------------  Formula
OUTCOME_NAME
Features
features_str <- paste(c(Features, "TotalPopulation_log_1e5_imp"), collapse = " + ")
Formula <- as.formula(paste("Outcome ~", features_str))
Formula

#------------ Read data
Data_train <- read_fst("./input/data_train_for_prediction_model_scaled.fst", as.data.table = TRUE)
setnames(Data_train, OUTCOME_NAME, "Outcome")
Data_train[, Outcome := as.numeric(Outcome), ]

# Organize validation datasets
Data_train[, valset := 
	ifelse(n_val_imp <= 20, 0, 
	ifelse(n_val_imp <= 40, 1, 
	ifelse(n_val_imp <= 60, 2, 
	ifelse(n_val_imp <= 80, 3, 
	4)))), ]

# Change data.table to data.frame
Data_train <- data.frame(Data_train)










##------------##------------##------------
##------------ Train model by catboost/bonsai with parsnip
##------------##------------##------------
# PARALLEL
options(future.globals.maxSize = 450 * 1024^3) 
plan(multisession, workers = max(1, parallel::detectCores() - 2))
registerDoFuture()
set.seed(12345)
print("Do parallel")


# CatBoost by parsnip
cat_spec <- boost_tree(
  trees      = 1000,
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_engine(
    "catboost",
    l2_leaf_reg = tune(),
    subsample   = tune(),
    od_type     = "Iter",      
    od_wait     = 50          
  ) %>%
  set_mode("regression")
  
wf <- workflow() %>% add_model(cat_spec) %>% add_formula(Formula)


# Metric = RMSE
mets <- metric_set(rmse)

## 5-fold CV by using valset as fold ID
cv_manual <- group_vfold_cv(
  Data_train,
  group = valset,
  v = length(unique(Data_train$valset))
)

# Grid search
grid_search <- expand.grid(
  tree_depth  = c(4L, 8L, 12L),
  learn_rate  = c(0.01, 0.05, 0.1),
  l2_leaf_reg = c(1, 5, 10),
  subsample   = c(0.7, 1.0)
)
grid_search


## Train the model
tic("tune catboost")
tuned <- tune_grid(
  wf, resamples = cv_manual, grid = grid_search, metrics = mets,
  control = control_grid(save_pred = FALSE, parallel_over = "resamples", verbose = TRUE)
)
toc()

best <- select_best(tuned, metric = "rmse")
final_wf <- finalize_workflow(wf, best)

# Re-training the model using the best parameter set
final_fit <- fit(final_wf, data = Data_train)
saveRDS(final_fit, "./models/catboost_best.rds")








##------------##------------##------------
##------------ Train model by random forest/ranger with parsnip
##------------##------------##------------
plan(sequential)
registerDoSEQ()
set.seed(12345)

## Random Forest by parsnip (ranger)
rf_spec <- rand_forest(
  mtry  = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_engine(
    "ranger",
    importance  = "impurity",
    splitrule   = tune(),  
    num.threads = parallel::detectCores() - 2
  ) %>%
  set_mode("regression")

wf <- workflow() %>% add_model(rf_spec) %>% add_formula(Formula)

## Metric = RMSE
mets <- metric_set(rmse)

## 5-fold CV by using valset as fold ID
cv_manual <- group_vfold_cv(
  Data_train,
  group = valset,
  v = length(unique(Data_train$valset))
)

##
p <- length(c(Features, "TotalPopulation_log_1e5_imp"))

mtry_candidates <- unique(pmax(1L, pmin(p, c(
  1L,
  floor(sqrt(p)),
  floor(p/4), floor(p/3), floor(p/2),
  floor(p*0.8),
  p
))))

grid_search <- expand.grid(
  mtry      = mtry_candidates,
  trees     = c(500L, 1000L),        
  min_n     = c(2L, 5L, 10L, 20L),
  splitrule = c("variance", "extratrees") 
)
grid_search


## Train the model
tic("tune ranger (random forest)")
tuned <- tune_grid(
  wf,
  resamples = cv_manual,
  grid = grid_search,
  metrics = mets,
  control = control_grid(save_pred = FALSE, parallel_over = "everything", verbose = TRUE)
)
toc()

best <- select_best(tuned, metric = "rmse")
final_wf <- finalize_workflow(wf, best)

## Re-training the model using the best parameter set
final_fit <- fit(final_wf, data = Data_train)
saveRDS(final_fit, "./models/rf_best.rds")

