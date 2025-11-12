#-----------------------------------
# Make SHAP value
#-----------------------------------
rm(list=ls(all=TRUE))
source("./programs/R_001_enviroment.r")
source("./programs/R_001_enviroment_only_function.r")
set.seed(1)
SHAP_SAMPLE_SIZE <- 100e+3
TARGET_SEED <- 12280
library(conflicted)
conflicts_prefer(data.table::`:=`)



#---------------- #---------------- #---------------- 
#---------------- Input data
#---------------- #---------------- #---------------- 

##------------ Read data
Data_train <- read_fst("./input/data_train_for_prediction_model_scaled.fst", as.data.table = TRUE)


###########################
###########################
#--------------------------------
# Step 0: Get models
#--------------------------------
Model <- readRDS("./models/XGBoost_ICP_pop_pov_edu_best.rds")
Select_predictor <- Model$feature_names
Select_predictor


#--------------------------------
# Select some observations
#--------------------------------
set.seed(TARGET_SEED)
Select_row <- sample(1:nrow(Data_train), size = SHAP_SAMPLE_SIZE)
X <- data.matrix(Data_train[Select_row, Select_predictor, with = FALSE])



#--------------------------------
# Crunch SHAP values
#--------------------------------
shap <- shap.prep(Model, X_train = X)



#--------------------------------
# SHAP importance
#--------------------------------
shap.plot.summary.edited(shap)
ggsave("./output/SHAP_xgboost_icp_pop_pov_edu_all.svg", width = 7, height = 14, device = svg, dpi = 300)
ggsave("./output/SHAP_xgboost_icp_pop_pov_edu_all.png", width = 7, height = 14, device = png, dpi = 300)

