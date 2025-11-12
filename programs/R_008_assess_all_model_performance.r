#-----------------------------------
# Assessing prediction Performance
#-----------------------------------
source("./programs/R_001_enviroment.r")
source("./programs/R_001_enviroment_only_function.r")

# package for this program
library(ggsci)
library(rlang)
library(Hmisc)
library(tidymodels)  
library(bonsai)  
library(foreach)  
library(fixest)
library(conflicted)
conflicts_prefer(data.table::":=")



##------------ Read data
Data_train <- read_fst("./input/data_train_for_prediction_model_scaled.fst", as.data.table = TRUE)
Data_train[, TotalPopulation_log_1e5_xgboost := log(TotalPopulation / 1e5), ]


#---------------- Prediction settings
Data_test <- read_fst("./input/data_test_for_prediction_model_scaled.fst", as.data.table = TRUE)
Data_test[, TotalPopulation_log_1e5_xgboost := log(TotalPopulation / 1e5), ]


if(TEST_DATA_TYPE == "External_place"){
	Data_test <- Data_test[Year_original >= Year_test_start & externalEMS_original == "external", , ]
	SAVING_DIRECTORY <- "./output_external_place"
}

if(TEST_DATA_TYPE == "Internal_place"){
	Data_test <- Data_test[Year_original >= Year_test_start & externalEMS_original == "not_external", , ]
	SAVING_DIRECTORY <- "./output_internal_place"
}


setnames(Data_train, OUTCOME_NAME, "Outcome")
setnames(Data_test, OUTCOME_NAME, "Outcome")

if(dir.exists(paste0(SAVING_DIRECTORY)) == FALSE) {dir.create(SAVING_DIRECTORY)}
if(dir.exists(paste0(SAVING_DIRECTORY, "/tmp")) == FALSE) {dir.create(paste0(SAVING_DIRECTORY, "/tmp"))}







#-------------------------------
# Get prediction values
# xgboost with all variables
#-------------------------------
Model <- readRDS("./models/XGBoost_best.rds") 
Features <- Model$feature_names
Features

### Train
Data_train_dm <- xgb.DMatrix(
	data = Data_train[, Features, with = FALSE] %>% as.matrix(),
	label= Data_train[, Outcome, ]
	)
setinfo(Data_train_dm, "base_margin", Data_train[, TotalPopulation_log_1e5_xgboost, ])
Data_train_dm

### Testing
Data_test_dm <- xgb.DMatrix(
	data = Data_test[, Features, with = FALSE] %>% as.matrix(),
	label= Data_test[, Outcome, ]
	)
setinfo(Data_test_dm, "base_margin", Data_test[, TotalPopulation_log_1e5_xgboost, ])   
Data_test_dm

### Get prediction values
Data_train[, Pred_xgboost := predict(Model, newdata = Data_train_dm), ]
Data_test[,  Pred_xgboost := predict(Model, newdata = Data_test_dm), ]




#-------------------------------
# Get prediction values
# xgboost population-poverty-education-version ICP
#-------------------------------
Model <- readRDS("./models/XGBoost_ICP_pop_pov_edu_best.rds") 
Features <- Model$feature_names
Features

### Train
Data_train_dm <- xgb.DMatrix(
	data = Data_train[, Features, with = FALSE] %>% as.matrix(),
	label= Data_train[, Outcome, ]
	)
setinfo(Data_train_dm, "base_margin", Data_train[, TotalPopulation_log_1e5_xgboost, ])
Data_train_dm

### Testing
Data_test_dm <- xgb.DMatrix(
	data = Data_test[, Features, with = FALSE] %>% as.matrix(),
	label= Data_test[, Outcome, ]
	)
setinfo(Data_test_dm, "base_margin", Data_test[, TotalPopulation_log_1e5_xgboost, ])   
Data_test_dm

### Get prediction values
Data_train[, Pred_xgboost_ICP_pop_pov_edu := predict(Model, newdata = Data_train_dm), ]
Data_test[,  Pred_xgboost_ICP_pop_pov_edu := predict(Model, newdata = Data_test_dm), ]




#-------------------------------
# Get prediction values
# random forest with all variables
#-------------------------------
Model <- readRDS("./models/rf_best.rds") 
Data_train[, Pred_rf := predict(Model, Data_train), ]
Data_test[,  Pred_rf := predict(Model, Data_test), ]

Data_train[, Pred_rf := ifelse(Pred_rf < 0 , 0 , Pred_rf), ]
Data_test[, Pred_rf := ifelse(Pred_rf < 0 , 0 , Pred_rf), ]



#-------------------------------
# Get prediction values
# catboost with all variables
#-------------------------------
mdl <- readRDS("./models/catboost_best.rds")
Data_train <- data.table(Data_train)
Data_train[, Pred_catboost := predict(mdl, Data_train), ]
Data_test[, Pred_catboost := predict(mdl, Data_test), ]

Data_train[, Pred_catboost := ifelse(Pred_catboost < 0 , 0 , Pred_catboost), ]
Data_test[, Pred_catboost := ifelse(Pred_catboost < 0 , 0 , Pred_catboost), ]



#-------------------------------
# Get prediction values
# bam with all variables
#-------------------------------
predict_global <- function(m, newdata){
  t0 <- attr(m, "t0"); dow_lv <- attr(m, "dow_levels")
  stopifnot(!is.null(t0), !is.null(dow_lv))

  nd <- as.data.frame(newdata)
  stopifnot(all(c("date","pop") %in% names(nd)))
  if (!inherits(nd$date, "Date")) nd$date <- as.Date(nd$date)

  nd$dow        <- lubridate::wday(nd$date, week_start = 1)
  nd$dow_factor <- factor(nd$dow, levels = dow_lv)
  nd$t_idx      <- as.integer(nd$date - t0)
  nd$ylen       <- 365L + lubridate::leap_year(nd$date)
  nd$doy        <- lubridate::yday(nd$date)
  nd$doy_frac   <- (nd$doy - 0.5) / nd$ylen

  pop_vec <- as.numeric(nd$pop)
  stopifnot(all(is.finite(log(pop_vec))))

  mu_hat   <- as.numeric(predict(m, newdata = nd, type = "response"))
					 
  rate_hat <- mu_hat / pop_vec
  
  tibble::as_tibble(nd) |>
    dplyr::mutate(mu_hat = mu_hat, rate_hat = rate_hat)

}




# model and data
m_nb <- readRDS("./models/bam_hypr_best.rds")


#-----train
# prepare data
tmp_train <- copy(Data_train)

# get prediction values
tmp_train[, ":="(
	y = Outcome
	, region_id = id
	, date = Time
	, pop = TotalPopulation
	), ]
	
tmp_train <- predict_global(m_nb, tmp_train)
tmp_train <- data.table(tmp_train)
setnames(tmp_train, "mu_hat", "Pred_bam_ref")

Data_train <- merge(Data_train, tmp_train[, list(id, Time, Pred_bam_ref), ], by = c("id", "Time"))



#----test
# prepare data
tmp_test <- copy(Data_test)

# get prediction values
tmp_test[, ":="(
	y = Outcome
	, region_id = id
	, date = Time
	, pop = TotalPopulation
	), ]
	
tmp_test <- predict_global(m_nb, tmp_test)
tmp_test <- data.table(tmp_test)
setnames(tmp_test, "mu_hat", "Pred_bam_ref")

Data_test <- merge(Data_test, tmp_test[, list(id, Time, Pred_bam_ref), ], by = c("id", "Time"))



# treat 0 value
Data_train[, Pred_bam_ref := ifelse(Pred_bam_ref < 0 , 0 , Pred_bam_ref), ]
Data_test[, Pred_bam_ref := ifelse(Pred_bam_ref < 0 , 0 , Pred_bam_ref), ]



#-------------------------------
# Check predicted values of models of interest
#-------------------------------
pred_cols


#-------------------------------
# Temperature strata
#-------------------------------
## preparation
Data_train[, EMS_AgencyID := copy(id), ]
Data_test[, EMS_AgencyID := copy(id), ]

Temp <- copy(Data_train)
Temp <- Temp[, .(Mean_temp_mean_agency = mean(Meantemp_imp)), by = EMS_AgencyID]
(Mean_temp_mean_agency_25 <- quantile(Temp$Mean_temp_mean_agency, p = 0.25))
(Mean_temp_mean_agency_75 <- quantile(Temp$Mean_temp_mean_agency, p = 0.75))




## train
Data_train[, Mean_temp_mean_agency_temp := mean(Meantemp_imp), by = EMS_AgencyID]

Data_train[, Mean_temp_mean_agency_category :=
	ifelse(Mean_temp_mean_agency_temp < Mean_temp_mean_agency_25, "0low",
	ifelse(Mean_temp_mean_agency_temp < Mean_temp_mean_agency_75, "1mid",
	"2high")), ]



## test
Data_test[, Mean_temp_mean_agency_temp := mean(Meantemp_imp), by = EMS_AgencyID]

Data_test[, Mean_temp_mean_agency_category :=
	ifelse(Mean_temp_mean_agency_temp < Mean_temp_mean_agency_25, "0low",
	ifelse(Mean_temp_mean_agency_temp < Mean_temp_mean_agency_75, "1mid",
	"2high")), ]


## save
Temp1 <- Data_train[duplicated(id) == FALSE, list(id, Mean_temp_mean_agency_category), ]
Temp1[, TrainTest := "Train", ]
Temp2 <- Data_test[duplicated(id) == FALSE, list(id, Mean_temp_mean_agency_category), ]
Temp2[, TrainTest := "Test", ]
fwrite(rbind(Temp1, Temp2), paste0(SAVING_DIRECTORY, "/data_agencyID_tempCategory.csv"))







##-------------------------------
##------ State x agency N
##-------------------------------
## train
Data_train[, EMS_AgencyID_state := paste(StateID, EMS_AgencyID, sep = "_"), ]
Temp <- copy(Data_train[duplicated(EMS_AgencyID_state) == FALSE, .(N = .N, Population = sum(TotalPopulation_imp)), by = StateID])
Data_train[, EMS_AgencyID_state := NULL, ]

setorder(Temp, StateID)
setorder(Temp, -N)
Temp[, Selected := ifelse(N >= 5, 1, 0), ]
Temp
fwrite(Temp,  paste0(SAVING_DIRECTORY, "/Table_N_of_agency_by_sate_train.csv"))

StateID_selected_train <- Temp[Selected == 1, unique(StateID), ]
StateID_selected_train



## test
Data_test[, EMS_AgencyID_state := paste(StateID, EMS_AgencyID, sep = "_"), ]
Temp <- copy(Data_test[duplicated(EMS_AgencyID_state) == FALSE, .(N = .N, Population = sum(TotalPopulation_imp)), by = StateID])
Data_test[, EMS_AgencyID_state := NULL, ]

setorder(Temp, StateID)
setorder(Temp, -N)
Temp[, Selected := ifelse(N >= 5, 1, 0), ]
Temp
fwrite(Temp,  paste0(SAVING_DIRECTORY, "/Table_N_of_agency_by_sate_test.csv"))

StateID_selected_test <- Temp[Selected == 1, unique(StateID), ]
StateID_selected_test


## all
Data_all <- rbind(Data_train, Data_test, fill = TRUE)
Data_all[, EMS_AgencyID_state := paste(StateID, EMS_AgencyID, sep = "_"), ]
Temp <- copy(Data_all[duplicated(EMS_AgencyID_state) == FALSE, .(N = .N, Population = sum(TotalPopulation_imp)), by = StateID])
Data_all[, EMS_AgencyID_state := NULL, ]
setorder(Temp, StateID)
Temp
fwrite(Temp,  paste0(SAVING_DIRECTORY, "/Table_N_of_agency_by_sate_train_test.csv"))

Data_all <- NULL






#############################
#### Seasons
Data_train[, Season := 
	ifelse(Months_imp %in% c(6,7,8), "Summer", 
	ifelse(Months_imp %in% c(12, 1, 2), "Winter",
	ifelse(Months_imp %in% c(3, 4, 5), "Spring",
	"Autum"))), ]
	


Data_test[, Season := 
	ifelse(Months_imp %in% c(6,7,8), "Summer", 
	ifelse(Months_imp %in% c(12, 1, 2), "Winter",
	ifelse(Months_imp %in% c(3, 4, 5), "Spring",
	"Autum"))), ]
	

#############################
# for fixest
#############################
Data_train[, w := 1, ]
Data_test[, w := 1, ]
z <- qnorm(0.975)   
LAG_NW <- 12 



#-------------------------------
# Performance at agency level
#-------------------------------
# train
train <- Data_train[, .(
  TrainTest = "train",
  model     = pred_cols,

  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols][
  , `:=`(Season = "ALL", Mean_temp_mean_agency_category = "ALL")
]




train_season <- Data_train[, .(
  TrainTest = "train",
  model     = pred_cols,

  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category) ]





# test
test <- Data_test[, .(
  TrainTest = "test",
  model     = pred_cols,

  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols][
  , `:=`(Season = "ALL", Mean_temp_mean_agency_category = "ALL")
]


test_season <- Data_test[, .(
  TrainTest = "test",
  model     = pred_cols,

  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id   = id,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category) ]



train_test <- rbindlist(list(train, test, train_season, test_season), fill = TRUE)
train_test

fwrite(train_test, paste0(SAVING_DIRECTORY, "/prediction_performance_agency_ci.csv"))






# # --- make plots like calibration plots
# clbplot_wd <- SAVING_DIRECTORY
# pred_cols_clbplot
# 
# for(iii in pred_cols_clbplot){
# 
# # organize data
# Data_train_cbplot <- copy(Data_train)
# setnames(Data_train_cbplot, iii, "tmp_pred")
# #Data_train_cbplot <- Data_train_cbplot %>% mutate(y = Outcome / TotalPopulation_imp * 1e5, yhat = tmp_pred / TotalPopulation_imp * 1e5)
# Data_train_cbplot <- Data_train_cbplot %>% mutate(y = Outcome, yhat = tmp_pred)
# 
# 
# Data_test_cbplot <- copy(Data_test)
# setnames(Data_test_cbplot, iii, "tmp_pred")
# #Data_test_cbplot <- Data_test_cbplot %>% mutate(y = Outcome / TotalPopulation_imp * 1e5, yhat = tmp_pred / TotalPopulation_imp * 1e5)
# Data_test_cbplot <- Data_test_cbplot %>% mutate(y = Outcome, yhat = tmp_pred)
# 
# 
# # ビン分け（10等分） in training dataset
# n_bins <- 10
# calib_bins <- Data_train_cbplot %>%
#   mutate(
# 	bin = ntile(yhat, n_bins)
# 	) %>%  
#   group_by(bin) %>%
#   summarise(
#     mean_pred = mean(yhat, na.rm = TRUE),
#     mean_obs  = mean(y,    na.rm = TRUE),
#     n         = n(),
#     se_obs    = sd(y, na.rm = TRUE) / sqrt(n)
#   ) %>%
#   ungroup()
# 
# # プロット
# ggplot(calib_bins, aes(x = mean_pred, y = mean_obs)) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   geom_errorbar(aes(ymin = mean_obs - 1.96 * se_obs,
#                     ymax = mean_obs + 1.96 * se_obs),
#                 width = 0) +
#   geom_point(size = 2) +
#   geom_line() +
#   coord_equal() +
#   scale_y_continuous(limits = c(NA, NA)) +   # ★ ここでY軸を0〜1に固定
#   labs(title = "Calibration-style plot for count outcome (overall) in training data",
#        x = "Mean predicted count (by quantile bin)",
#        y = "Mean observed count (by quantile bin)") +
#   theme_minimal()
# 
# 
# ggsave(paste0(clbplot_wd, "/clbplot_trainingdata_", iii, ".svg"), width = 7, height = 7, units = "in")
# 
# 
# # ビン分け（10等分） in test dataset
# n_bins <- 10
# calib_bins <- Data_test_cbplot %>%
#   mutate(
# 	bin = ntile(yhat, n_bins)
# 	) %>%  
#   group_by(bin) %>%
#   summarise(
#     mean_pred = mean(yhat, na.rm = TRUE),
#     mean_obs  = mean(y,    na.rm = TRUE),
#     n         = n(),
#     se_obs    = sd(y, na.rm = TRUE) / sqrt(n)
#   ) %>%
#   ungroup()
# 
# # プロット
# ggplot(calib_bins, aes(x = mean_pred, y = mean_obs)) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   geom_errorbar(aes(ymin = mean_obs - 1.96 * se_obs,
#                     ymax = mean_obs + 1.96 * se_obs),
#                 width = 0) +
#   geom_point(size = 2) +
#   geom_line() +
#   coord_equal() +
#   scale_y_continuous(limits = c(NA, NA)) +   # ★ ここでY軸を0〜1に固定
#   labs(title = "Calibration-style plot for count outcome (overall) in testing data",
#        x = "Mean predicted count (by quantile bin)",
#        y = "Mean observed count (by quantile bin)") +
#   theme_minimal()
# 
# 
# ggsave(paste0(clbplot_wd, "/clbplot_testdata_", iii, ".svg"), width = 7, height = 7, units = "in")
# 
# 
# }







#-------------------------------
# Performance at national level
#-------------------------------
## preparation
Data_train_national <- Data_train[
  , c(
      list(
        TrainTest = "train",
        TotalPopulation_imp = sum(TotalPopulation_imp),
        Outcome = sum(Outcome)
      ),
      setNames(lapply(.SD, sum), paste0(pred_cols))
    ),
  by = "Time",
  .SDcols = pred_cols
]

Data_train_national[, Year := data.table::year(Time), ]




Data_test_national <- Data_test[
  , c(
      list(
        TrainTest = "test",
        TotalPopulation_imp = sum(TotalPopulation_imp),
        Outcome = sum(Outcome)
      ),
      setNames(lapply(.SD, sum), paste0(pred_cols))
    ),
  by = "Time",
  .SDcols = pred_cols
]

Data_test_national[, Year := data.table::year(Time), ]





## preparation season temperature
Data_train_national_season <- Data_train[
  , c(
      list(
        TrainTest = "train",
        TotalPopulation_imp = sum(TotalPopulation_imp),
        Outcome = sum(Outcome)
      ),
      setNames(lapply(.SD, sum), paste0(pred_cols))
    ),
  by = .(Time, Season, Mean_temp_mean_agency_category),
  .SDcols = pred_cols
]

Data_train_national_season[, Year := data.table::year(Time), ]



Data_test_national_season <- Data_test[
  , c(
      list(
        TrainTest = "test",
        TotalPopulation_imp = sum(TotalPopulation_imp),
        Outcome = sum(Outcome)
      ),
      setNames(lapply(.SD, sum), paste0(pred_cols))
    ),
  by = .(Time, Season, Mean_temp_mean_agency_category),
  .SDcols = pred_cols
]

Data_test_national_season[, Year := data.table::year(Time), ]





# train
Data_train_national[, w := 1, ]
Data_train_national_season[, w := 1, ]
Data_test_national[, w := 1, ]
Data_test_national_season[, w := 1, ]




#####
train <- Data_train_national[, .(
    TrainTest = "train",
    model     = pred_cols,
    
    MAPE_CI = lapply(.SD, function(p) {
      d <- data.frame(
        mape = abs(Outcome - p) / pmax(abs(Outcome), 1e-6),
        w    = w,
        Time = Time
      )
      est <- feols(mape ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.2f%% (%.2f%% to %.2f%%)", 100*mu, 100*(mu - z*se), 100*(mu + z*se))
    }),

    RMSE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
                (p        / TotalPopulation_imp * 1e5))^2,
        w    = w,
        Time = Time
      )
      est <- feols(e2_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      lo  <- max(mu - z*se, 0); hi <- mu + z*se
      sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
    }),

    MAE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                   (p        / TotalPopulation_imp * 1e5)),
        w    = w,
        Time = Time
      )
      est <- feols(ae_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
    })
), .SDcols = pred_cols][
  , `:=`(Season = "ALL", Mean_temp_mean_agency_category = "ALL")
]




train_season <- Data_train_national_season[, .(
    TrainTest = "train",
    model     = pred_cols,
 
    MAPE_CI = lapply(.SD, function(p) {
      d <- data.frame(
        mape = abs(Outcome - p) / pmax(abs(Outcome), 1e-6),
        w    = w,
        Time = Time
      )
      est <- feols(mape ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.2f%% (%.2f%% to %.2f%%)", 100*mu, 100*(mu - z*se), 100*(mu + z*se))
    }),

    RMSE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
                (p        / TotalPopulation_imp * 1e5))^2,
        w    = w,
        Time = Time
      )
      est <- feols(e2_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      lo  <- max(mu - z*se, 0); hi <- mu + z*se
      sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
    }),

    MAE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                   (p        / TotalPopulation_imp * 1e5)),
        w    = w,
        Time = Time
      )
      est <- feols(ae_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
    })
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category) ]




test <- Data_test_national[, .(
    TrainTest = "test",
    model     = pred_cols,

    MAPE_CI = lapply(.SD, function(p) {
      d <- data.frame(
        mape = abs(Outcome - p) / pmax(abs(Outcome), 1e-6),
        w    = w,
        Time = Time
      )
      est <- feols(mape ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.2f%% (%.2f%% to %.2f%%)", 100*mu, 100*(mu - z*se), 100*(mu + z*se))
    }),

    RMSE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
                (p        / TotalPopulation_imp * 1e5))^2,
        w    = w,
        Time = Time
      )
      est <- feols(e2_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      lo  <- max(mu - z*se, 0); hi <- mu + z*se
      sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
    }),

    MAE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                   (p        / TotalPopulation_imp * 1e5)),
        w    = w,
        Time = Time
      )
      est <- feols(ae_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
    })
), .SDcols = pred_cols][
  , `:=`(Season = "ALL", Mean_temp_mean_agency_category = "ALL")
]




test_season <- Data_test_national_season[, .(
    TrainTest = "test",
    model     = pred_cols,

    MAPE_CI = lapply(.SD, function(p) {
      d <- data.frame(
        mape = abs(Outcome - p) / pmax(abs(Outcome), 1e-6),
        w    = w,
        Time = Time
      )
      est <- feols(mape ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.2f%% (%.2f%% to %.2f%%)", 100*mu, 100*(mu - z*se), 100*(mu + z*se))
    }),

    RMSE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
                (p        / TotalPopulation_imp * 1e5))^2,
        w    = w,
        Time = Time
      )
      est <- feols(e2_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      lo  <- max(mu - z*se, 0); hi <- mu + z*se
      sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
    }),

    MAE_inc_CI = lapply(.SD, function(p) {
      d <- data.frame(
        ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                   (p        / TotalPopulation_imp * 1e5)),
        w    = w,
        Time = Time
      )
      est <- feols(ae_i ~ 1, data = d, vcov = NW(lag = LAG_NW) ~ Time)
      mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
      sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
    })
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category) ]


train_test <- rbindlist(list(train, test, train_season, test_season), fill = TRUE)
train_test

fwrite(train_test, paste0(SAVING_DIRECTORY, "/prediction_performance_national_ci.csv"))









### plot at national
for (pred in pred_cols) {
  
  #
  p_train <- ggplot() + 
    geom_line(data = Data_train_national,
              aes(x = Time, y = Outcome / TotalPopulation_imp * 1e5),
              colour = "steelblue2") + 
    geom_line(data = Data_train_national,
              aes(x = Time, y = !!sym(pred) / TotalPopulation_imp * 1e5),
              colour = "goldenrod2") +    
    ylab("Incidence rate per 100,000 per day at the national level") + 
    ylim(0, 0.35)
		 
	ggsave(
		filename = file.path(SAVING_DIRECTORY, sprintf("Fig_%s_national_train_p1.svg", pred)),
		plot     = p_train,
		width    = 7, height = 7, dpi = 300
  )
		 
  
  # 
  p_test <- ggplot() + 
    geom_line(data = Data_test_national,
              aes(x = Time, y = Outcome / TotalPopulation_imp * 1e5),
              colour = "steelblue2") + 
    geom_line(data = Data_test_national,
              aes(x = Time, y = !!sym(pred) / TotalPopulation_imp * 1e5),
              colour = "goldenrod2") +    
    ylab("Incidence rate per 100,000 per day at the national level") + 
    ylim(0, 0.35)
  
	ggsave(	 
		filename = file.path(SAVING_DIRECTORY, sprintf("Fig_%s_national_test_p1.svg", pred)),
		plot     = p_test,
		width    = 7, height = 7, dpi = 300
  )
		 
}













#-------------------------------
# Performance at state level
#-------------------------------
# preparation
Data_train_state <- Data_train[StateID %in% StateID_selected_train
  ,
  c(
    list(
      TrainTest = "train",
      TotalPopulation_imp = sum(TotalPopulation_imp),
      Outcome = sum(Outcome)
    ),
    setNames(lapply(.SD, sum), paste0(pred_cols))
  ),
  by = .(Time, StateID),
  .SDcols = pred_cols
]


Data_train_state[, Year := data.table::year(Time), ]






Data_test_state <- Data_test[StateID %in% StateID_selected_test
  , 
  c(
      list(
        TrainTest = "test",
        TotalPopulation_imp = sum(TotalPopulation_imp),
        Outcome = sum(Outcome)
      ),
      setNames(lapply(.SD, sum), paste0(pred_cols))
    ),
  by = .(Time, StateID),
  .SDcols = pred_cols
]


Data_test_state[, Year := data.table::year(Time), ]






# season
Data_train_state_season <- Data_train[StateID %in% StateID_selected_train
  , c(
      list(
        TrainTest = "train",
        TotalPopulation_imp = sum(TotalPopulation_imp),
        Outcome = sum(Outcome)
      ),
      setNames(lapply(.SD, sum), paste0(pred_cols))
    ),
  by = .(Time, StateID, Season, Mean_temp_mean_agency_category),
  .SDcols = pred_cols
]

Data_train_state_season[, Year := data.table::year(Time), ]






Data_test_state_season <- Data_test[StateID %in% StateID_selected_test
  , c(
      list(
        TrainTest = "test",
        TotalPopulation_imp = sum(TotalPopulation_imp),
        Outcome = sum(Outcome)
      ),
      setNames(lapply(.SD, sum), paste0(pred_cols))
    ),
  by = .(Time, StateID, Season, Mean_temp_mean_agency_category),
  .SDcols = pred_cols
]


Data_test_state_season[, Year := data.table::year(Time), ]











#-------------------------------
Data_train_state[, w := 1, ]
Data_test_state[, w := 1, ]
Data_train_state_season[, w := 1, ]
Data_test_state_season[, w := 1, ]


#-------------------------------
# Performance at agency level
#-------------------------------
# train
train <- Data_train_state[, .(
  TrainTest = "train",
  model     = pred_cols,


  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols][
  , `:=`(Season = "ALL", Mean_temp_mean_agency_category = "ALL")
]




train_season <- Data_train_state_season[, .(
  TrainTest = "train",
  model     = pred_cols,

  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category) ]





# test
test <- Data_test_state[, .(
  TrainTest = "test",
  model     = pred_cols,

  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols][
  , `:=`(Season = "ALL", Mean_temp_mean_agency_category = "ALL")
]




test_season <- Data_test_state_season[, .(
  TrainTest = "test",
  model     = pred_cols,

  #
  RMSE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      e2_i = ((Outcome / TotalPopulation_imp * 1e5) -
              (p        / TotalPopulation_imp * 1e5))^2,
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(e2_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    lo  <- max(mu - z*se, 0); hi <- mu + z*se
    sprintf("%.3f (%.3f to %.3f)", sqrt(mu), sqrt(lo), sqrt(hi))
  }),

  MAE_inc_CI = lapply(.SD, function(p) {
    d <- data.frame(
      ae_i = abs((Outcome / TotalPopulation_imp * 1e5) -
                 (p        / TotalPopulation_imp * 1e5)),
      w    = w,
      id = StateID,
      Time = Time
    )
    est <- feols(ae_i ~ 1, data = d, weights = ~ w, vcov = ~ id + Time)
    mu  <- unname(coef(est)[1]); se <- unname(se(est)[1])
    sprintf("%.3f (%.3f to %.3f)", mu, mu - z*se, mu + z*se)
  })
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category) ]




train_test <- rbindlist(list(train, test, train_season, test_season), fill = TRUE)
train_test




fwrite(train_test, paste0(SAVING_DIRECTORY, "/prediction_performance_state_ci.csv"))

