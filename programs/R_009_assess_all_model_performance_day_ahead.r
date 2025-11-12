#-----------------------------------
# Assessing prediction Performance for x-day-ahead
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



for(iii in TIME_FRAME){





##------------ Read data
Data_train <- read_fst(paste0("./input/data_train_for_prediction_model_scaled_slide_", iii, "days.fst"), as.data.table = TRUE)
Data_train[, TotalPopulation_log_1e5_xgboost := log(TotalPopulation / 1e5), ]
Data_train[, TotalPopulation_imp := copy(TotalPopulation), ]

#---------------- Prediction settings
Data_test <- read_fst(paste0("./input/data_test_for_prediction_model_scaled_slide_", iii, "days.fst"), as.data.table = TRUE)
Data_test[, TotalPopulation_log_1e5_xgboost := log(TotalPopulation / 1e5), ]
Data_test[, TotalPopulation_imp := copy(TotalPopulation), ]


if(TEST_DATA_TYPE == "External_place"){
	Data_test <- Data_test[Year_original >= Year_test_start & externalEMS_original == "external", , ]
	WD_SLIDING <- paste0("./output_external_place_sliding_", iii, "days")
}

if(TEST_DATA_TYPE == "Internal_place"){
	Data_test <- Data_test[Year_original >= Year_test_start & externalEMS_original == "not_external", , ]
	WD_SLIDING <- paste0("./output_internal_place_sliding_", iii, "days")
}

setnames(Data_train, OUTCOME_NAME, "Outcome")
setnames(Data_test, OUTCOME_NAME, "Outcome")


if(dir.exists(WD_SLIDING) == FALSE) {dir.create(WD_SLIDING)}
if(dir.exists(paste0(WD_SLIDING, "/tmp")) == FALSE) {dir.create(paste0(WD_SLIDING, "/tmp"))}








#-------------------------------
# Get prediction values
# xgboost with all variables
#-------------------------------
Model <- readRDS(paste0("./models/XGBoost_slide_",  iii, "days_best.rds")) 
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
# Check predicted values of models of interest
#-------------------------------
pred_cols <- pred_cols_slide
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

Data_train[, table(Mean_temp_mean_agency_category), ]




## test
Data_test[, Mean_temp_mean_agency_temp := mean(Meantemp_imp), by = EMS_AgencyID]

Data_test[, Mean_temp_mean_agency_category :=
	ifelse(Mean_temp_mean_agency_temp < Mean_temp_mean_agency_25, "0low",
	ifelse(Mean_temp_mean_agency_temp < Mean_temp_mean_agency_75, "1mid",
	"2high")), ]

Data_test[, table(Mean_temp_mean_agency_category), ]


## save
Temp1 <- Data_train[duplicated(id) == FALSE, list(id, Mean_temp_mean_agency_category), ]
Temp1[, TrainTest := "Train", ]
Temp2 <- Data_test[duplicated(id) == FALSE, list(id, Mean_temp_mean_agency_category), ]
Temp2[, TrainTest := "Test", ]
fwrite(rbind(Temp1, Temp2), paste0(WD_SLIDING, "/data_agencyID_tempCategory.csv"))







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
fwrite(Temp, paste0(WD_SLIDING, "/Table_N_of_agency_by_sate_train.csv"))

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
fwrite(Temp, paste0(WD_SLIDING, "/Table_N_of_agency_by_sate_test.csv"))

StateID_selected_test <- Temp[Selected == 1, unique(StateID), ]
StateID_selected_test


## all
Data_all <- rbind(Data_train, Data_test, fill = TRUE)
Data_all[, EMS_AgencyID_state := paste(StateID, EMS_AgencyID, sep = "_"), ]
Temp <- copy(Data_all[duplicated(EMS_AgencyID_state) == FALSE, .(N = .N, Population = sum(TotalPopulation_imp)), by = StateID])
Data_all[, EMS_AgencyID_state := NULL, ]
setorder(Temp, StateID)
Temp
fwrite(Temp, paste0(WD_SLIDING, "/Table_N_of_agency_by_sate_train_test.csv"))

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

fwrite(train_test, paste0(WD_SLIDING, "/prediction_performance_agency_ci.csv"))

	







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
][]





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





# test
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
][]






# test
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

fwrite(train_test, paste0(WD_SLIDING, "/prediction_performance_national_ci.csv"))







### plot at national
library(ggsci)
library(rlang)  # sym, !!

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
  
  ggsave(file.path(WD_SLIDING, sprintf("Fig_%s_national_train_p1.svg", pred)),
         p_train, width = 7, height = 7, device = svg, dpi = 300)
  
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
  
  ggsave(file.path(WD_SLIDING, sprintf("Fig_%s_national_test_p1.svg", pred)),
         p_test, width = 7, height = 7, device = svg, dpi = 300)
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
][]





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
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category)]







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
][]




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
), .SDcols = pred_cols, by = .(Season, Mean_temp_mean_agency_category)]




train_test <- rbindlist(list(train, test, train_season, test_season), fill = TRUE)
train_test

fwrite(train_test, paste0(WD_SLIDING, "/prediction_performance_state_ci.csv"))




}

