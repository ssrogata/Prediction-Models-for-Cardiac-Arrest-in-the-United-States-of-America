#-----------------------------------
# Make bam model as the reference model
#-----------------------------------
rm(list=ls(all=TRUE))
library(dplyr)
library(lubridate)
library(mgcv)
library(yardstick)
library(tibble)
library(fst)
library(tictoc)
library(conflicted)
conflicts_prefer(dplyr::filter)


if(dir.exists(paste0(getwd(), "/models/bam")) == FALSE) {dir.create(paste0(getwd(), "/models/bam"))}


#---#---#---#---#---
#--- Functions for this program only
#---#---#---#---#---
source("./programs/R_001_enviroment_only_function.r")


prep_panel <- function(df_panel){
  df <- df_panel %>%
    filter(!is.na(y)) %>%              
    mutate(
      region_id = factor(region_id),
      dow = lubridate::wday(date, week_start = 1), 
      t_idx = as.integer(date - min(date)),
      ylen = 365L + lubridate::leap_year(date),
      doy  = lubridate::yday(date),
      doy_frac = (doy - 0.5) / ylen
    ) %>%
    arrange(region_id, date)
  df
}


fit_bam_nb_global <- function(df_train,
                              kt = 80, k_year = 14,
                              gamma = 1.2,
                              nthreads = max(1, parallel::detectCores()-1)) {
  library(mgcv)
  kn <- list(doy_frac = c(0,1))
  df_train <- df_train %>% mutate(dow_factor = factor(dow))

  m <- bam(
    y ~ s(t_idx, k = kt, m = 2) +
         s(doy_frac, bs = "cc", k = k_year, m = 2) +
         dow_factor +
         offset(log(pop)),
    family = nb(),
    method = "fREML", knots = kn, data = df_train,
    discrete = TRUE, nthreads = nthreads, gamma = gamma
  )

  attr(m, "t0")         <- min(df_train$date)
  attr(m, "dow_levels") <- levels(df_train$dow_factor)
  m
}


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




#-------------------------------------
# Data 
#-------------------------------------
data_train <- read_fst("./input/data_train_for_prediction_model_scaled.fst", as.data.table = TRUE)
data_train[, ":="(
	y = Cases
	, region_id = id
	, date = Time
	, pop = TotalPopulation
	), ]

## CV
data_train[, valset := 
	ifelse(n_val_imp <= 20, 0, 
	ifelse(n_val_imp <= 40, 1,
	ifelse(n_val_imp <= 60, 2,
	ifelse(n_val_imp <= 80, 3,
	4)))), ]

## Parameter set
hypr_set <- expand.grid(
	kt = c(60, 80, 100),
	k_year = c(12, 14, 16, 18)
	)
hypr_set


## Making model
hoge <- c()

for(iii_hypr in 1:nrow(hypr_set)){

	# Parameters
	KT <- hypr_set[iii_hypr, "kt"]
	K_YEAR <- hypr_set[iii_hypr, "k_year"]
	
	for(iii_val in unique(data_train$valset)%>%sort()){

		# Training data
		df_panel_train <- data_train[valset != iii_val, , ]
		df_train <- prep_panel(df_panel_train)

		# Validation data
		df_panel_val <- data_train[valset == iii_val, , ]

		# Learning
		tic()
		m_nb <- fit_bam_nb_global(df_train, kt = KT, k_year = K_YEAR)
		toc()
	
		# Evaluate prediction performance in validation data
		df_panel_val <- predict_global(m_nb, df_panel_val)

		# Save information into model file
		m_nb$k_parameters <- list(hypr_no = iii_hypr, kt = KT, k_year = K_YEAR)
		m_nb$train_set <- unique(data_train$valset)[unique(data_train$valset) != iii_val]
		m_nb$cv_set <- iii_val

		m_nb$rmse <- RMSE(Obs = df_panel_val$Cases, Pred = df_panel_val$mu_hat)
		m_nb$mae <- MAE2(Obs = df_panel_val$Cases, Pred = df_panel_val$mu_hat)

		saveRDS(m_nb, paste0("./models/bam/bam_hypr_", iii_hypr, "_cv_", iii_val, ".rds"))

		# Save information into csv format
		temp <- c(
			unlist(m_nb$k_parameters),
			train_set = paste(m_nb$train_set, sep = "_"), 
			cv_set = m_nb$cv_set, 
			rmse = m_nb$rmse, 
			mae = m_nb$mae
		)
		
		hoge <- rbind(hoge, temp)
	}
}


library(data.table)
conflicted::conflicts_prefer(lubridate::wday)
conflicts_prefer(lubridate::yday)
fwrite(hoge, "./output/Table_bam_population_params_round_best.csv")


# Find the best model based on the lowest RMSE
cvresult <- fread("./output/Table_bam_population_params_round_best.csv")
cvresult <- cvresult[, .(
	rmse_mean = mean(rmse),
	kt = unique(kt),
	k_year = unique(k_year)	
	), by = hypr_no
	]
	
cvresult
cvresult[, min(rmse_mean), ]
cvresult <- cvresult[rmse_mean == min(rmse_mean), , ] 
cvresult



# Re-training the model using the best parameter set
tic()

m_nb <- fit_bam_nb_global(
	prep_panel(data_train), 
	kt = cvresult[rmse_mean == min(rmse_mean), "kt"] %>% as.numeric(), 
	k_year = cvresult[rmse_mean == min(rmse_mean), "k_year"] %>% as.numeric()
	)
	
toc()



saveRDS(m_nb, "./models/bam_hypr_best.rds")
