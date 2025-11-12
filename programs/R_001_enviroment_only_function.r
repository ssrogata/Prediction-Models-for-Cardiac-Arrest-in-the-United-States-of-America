#------------------
# functions
#------------------
RMSE <- function(Obs, Pred){
	Dif <- Pred - Obs
	RMSE <- round(sqrt(mean(Dif**2)), 10)
	return(RMSE)
}

MAE2 <- function(Obs, Pred){
	Dif <- Pred - Obs
	MAE <- mean(abs(Dif)) %>% round(., 10)
	return(MAE)
}

MAPE <- function(Obs, Pred){
	Dif <- Pred - Obs
	MAPE <- mean(abs(Dif/Obs)*100) %>% round(., 2)
	return(MAPE)
}

shap.plot.summary.edited <- function(
						  data_long,
                          x_bound = NULL,
                          dilute = FALSE,
                          scientific = FALSE,
                          my_format = NULL){

	# packages
	library(xgboost)
	library(DALEX)
	library(iBreakDown)
	library(r2d3)
	library(SHAPforxgboost)

						  
	if (scientific){label_format = "%.1e"} else {label_format = "%.3f"}	
	if (!is.null(my_format)) label_format <- my_format


	N_features <- setDT(data_long)[,uniqueN(variable)]
	if (is.null(dilute)) dilute = FALSE
	nrow_X <- nrow(data_long)/N_features # n per feature
	if (dilute!=0){
		# if nrow_X <= 10, no dilute happens
		dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute)))) 
		set.seed(1234)
		data_long <- data_long[sample(nrow(data_long),
									min(nrow(data_long)/dilute, nrow(data_long)/2))] 
	}
	
	
	x_bound <- if (is.null(x_bound)) max(abs(data_long$value))*1.1 else as.numeric(abs(x_bound))
	plot1 <- ggplot(data = data_long) +
		coord_flip(ylim = c(-x_bound, x_bound)) +
		geom_hline(yintercept = 0) + 

		ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
						method = "counts", maxwidth = 0.7, alpha = 0.7) + 
						scale_color_gradient(
							low="#FFCC33", high="#6600CC",
							breaks=c(0,1), labels=c(" Low","High "),
							guide = guide_colorbar(barwidth = 12, barheight = 0.3)
							) +
			theme_bw() +
			theme(axis.line.y = element_blank(),
				axis.ticks.y = element_blank(), # remove axis line
				legend.position="bottom",
				legend.title=element_text(size=10),
				legend.text=element_text(size=8),
				axis.title.x= element_text(size = 10)) +

			scale_x_discrete(limits = rev(levels(data_long$variable))) +
			labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value  ")

	return(plot1)
}
