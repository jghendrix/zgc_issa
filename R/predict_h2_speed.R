#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_speed <- function(DT, model) {

  setDT(DT)
  
	new_data_slow <- DT[, .(
		sl_ = quantile(sl_, 0.1),
		ta_ = mean(ta_),
		prop_forest = mean(prop_forest, na.rm = T),
		N_Cladonia = mean(N_Cladonia, na.rm = T),
		N_vasc = mean(N_vasc, na.rm = T),
		indiv_step_id = NA,
		yr = NA),
		by = id]

	new_data_slow[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
	new_data_slow[, speed := "10th"]
	
	new_data_med <- DT[, .(
	  sl_ = quantile(sl_, 0.5),
	  ta_ = mean(ta_),
	  prop_forest = mean(prop_forest, na.rm = T),
	  N_Cladonia = mean(N_Cladonia, na.rm = T),
	  N_vasc = mean(N_vasc, na.rm = T),
	  indiv_step_id = NA,
	  yr = NA),
	  by = id]
	
	new_data_med[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
	new_data_med[, speed := "50th"]
	
	new_data_fast <- DT[, .(
	  sl_ = quantile(sl_, 0.9),
	  ta_ = mean(ta_),
	  prop_forest = mean(prop_forest, na.rm = T),
	  N_Cladonia = mean(N_Cladonia, na.rm = T),
	  N_vasc = mean(N_vasc, na.rm = T),
	  indiv_step_id = NA,
	  yr = NA),
	  by = id]
	
	new_data_fast[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
	new_data_fast[, speed := "90th"]
	
	new_data <- rbind(new_data_slow, new_data_med, new_data_fast)
	
}
