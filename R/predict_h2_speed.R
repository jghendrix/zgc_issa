#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_speed <- function(DT, model, quart) {

  setDT(DT)
  
	new_data <- DT[, .(
		sl_ = quantile(sl_, quart),
		ta_ = mean(ta_),
		prop_forest = mean(prop_forest, na.rm = T),
		N_Cladonia = mean(N_Cladonia, na.rm = T),
		N_vasc = mean(N_vasc, na.rm = T),
		indiv_step_id = NA,
		yr = NA),
		by = id]

	new_data[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
	new_data[, speed := quart]
}
