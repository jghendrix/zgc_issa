#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2 <- function(DT, model) {

  setDT(DT)
  
	new_data <- DT[, .(
		sl_ = mean(sl_),
		ta_ = mean(ta_),
		prop_forest = mean(prop_forest, na.rm = T),
		N_Cladonia = mean(N_Cladonia, na.rm = T),
		N_vasc = mean(N_vasc, na.rm = T),
		indiv_step_id = NA,
		yr = NA),
		by = id]

	new_data[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
}
