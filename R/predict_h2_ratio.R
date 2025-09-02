#' @title Predict H2
#' @export
#' @author Jack G Hendrix
predict_h2_ratio <- function(DT, model) {

  setDT(DT)
  
	new_data <- DT[, .(
		sl_ = mean(sl_),
		ta_ = mean(ta_),
		prop_forest = mean(prop_forest, na.rm = T),
		CN_Cladonia = mean(CN_Cladonia, na.rm = T),
		CN_vasc = mean(CN_vasc, na.rm = T),
		indiv_step_id = NA,
		yr = NA),
		by = id]

	new_data[, h2 := predict(model, .SD, type = 'link', re.form = NULL)]
}
