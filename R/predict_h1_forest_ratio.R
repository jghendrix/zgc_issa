#' @title Predict H1 forest
#' @export
#' @author Jack G Hendrix
predict_h1_forest_ratio <- function(DT, model) {
	N <- 100L

	setDT(DT)
	
	new_data <- DT[, .(
		sl_ = mean(sl_),
		ta_ = mean(ta_),
		prop_forest = seq(from = 0, to = 1, length.out = N),
		CN_Cladonia = mean(CN_Cladonia, na.rm = T),
		CN_vasc = mean(CN_vasc, na.rm = T),
		indiv_step_id = NA,
		yr = NA
	), by = id]

	new_data[, h1_forest := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
}
