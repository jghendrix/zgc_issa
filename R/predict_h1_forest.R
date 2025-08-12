#' @title Predict H1 forest
#' @export
#' @author Jack G Hendrix
predict_h1_forest <- function(DT, model) {
	N <- 100L

	setDT(DT)
	
	new_data <- DT[, .(
		sl_ = mean(sl_),
		ta_ = mean(ta_),
		prop_forest = seq(from = 0, to = 1, length.out = N),
		N_Cladonia = mean(N_Cladonia, na.rm = T),
		N_vasc = mean(N_vasc, na.rm = T),
		indiv_step_id = NA,
		yr = NA
	), by = id]

	new_data[, h1_forest := predict(model, .SD, type = 'link', re.form = NULL)]

	new_data[, x := seq(from = 0, to = 1, length.out = N), by = id]
}
