#' @title iSSA informed by C:N stoich layer
#' @export
#' @author Jack G Hendrix
#' 
ratio_model <- function(DT) {

 	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
		  I(cos(ta_)) +
			prop_forest +
			CN_Cladonia +
			CN_vasc +
			prop_forest:I(log(sl_)) +
			CN_Cladonia:I(log(sl_)) +
			CN_vasc:I(log(sl_)) +
			(1 | indiv_step_id) +
	    (1 | yr) +
			(0 + prop_forest | id) +
			(0 + CN_Cladonia | id) +
			(0 + CN_vasc | id) +
		  (0 + I(log(sl_)) | id) +
	 	  (0 + prop_forest:I(log(sl_)) | id) +
			(0 + CN_Cladonia:I(log(sl_)) | id) +
			(0 + CN_vasc:I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:8))),
	start = list(theta = c(log(1000), seq(0, 0, length.out = 8)))
	)
}
