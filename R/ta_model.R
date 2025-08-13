#' @title iSSA of stoich - turn angle interaction instead of step length
#' @export
#' @author Jack G Hendrix
#' 
ta_model <- function(DT) {

 	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
		  I(cos(ta_)) +
			prop_forest +
			N_Cladonia +
			N_vasc +
			prop_forest:I(cos(ta_)) +
			N_Cladonia:I(cos(ta_)) +
			N_vasc:I(cos(ta_)) +
			(1 | indiv_step_id) +
	    (1 | yr) +
			(0 + prop_forest | id) +
			(0 + N_Cladonia | id) +
			(0 + N_vasc | id) +
		  (0 + I(cos(ta_)) | id) +
	 	  (0 + prop_forest:I(cos(ta_)) | id) +
			(0 + N_Cladonia:I(cos(ta_))| id) +
			(0 + N_vasc:I(cos(ta_)) | id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:8))),
	start = list(theta = c(log(1000), seq(0, 0, length.out = 8)))
	)
}
