#' @title iSSA informed by only forest proportion to compare against StDM layer
#' @export
#' @author Jack G Hendrix
#' 
ref_forest_model <- function(DT) {

 	glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
		  I(cos(ta_)) +
			prop_forest +
		#	N_Cladonia +
	#		N_vasc +
			prop_forest:I(log(sl_)) +
	#		N_Cladonia:I(log(sl_)) +
	#		N_vasc:I(log(sl_)) +
			(1 | indiv_step_id) +
	    (1 | yr) +
			(0 + prop_forest | id) +
	#		(0 + N_Cladonia | id) +
	#		(0 + N_vasc | id) +
		  (0 + I(log(sl_)) | id) +
	 	  (0 + prop_forest:I(log(sl_)) | id) #+
	#		(0 + N_Cladonia:I(log(sl_)) | id) +
	#		(0 + N_vasc:I(log(sl_)) | id)
		,
		data = subset(DT, !is.na(N_vasc) & !is.na(N_Cladonia)),
		family = poisson(),
		map = list(theta = factor(c(NA, 1:4))),
	start = list(theta = c(log(1000), seq(0, 0, length.out = 4)))
 	)
}
