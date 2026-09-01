#' @title iSSA informed by no environmental variables at all
#' @export
#' @author Jack G Hendrix
#' 
null_model <- function(DT) {
  
  DT %<>% filter(!is.na(N_vasc) & !is.na(N_Cladonia) & !is.na(prop_forest))
  
  glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
		  I(cos(ta_)) +
		  (1 | indiv_step_id) +
	    (1 | yr) +
		  (0 + I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:2))),
	start = list(theta = c(log(1000), seq(0, 0, length.out = 2)))
 	)
}
