#' @title iSSA informed by categorical land cover to compare against StDM layer
#' @export
#' @author Jack G Hendrix
#' 
ref_lc_model <- function(DT) {
  
  DT %<>% filter(!is.na(N_vasc) & !is.na(N_Cladonia)) %>%
    mutate(prop_barrens = ifelse(is.na(prop_barrens), 0, prop_barrens),
           prop_wetland = ifelse(is.na(prop_wetland), 0, prop_wetland))
  
  glmmTMB(
		case_ ~ -1 +
			I(log(sl_)) +
		  I(cos(ta_)) +
			prop_forest +
		  prop_barrens +
		  prop_wetland +
			prop_forest:I(log(sl_)) +
		  prop_barrens:I(log(sl_)) +
		  prop_wetland:I(log(sl_)) +
		  (1 | indiv_step_id) +
	    (1 | yr) +
			(0 + prop_forest | id) +
		  (0 + prop_barrens | id) +
		  (0 + prop_wetland | id) +
		  (0 + I(log(sl_)) | id) +
	 	  (0 + prop_forest:I(log(sl_)) | id) +
		  (0 + prop_barrens:I(log(sl_)) | id) +
	  	(0 + prop_wetland:I(log(sl_)) | id)
		,
		data = DT,
		family = poisson(),
		map = list(theta = factor(c(NA, 1:8))),
	start = list(theta = c(log(1000), seq(0, 0, length.out = 8)))
 	)
}

# with prop_barrens and prop_wetland, better AIC than stoich model; 
# just retaining prop_forest, much worse AIC