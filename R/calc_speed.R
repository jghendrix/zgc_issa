#' @title Calculate speed
#' @export
#' @author Julie W. Turner
calc_speed <- function(DT, covariate, seq) {



	if(covariate == "forest")
	DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
															`prop_forest:I(log(sl_))`*seq +
															`N_Cladonia:I(log(sl_))`*mean_Clad +
															`N_vasc:I(log(sl_))`*mean_vasc
	)*(scale))),
	x = list(list(seq))),
	by=.(id)]


  if(covariate == "Cladonia")
    DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
                                  `prop_forest:I(log(sl_))`*mean_forest +
                                  `N_Cladonia:I(log(sl_))`*seq +
                                  `N_vasc:I(log(sl_))`*mean_vasc
    )*(scale))),
    x = list(list(seq))),
    by=.(id)]
  
  if(covariate == "forest")
    DT[, `:=` (spd = list(list((shape +`I(log(sl_))` +
                                  `prop_forest:I(log(sl_))`*mean_forest +
                                  `N_Cladonia:I(log(sl_))`*mean_Clad +
                                  `N_vasc:I(log(sl_))`*seq
    )*(scale))),
    x = list(list(seq))),
    by=.(id)]
  


	move <- DT[, .(spd = unlist(spd), x = unlist(x)), by=.(id)]

	move %<>% mutate(spd = spd/2
							#		 ,
							#	 spd = ifelse(spd < 0, NA, spd)
								 )
}
