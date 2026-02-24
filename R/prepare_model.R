#' @title Prepare model data
#' @export
#' @author Jack G Hendrix
prepare_model <- function(DT, season) {

  if(season == "winter"){
    
    DT %<>% filter(doy < 61)

  }
  
  if(season == "summer"){
    
  DT %<>% filter(doy >= 167 & doy <= 228)
   
  }
  
  else{}
  
  return(DT)
  
	}
