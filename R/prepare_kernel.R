#' @title Prepare data for winter kernel density data
#' @export
#' @author Jack G Hendrix
prepare_kernel <- function(DT, season, split_by) {

  DT %<>% filter(y_ > 49.64336) #from FO2023011, the southern cutoff
  
  if(season == "winter"){
    
    DT %<>% filter(doy < 61)
    
  }
  
  if(season == "summer"){
    
  DT %<>% filter(doy >= 167 & doy <= 228)
   
  }
  
  DT[, tar_group := .GRP, by = c(split_by)]
  
  return(DT)

  	}
