#' @title Prepare data for winter kernel density data
#' @export
#' @author Jack G Hendrix
prepare_kernel <- function(DT, season, split_by) {

 # We don't want to restrict to only the northern half, like we did for the StDM & iSSA, because more of the summer range is on the south of the island and we don't want to skew the seasonal estimates
  
    if(season == "winter"){
    
    DT %<>% filter(doy < 61)
    
  }
  
  if(season == "summer"){
    
  DT %<>% filter(doy >= 167 & doy <= 228)
   
  }
  
  DT[, tar_group := .GRP, by = c(split_by)]
  
  return(DT)

  	}
