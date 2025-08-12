#' @title Prepare model data
#' @export
#' @author Jack G Hendrix
prepare_model <- function(DT, season) {

  DT %<>%
    rename(N_vasc = pt_r3, 
           prop_forest = pt_r,
           id = Animal_ID) %>% 
    group_by(indiv_step_id) %>% 
    filter(max(N_Cladonia) > 0) %>%
    ungroup() %>%
    filter(sl_ > 0)
  
  if(season == "winter"){
    
    DT %<>% filter(doy > 305 | doy < 90)
  # Nov 1st to March 31st
  }
  
  if(season == "summer"){
    
  DT %<>% filter(doy > 120 & doy < 244)
    # May 1st to Aug 31st
  }
  
  return(DT)
	}
