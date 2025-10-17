#' @title Prepare model data
#' @export
#' @author Jack G Hendrix
prepare_model <- function(DT, season) {

  DT %<>% 
    rename(prop_forest = pt_r,
           id = Animal_ID) %>%
    # Initially: was just removing clusters where every single step was a 0 for cladonia
    # now removing any step where Cladonia is a true zero... which means some clusters are going to have less than 10 avail steps. Gonna assume that's fine, for now, but maybe we should later go on and kick out steps with only 1 or 2 vs. 10 avail?
    # no I stand by this, keep the zeros!
    group_by(indiv_step_id) %>% 
    filter(max(N_Cladonia) > 0) %>%
    ungroup() %>%
    filter(sl_ > 0) %>% 
  # 17 October 2025: restricting down to steps based on our stoich sites only being on the north side of the island
  # anything below this latitude, we don't trust the stoich projections as much
    filter(y1_ > 5503000)
  
  
  if(season == "winter"){
    
    DT %<>% filter(doy < 61)

  }
  
  if(season == "summer"){
    
  DT %<>% filter(doy >= 167 & doy <= 228)
   
  }
  
  return(DT)
	}
