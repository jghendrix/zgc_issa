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
    filter(sl_ > 0)
  
  ## 12 September: these date ranges are all wrong?? WTF?
  
  if(season == "winter"){
    
    DT %<>% filter(doy < 75 & doy > 14)
  # winter = Jan 15th to March 3rd (Peignier et al 2019)
  }
  
  if(season == "summer"){
    
  DT %<>% filter(doy > 181 & doy < 245)
    # July 1st to Aug 31st?
    # spring = 1 May - 19 June, summer = 15 July - 1 September
  }
  
  return(DT)
	}
