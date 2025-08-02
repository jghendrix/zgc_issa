remove_NA_steps <- function(DT) {

  # there are steps where all the theoretical steps are NAs b/c they go into the ocean
  # but the real ones are still in the dataset
  # we have to remove all the steps from those clusters
  
  nas <- DT %>% 
    filter(is.na(x2_)) %>%
    mutate(animal_step = paste0(Animal_ID, "_", step_id_)) %>%
    filter(!duplicated(animal_step))
	
  DT %<>%  
    mutate(indiv_step_id = paste0(Animal_ID, "_", step_id_)) %>%
    filter(!indiv_step_id %in% c(nas$animal_step))

}
