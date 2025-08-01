## Tidying up GPS collar data

GPS_tidying <- function(DT, ids) {

  # some ids here are collar IDs, not animal IDs, need to translate those across
  
  # two of the LiteTrack collars have been deployed on two different animals, but their first deployments were less than a month long
  # for id == 88164 | id == 88169, if date < 2023-04-10, remove those data
  
  ids$id <- as.character(ids$id)
  df <- left_join(DT, ids, by = "id") %>%
    mutate(Animal_ID = ifelse(is.na(Animal_ID), id, Animal_ID))
  
  omits <- df %>% filter(id %in% c("88164", "88169") & idate < "2023-04-10")
  
  df %<>% filter(!V1 %in% c(omits$V1)) %>%
    dplyr::select(-c(1:2)) %>%
    mutate(idate = lubridate::as_date(idate))
  
}
