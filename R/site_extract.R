# Getting the unique points for which we need to extract caribou intensity of use
site_extract <- function(DT) {

 
  DT %<>% filter(species == "Cladonia") %>%
    dplyr::select(c(site, x, y, Lat, Long, dist_to_coast)) %>%
    filter(!duplicated(paste0(x,y)))
  
}