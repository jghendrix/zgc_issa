## Tidying up data post-extraction of landscape covariates

prepare_data <- function(DT, raster) {

  raster %<>% 
    rename(ndvi = 9,
           dem = 10,
           slope = 11,
           aspect = 12,
           TPI = 13,
           terrain = 14) #%>%
    #mutate(ndvi = ndvi/10000#,
           # Merge rock/rubble into barrens
           #pt_lc = ifelse(pt_lc == "32", "33", pt_lc),
           #lc_description = ifelse(pt_lc == "32" | pt_lc == "33",
            #                       "Barrens", lc_description)
           #)
  
  DT1 <- left_join(DT, raster, by = c("x", "y"))
  
  DT1 %<>% rename(dist_to_road = dist_to_feat_line) %>%
    mutate(NC_ratio = ifelse(NC_ratio > 1, NC_ratio, (NC_ratio)^-1)) %>%
  # realized my NC and Dara's CN were getting mixed up
    dplyr::rename(CN_ratio = NC_ratio) %>%
    mutate(site = as.factor(site),
           sdss_lc = as.factor(sdss_lc),
           cfs_lc = as.factor(cfs_lc),
           species = as.factor(species))
  
  DT1 %<>%
  group_by(species) %>%
    mutate(aspect = ifelse(is.na(aspect), mean(aspect, na.rm = T), aspect),
           date = lubridate::as_date(date))
  
  
  
  return(DT1)  
  
}
