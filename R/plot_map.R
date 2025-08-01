# make a plot of given study area

plot_map <- function(land, lakes, roads, utm, bb) {
  
  ggplot() +
    geom_sf(data = land, fill = landcol) +
    geom_sf(data = roads, colour = roadcol) +
    geom_sf(data = lakes, fill = watercol) +
    themeMap
    
  
}