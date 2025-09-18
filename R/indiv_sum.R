#' @title Average N experienced by an individual in a given year
#'  @export
#' @author Jack G Hendrix
indiv_sum <- function(DT, pred, theme) {

  DT %<>% filter(case_ == TRUE) # first remove all the random steps to only get what they actually experienced
  
  if(pred == "vasc"){
  
  byindiv <- DT %>% group_by(id, yr) %>% 
    summarise(mean = mean(N_vasc, na.rm = T), 
              sd = sd(N_vasc, na.rm = T), 
              n = n(), 
              se = sd/sqrt(n))
  }
  
  else{
    byindiv <- DT %>% group_by(id, yr) %>% 
      summarise(mean = mean(N_Cladonia, na.rm = T), 
                sd = sd(N_Cladonia, na.rm = T), 
                n = n(), 
                se = sd/sqrt(n))
  }
  
  # gives an individual-year average N for the predictor listed in the argument
  
  # some of these individuals barely have any steps in a given season... if it's less than 100, omit them from this. Feels like a reasonable cutoff, and also natural break point between n = 58 and 154
  
  byindiv %<>% filter(n > 100)
  
  ggplot(byindiv, aes(x = yr, y = mean, 
                      ymin = mean - se, ymax = mean + se, 
                      colour = id)) +
    geom_pointrange(show.legend = FALSE) +
    scale_colour_viridis(discrete = TRUE)
    plot_theme()
  
		ggsave(
			paste0('graphics/summary/', pred, 'average.png'),
			width = 10,
			height = 10,
			dpi = 320
		)

		return(byindiv)
}
