#' @title Average N found vs. individual's mean SRI for that season
#'  @export
#' @author Jack G Hendrix
N_by_SRI <- function(DT, SRI, pred) {

  SRI %<>%
    rename(consp = n)
  
  dat <- left_join(DT, SRI, by = c("id", "yr"))
                   
                   
  if(pred == "vasc"){
    
    ggplot(dat, aes(x = SRI, y = mean, 
                    ymin = mean - se, ymax = mean + se)) +
      geom_pointrange(show.legend = FALSE) +
      geom_smooth(method = "lm") +
      scale_colour_viridis(discrete = TRUE) +
      plot_theme() +
      labs(x = "Sociability = Average SRI", y = 'Vascular %N',
           title = 'Summer forage quality by sociality') +
      theme(plot.title = element_text(size = 12, hjust = 0.05),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  }
  
  else{
    ggplot(dat, aes(x = SRI, y = mean, 
                        ymin = mean - se, ymax = mean + se)) +
      geom_pointrange(show.legend = FALSE) +
      geom_smooth(method = "lm") +
      scale_colour_viridis(discrete = TRUE) +
      plot_theme() +
      labs(x = "Sociability = Average SRI", y = 'Cladonia %N',
           title = 'Winter forage quality by sociality') +
    theme(plot.title = element_text(size = 12, hjust = 0.05),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  }
  
		ggsave(
			paste0('graphics/summary/', pred, '_bySRI.png'),
			width = 10,
			height = 10,
			dpi = 320
		)

}
