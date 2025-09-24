#' @title Average N found vs. individual's mean SRI for that season
#'  @export
#' @author Jack G Hendrix
N_rss_by_SRI <- function(DT, SRI, pred) {

  if(pred == "vasc"){
    DT %<>% filter(x == 4) %>% dplyr::select(c(1,5))
    
    dat <- left_join(SRI, DT, by = c("id")) %>%
      mutate(year = as.factor(yr))
    
    ggplot(dat, aes(x = SRI, y = rss)) +
      geom_point(aes(colour = year),
                 position = position_jitter(),
                 show.legend = TRUE) +
      geom_smooth(method = "lm", colour = "black") +
      scale_colour_viridis(discrete = TRUE) +
      plot_theme() +
      labs(x = "Sociability = Average SRI", y = 'RSS for vascular %N',
           title = 'Selection for summer forage quality by sociality') +
      theme(plot.title = element_text(size = 12, hjust = 0.05),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  }
  
  else{
    DT %<>% filter(x == 1) %>% dplyr::select(c(1,5))
    
    dat <- left_join(SRI, DT, by = c("id")) %>%
      mutate(year = as.factor(yr))
    
    levels(dat$year) <- c("2017", "2018", "2019", "2023", "2024", "2025")
    
    ggplot(dat, aes(x = SRI, y = rss)) +
      geom_point(aes(colour = year),
                 position = position_jitter(width = 0.005),
                 show.legend = TRUE) +
      geom_smooth(method = "lm", colour = "black") +
      scale_colour_viridis(discrete = TRUE) +
      plot_theme() +
      labs(x = "Sociability = Average SRI", y = 'RSS for Cladonia %N',
           title = 'Selection for winter forage quality by sociality') +
    theme(plot.title = element_text(size = 12, hjust = 0.05),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  }
  
		ggsave(
			paste0('graphics/summary/', pred, '_rss_bySRI.png'),
			width = 10,
			height = 10,
			dpi = 320
		)

}
