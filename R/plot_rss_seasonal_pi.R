#' @title Plot RSS for both population-level and individual lines by season
#'
plot_rss_seasonal_pi <- function(summer_pop, winter_pop,
                                 summer_indiv, winter_indiv,
                                 theme) {
  
  summer_pop %<>% mutate(season = "summer")
  winter_pop %<>% mutate(season = "winter")
  pop <- rbind(summer_pop, winter_pop)
  
  summer_indiv %<>%
    mutate(season = "summer")
  winter_indiv %<>%
    mutate(season = "winter")
  indiv <- rbind(summer_indiv, winter_indiv) %>%
    mutate(seasonID = paste0(id, season))
  
  
  
  ggplot(data = pop) +
    geom_line(aes(x = x, y = rss, colour = season),
              linewidth = 1) +
    geom_ribbon(aes(x = x, ymin = min_rss, ymax = max_rss, fill = season),
                alpha = 0.5) +
    scale_color_viridis(discrete = "TRUE", option = "D",
                        begin = 0.85, end = 0.2) +
    scale_fill_viridis(discrete = "TRUE", option = "D",
                      begin = 0.85, end = 0.2) +
    geom_hline(
      yintercept = 0,
      colour = "black",
      lty = 2,
      size = .7
    ) +  
    geom_line(data = indiv, aes(x = x, y = rss, 
                                group = seasonID, colour = season),
                   show.legend = F, linetype = "dashed", alpha = 0.6) +
    plot_theme() +
    #	xlim(c(0, 2)) +
    theme(plot.title = element_text(size = 12, hjust = 0.05),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
}
