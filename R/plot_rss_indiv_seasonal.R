#' @title Plot RSS for social interaction using population-level estimates
#'
plot_rss_indiv_seasonal <- function(rss_summer, rss_winter, theme) {
  
  rss_summer %<>% mutate(season = "summer")
  rss_winter %<>% mutate(season = "winter")
  rss <- rbind(rss_summer, rss_winter) %>%
    mutate(seasonID = paste0(id, season))
  
  
  ggplot(data = rss, aes(x = x, y = rss)) +
    geom_line(aes(group = seasonID, colour = season),
              alpha = 0.5,
              linetype = "twodash") +
    scale_color_viridis(discrete = "TRUE", option = "D",
                        begin = 0.85, end = 0.2) +
    geom_smooth(aes(colour = season),
                linewidth = 1.2,
                fill = "darkgrey",
                method = "lm",
                formula = "y ~ x") +
    geom_hline(
      yintercept = 0,
      colour = "black",
      lty = 2,
      linewidth = .7) +
    plot_theme() +
    #	xlim(c(0, 2)) +
    theme(plot.title = element_text(size = 12, hjust = 0.05),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
}
