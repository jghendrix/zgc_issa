#' @title Plot RSS for social interaction using population-level estimates
#'
plot_rss_popn <- function(rss, theme) {
  
  
  ggplot(data = rss) +
    geom_line(aes(x = x, y = rss),
              linewidth = 1, colour = "darkgreen") +
    geom_ribbon(aes(x = x, ymin = min_rss, ymax = max_rss),
                alpha = 0.5, fill = "palegreen") +
   # scale_color_viridis(discrete = "TRUE", option = "D",
    #                    begin = 0.85, end = 0.2) +
    #scale_fill_viridis(discrete = "TRUE", option = "D",
     #                  begin = 0.85, end = 0.2) +
    geom_hline(
      yintercept = 0,
      colour = "black",
      lty = 2,
      size = .7
    ) +
    plot_theme() +
    #	xlim(c(0, 2)) +
    theme(plot.title = element_text(size = 12, hjust = 0.05),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
}
