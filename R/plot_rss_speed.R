#' @title Plot RSS by speed
#' @export

plot_rss_speed <- function(rss, theme) {

#if(season == "winter"){
#	rss %<>% mutate(idspd = paste0(id, "_", speed)) %>%
#    mutate(`Speed percentile` = ifelse(speed == "10th", "10th (4 m/h)", "50th (64 m/h)"),
#           `Speed percentile` = ifelse(speed == "90th", "90th (312 m/h)", `Speed percentile`))
#}
#  else{
#    rss %<>% mutate(idspd = paste0(id, "_", speed)) %>%
#      mutate(`Speed percentile` = ifelse(speed == "10th", "10th (4 m/h)", "50th (62 m/h)"),
#             `Speed percentile` = ifelse(speed == "90th", "90th (301 m/h)", `Speed percentile`))
    
 # }

  # We're not gonna worry about including the actual speeds in the legend, just leave the percentages
  rss %<>% mutate(idspd = paste0(id, '_', speed))
  
ggplot(data = rss, aes(x = x, y = rss, colour = speed)) +
		geom_line(aes(group = idspd),
		          alpha = 0.25,
							linewidth = 0.5,
							linetype = "dashed",
							show.legend = F
		) +
		scale_color_viridis(discrete = "TRUE", option = "D", begin = 0.85, end = 0.2) +
		geom_smooth(linewidth = 1.2, method = "lm",
								fill = "darkgrey",
								formula = "y ~ x") +
		geom_hline(
			yintercept = 0,
			colour = "black",
			lty = 2,
			linewidth = .7
		) +
		plot_theme() +
	#	ylim(c(-10, 3)) +
		theme(plot.title = element_text(size = 12, hjust = 0.05),
					axis.title = element_text(size = 12),
					axis.text = element_text(size = 10))
	
	#ggsave(filename = paste0('graphics/rss/byspeed_winterCladonia.png'),
	#       width = 2000,
	#       height = 1500,
	#       dpi = 320,
	#       units = "px"
	#)

}