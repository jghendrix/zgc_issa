p_Clad_s <- popn_Cladonia_summer %>%
  mutate(season = "summer")
p_Clad_w <- popn_Cladonia_winter %>%
  mutate(season = "winter")

p_Clad <- rbind(p_Clad_s, p_Clad_w)
i_Clad <- rbind(i_Clad_s, i_Clad_w) %>%
  mutate(seasonID = paste0(id, season))


# Cladonia RSS, pop'n w/ 95%CI, overlaid with individual responses
ggplot(data = p_Clad) +
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
    linewidth = .7
  ) +
  geom_line(data = i_Clad, aes(x = x, y = rss, group = seasonID, colour = season),
            show.legend = F, linetype = "dashed", alpha = 0.4)
  
  
  
  
  
  plot_theme() +
  #	xlim(c(0, 2)) +
  theme(plot.title = element_text(size = 12, hjust = 0.05),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))