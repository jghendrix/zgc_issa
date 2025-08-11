save_plot <- function(plot_a, name_a, plot_b, name_b, plot_c, name_c)

{

	ggsave(paste0("graphics/speed/", name_a, ".png"),
				 plot = plot_a,
				 width = 4,
				 height = 4,
				 dpi = 320)#,
#				 units="px")

	ggsave(paste0("graphics/speed/", name_b, ".png"),
				 plot = plot_b,
				 width = 4,
				 height = 4,
				 dpi = 320)#,units="px")

	ggsave(paste0("graphics/speed/", name_c, ".png"),
				 plot = plot_c,
				 width = 4,
				 height = 4,
				 dpi = 320)#,units = "px")
}
