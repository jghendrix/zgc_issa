targets_rss_social_popn <- c(
  tar_target(
    summer_popn,
    setDT(readr::read_csv('summer_betas.csv', show_col_types = FALSE))
  ),
  tar_target(
    winter_popn,
    setDT(readr::read_csv('winter_betas.csv', show_col_types = FALSE))
  ),
  
  # What responses do we want the error bar for?
  ## Let's start with winter Cladonia
  
  tar_target(
    p_h1_Cladonia_winter,
    predict_h1_Cladonia_popn(winter_prep, winter_popn)
  ),

    tar_target(
    p_h2_winter,
    predict_h2_popn(winter_prep, winter_popn)
  ),
  
  tar_target(
    min_Clad_winter,
    calc_rss_popn(p_h1_Cladonia_winter, 'h1_Clad_min', p_h2_winter, 'h2_min')
  ),
  
  tar_target(
    mean_Clad_winter,
    calc_rss_popn(p_h1_Cladonia_winter, 'h1_Clad_mean', p_h2_winter, 'h2_mean')
  ),
  
  tar_target(
    max_Clad_winter,
    calc_rss_popn(p_h1_Cladonia_winter, 'h1_Clad_max', p_h2_winter, 'h2_max')
  ),
  
  tar_target(
    popn_Cladonia_winter,
    join_rss_popn(min_Clad_winter, mean_Clad_winter, max_Clad_winter)
  ),
  
  tar_target(
    p_Clad_winter_plot,
    plot_rss_popn(popn_Cladonia_winter, plot_theme()) +
      labs(x = '%N in Cladonia lichen', y = 'logRSS',
           title = 'Population-level RSS for Cladonia %N'))

)