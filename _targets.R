# === Targets: Fogo StDM iSSA ----------------------
# Jack G Hendrix

# Source ------------------------------------------------------------------
targets::tar_source('R')

# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')

# Data --------------------------------------------------------------------
# Caribou data path
gps_path <- file.path('input', 'gps_data.Rds')

# Variables ---------------------------------------------------------------
bb <- c(
  xmin = -54.3533,
  ymin = 49.5194,
  xmax = -53.954220,
  ymax = 49.763834
)
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)


id_col <- 'id'
datetime_col <- 'datetime'
tz <- 'America/St_Johns'

# Split by: within which column or set of columns (eg. c(id, yr))
split_by <- id_col

# Targets: data -----------------------------------------------------------
targets_data <- c(
  
   tar_target(
    tracks_lc,
    readr::read_rds(gps_path)
    ),
   
  #  tar_target(
  #    tracks_lc,
  #    prepare_locs(gps_data, id_col, datetime_col, tz, split_by),
  #   iteration = "group"
  # ),
  # 
  tar_target(
    split_key,
    unique(tracks_lc[, .SD, .SDcols = c(split_by, 'tar_group')])
  )
)
# Targets: seasonal models ----------------------------------------------------------
targets_winter <- c(
  tar_target(
    winter_prep,
    prepare_model(tracks_lc, "winter")
  ),
  tar_target(
    winter_model,
    stdm_model(winter_prep)
  ),
  tar_target(
    winter_model_check,
    model_check(winter_model)
  ),
  
  tar_target(
    summer_prep,
    prepare_model(tracks_lc, "summer")
  ),
  tar_target(
    summer_model,
    stdm_model(summer_prep)
  ),
  tar_target(
    summer_model_check,
    model_check(summer_model)
  ),
  
  # Reference models against which to compare the StDM iSSAs
  tar_target(
    winter_ref_model,
    ref_forest_model(winter_prep)
  ),
  tar_target(
    winter_ref_model_check,
    model_check(winter_ref_model)
  ),
  
  tar_target(
    summer_ref_model,
    ref_forest_model(summer_prep)
  ),
  tar_target(
    summer_ref_model_check,
    model_check(summer_ref_model)
  )
)

# Targets: model output and effects ------------------------------------------------------------
targets_seasonal_effects <- c(
  tar_target(
    indiv_winter,
    indiv_estimates(winter_model)
  ),
  tar_target(
    winter_boxplot,
    plot_box_horiz(indiv_winter, plot_theme())
  ),
  tar_target(
    sum_winter,
    sum_plot(indiv_winter, plot_theme(), "winter")
  ),
  
  tar_target(
    indiv_summer,
    indiv_estimates(summer_model)
  ),
  tar_target(
    summer_boxplot,
    plot_box_horiz(indiv_summer, plot_theme())
  ),
  tar_target(
    sum_summer,
    sum_plot(indiv_summer, plot_theme(), "summer")
  )
)
# Targets: speed from seasonal models ------------------------------------------------------------
targets_speed <- c(
  
  tar_target(
    prep_speed_winter,
    prepare_speed(
      DT = winter_prep,
      summary = indiv_winter,
      params = dist_parameters
    )
  ),
  tar_target(
    calc_speed_forest_w,
    calc_speed(prep_speed_winter, 'forest', seq = 0:1)
  ),
  tar_target(
    plot_speed_forest_w,
    plot_dist(calc_speed_forest_w, plot_theme(), 'forest') +
      labs(x = 'Proportion forested (100 m cell)', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_Cladonia_w,
    calc_speed(prep_speed_winter, 'Cladonia', seq(0, 1, length.out = 100L))
    # highest N_Clad obs is 0.75%, use 1% as upper threshold
  ),
  tar_target(
    plot_speed_Cladonia_w,
    plot_dist(calc_speed_Cladonia_w, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_vasc_w,
    calc_speed(prep_speed_winter, 'vascular', seq(1, 4, length.out = 100L))
    # highest N_vasc obs is 3.9%, use 4% as upper threshold
  ),
  tar_target(
    plot_speed_vasc_w,
    plot_dist(calc_speed_vasc_w, plot_theme()) +
      labs(x = '%N of vascular plants', y = 'Speed (m/hr)')
  ),
  tar_target(
    winter_plots,
    save_plot(plot_speed_forest_w, "winter_model_speed_forest",
              plot_speed_Cladonia_w, "winter_model_speed_Cladonia",
              plot_speed_vasc_w, "winter_model_speed_vascular")
  ),
  
  # summer version of all these now
  tar_target(
    prep_speed_summer,
    prepare_speed(
      DT = summer_prep,
      summary = indiv_summer,
      params = dist_parameters
    )
  ),
  tar_target(
    calc_speed_forest_s,
    calc_speed(prep_speed_summer, 'forest', seq = 0:1)
  ),
  tar_target(
    plot_speed_forest_s,
    plot_dist(calc_speed_forest_s, plot_theme(), 'forest') +
      labs(x = 'Proportion forested (100 m cell)', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_Cladonia_s,
    calc_speed(prep_speed_summer, 'Cladonia', seq(0, 1, length.out = 100L))
    # highest N_Clad obs is 0.75, use 1% as upper threshold
  ),
  tar_target(
    plot_speed_Cladonia_s,
    plot_dist(calc_speed_Cladonia_s, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_vasc_s,
    calc_speed(prep_speed_summer, 'vascular', seq(1, 4, length.out = 100L))
    # highest N_vasc obs is 3.9, use 4% as upper threshold
  ),
  tar_target(
    plot_speed_vasc_s,
    plot_dist(calc_speed_vasc_s, plot_theme()) +
      labs(x = '%N of vascular plants', y = 'Speed (m/hr)')
  ),
  tar_target(
    summer_plots,
    save_plot(plot_speed_forest_s, "summer_model_speed_forest",
              plot_speed_Cladonia_s, "summer_model_speed_Cladonia",
              plot_speed_vasc_s, "summer_model_speed_vascular")
  )
  
)

# Targets: RSS from winter model -----------------------------------------------------------
targets_rss_winter <- c(
  tar_target(
    pred_h1_Cladonia_w,
    predict_h1_Cladonia(winter_prep, winter_model)
  ),
  tar_target(
    pred_h1_vasc_w,
    predict_h1_vasc(winter_prep, winter_model)
  ),
  tar_target(
    pred_h1_forest_w,
    predict_h1_forest(winter_prep, winter_model)
  ),
  tar_target(
    pred_h2_w,
    predict_h2(winter_prep, winter_model)
  ),
  
  tar_target(
    rss_forest_w,
    calc_rss(pred_h1_forest_w, 'h1_forest', pred_h2_w, 'h2')
  ),
  tar_target(
    rss_Cladonia_w,
    calc_rss(pred_h1_Cladonia_w, 'h1_Cladonia', pred_h2_w, 'h2')
  ),
  tar_target(
    rss_vasc_w,
    calc_rss(pred_h1_vasc_w, 'h1_vasc', pred_h2_w, 'h2')
  ),
  
  tar_target(
    plot_rss_forest_w,
    plot_rss(rss_forest_w, plot_theme()) +
      labs(x = 'Proportion forested (100m)', y = 'logRSS',
           title = 'Winter RSS for forest cover compared to mean')
  ),
  tar_target(
    plot_rss_Cladonia_w,
    plot_rss(rss_Cladonia_w, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Winter RSS for Cladonia %N relative to mean')
  ),
  tar_target(
    plot_rss_vasc_w,
    plot_rss(rss_vasc_w, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Winter RSS for vascular plant %N relative to mean')
  ),
  tar_target(
    winter_rss_plots,
    save_rss_plot(plot_rss_forest_w, "rss_forest_winter",
                  plot_rss_Cladonia_w, "rss_Cladonia_winter",
                  plot_rss_vasc_w, "rss_vascular_winter")
  )
)

# Targets: RSS from summer model -----------------------------------------------------------
targets_rss_summer <- c(
  tar_target(
    pred_h1_Cladonia_s,
    predict_h1_Cladonia(summer_prep, summer_model)
  ),
  tar_target(
    pred_h1_vasc_s,
    predict_h1_vasc(summer_prep, summer_model)
  ),
  tar_target(
    pred_h1_forest_s,
    predict_h1_forest(summer_prep, summer_model)
  ),
  tar_target(
    pred_h2_s,
    predict_h2(summer_prep, summer_model)
  ),
  
  tar_target(
    rss_forest_s,
    calc_rss(pred_h1_forest_s, 'h1_forest', pred_h2_s, 'h2')
  ),
  tar_target(
    rss_Cladonia_s,
    calc_rss(pred_h1_Cladonia_s, 'h1_Cladonia', pred_h2_s, 'h2')
  ),
  tar_target(
    rss_vasc_s,
    calc_rss(pred_h1_vasc_s, 'h1_vasc', pred_h2_s, 'h2')
  ),
  
  tar_target(
    plot_rss_forest_s,
    plot_rss(rss_forest_s, plot_theme()) +
      labs(x = 'Proportion forested (100m)', y = 'logRSS',
           title = 'Summer RSS for forest cover compared to mean')
  ),
  tar_target(
    plot_rss_Cladonia_s,
    plot_rss(rss_Cladonia_s, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Summer RSS for Cladonia %N relative to mean')
  ),
  tar_target(
    plot_rss_vasc_s,
    plot_rss(rss_vasc_s, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Summer RSS for vascular plant %N relative to mean')
  ),
  tar_target(
    summer_rss_plots,
    save_rss_plot(plot_rss_forest_s, "rss_forest_summer",
                  plot_rss_Cladonia_s, "rss_Cladonia_summer",
                  plot_rss_vasc_s, "rss_vascular_summer")
  )
)


# Targets: RSS for stoich from base model categorised out by speed ---------------------
targets_rss_by_speed <- c(
  
  # First the winter versions
  tar_target(
    pred_h1_Cladonia_byspeed_w,
    predict_h1_Cladonia_speed(winter_prep, winter_model)
  ),
  tar_target(
    pred_h2_byspeed_w,
    predict_h2_speed(winter_prep, winter_model)
  ),
  
  tar_target(
    rss_Cladonia_byspeed_w,
    calc_rss_speed(pred_h1_Cladonia_byspeed_w, 'h1_Cladonia', pred_h2_byspeed_w, 'h2')
  ),
  tar_target(
    plot_rss_Cladonia_speed_w,
    plot_rss_speed(rss_Cladonia_byspeed_w, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Winter RSS for Cladonia %N by speed')
  ),
  
  tar_target(
    pred_h1_vasc_byspeed_w,
    predict_h1_vasc_speed(winter_prep, winter_model)
  ),
  tar_target(
    rss_vasc_byspeed_w,
    calc_rss_speed(pred_h1_vasc_byspeed_w, 'h1_vasc', pred_h2_byspeed_w, 'h2')
  ),
  tar_target(
    plot_rss_vasc_speed_w,
    plot_rss_speed(rss_vasc_byspeed_w, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Winter RSS for vascular %N by speed')
  ),
  
  tar_target(
    pred_h1_forest_byspeed_w,
    predict_h1_forest_speed(winter_prep, winter_model)
  ),
  tar_target(
    rss_forest_byspeed_w,
    calc_rss_speed(pred_h1_forest_byspeed_w, 'h1_forest', pred_h2_byspeed_w, 'h2')
  ),
  
  tar_target(
    plot_rss_forest_speed_w,
    plot_rss_speed(rss_forest_byspeed_w, plot_theme()) +
      labs(x = 'Proportion forested (100m)', y = 'logRSS',
           title = 'Winter RSS for forest cover by speed')
  ),
  
  tar_target(
    winter_speed_rss_plots,
    save_rss_plot(plot_rss_Cladonia_speed_w, 'by_speed_Cladonia_winter',
                  plot_rss_vasc_speed_w, 'by_speed_vasc_winter',
                  plot_rss_forest_speed_w, 'by_speed_forest_winter'
    )
  ),
  
  # Now the summer versions of the speed-factored RSS ----
  
  tar_target(
    pred_h1_Cladonia_byspeed_s,
    predict_h1_Cladonia_speed(summer_prep, summer_model)
  ),
  tar_target(
    pred_h2_byspeed_s,
    predict_h2_speed(summer_prep, summer_model)
  ),
  tar_target(
    rss_Cladonia_byspeed_s,
    calc_rss_speed(pred_h1_Cladonia_byspeed_s, 'h1_Cladonia', pred_h2_byspeed_s, 'h2')
  ),
  tar_target(
    plot_rss_Cladonia_speed_s,
    plot_rss_speed(rss_Cladonia_byspeed_s, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Summer RSS for Cladonia %N by speed')
  ),
  
  tar_target(
    pred_h1_vasc_byspeed_s,
    predict_h1_vasc_speed(summer_prep, summer_model)
  ),
  tar_target(
    rss_vasc_byspeed_s,
    calc_rss_speed(pred_h1_vasc_byspeed_s, 'h1_vasc', pred_h2_byspeed_s, 'h2')
  ),
  tar_target(
    plot_rss_vasc_speed_s,
    plot_rss_speed(rss_vasc_byspeed_s, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Summer RSS for vascular %N by speed')
  ),
  
  tar_target(
    pred_h1_forest_byspeed_s,
    predict_h1_forest_speed(summer_prep, summer_model)
  ),
  tar_target(
    rss_forest_byspeed_s,
    calc_rss_speed(pred_h1_forest_byspeed_s, 'h1_forest', pred_h2_byspeed_s, 'h2')
  ),
  tar_target(
    plot_rss_forest_speed_s,
    plot_rss_speed(rss_forest_byspeed_s, plot_theme()) +
      labs(x = 'Proportion forested (100m)', y = 'logRSS',
           title = 'Summer RSS for forest cover by speed')
  ),
  
  tar_target(
    summer_speed_rss_plots,
    save_rss_plot(plot_rss_Cladonia_speed_s, 'by_speed_Cladonia_summer',
                  plot_rss_vasc_speed_s, 'by_speed_vasc_summer',
                  plot_rss_forest_speed_s, 'by_speed_forest_summer'
    )
  )
)

# Population-level RSS plots showing 95% confidence interval of estimates -----
targets_rss_popn <- c(
  
  tar_target(
    summer_popn, # population slopes and CIs from model
    setDT(readr::read_csv('output/north_summer_betas.csv', show_col_types = FALSE))
  ),
  tar_target(
    winter_popn,
    setDT(readr::read_csv('output/north_winter_betas.csv', show_col_types = FALSE))
  ),
  
  ## start with winter Cladonia %N, agnostic to speed
  tar_target(
    p_h1_Cladonia_winter,
    predict_h1_Cladonia_popn(winter_prep, winter_popn)
  ),
  tar_target(
    p_h2_winter_clad,
    predict_h2_popn(winter_prep, winter_popn, "Cladonia")
  ),
  
  tar_target(
    min_Clad_winter,
    calc_rss_popn(p_h1_Cladonia_winter, 'h1_Clad_min', p_h2_winter_clad, 'h2_min')
  ),
  tar_target(
    mean_Clad_winter,
    calc_rss_popn(p_h1_Cladonia_winter, 'h1_Clad_mean', p_h2_winter_clad, 'h2_mean')
  ),
  tar_target(
    max_Clad_winter,
    calc_rss_popn(p_h1_Cladonia_winter, 'h1_Clad_max', p_h2_winter_clad, 'h2_max')
  ),
  
  tar_target(
    popn_Cladonia_winter,
    join_rss_popn(min_Clad_winter, mean_Clad_winter, max_Clad_winter)
  ),
  tar_target(
    p_Clad_winter_plot,
    plot_rss_popn(popn_Cladonia_winter, plot_theme()) +
      labs(x = '%N in Cladonia lichen', y = 'logRSS',
           title = 'Population-level RSS for Cladonia %N in winter')),
  
  # and now try vascular in summer
  tar_target(
    p_h1_vasc_summer,
    predict_h1_vasc_popn(summer_prep, summer_popn)
  ),
  tar_target(
    p_h2_summer_vasc,
    predict_h2_popn(summer_prep, summer_popn, "vasc")
  ),
  
  tar_target(
    min_vasc_summer,
    calc_rss_popn(p_h1_vasc_summer, 'h1_vasc_min', p_h2_summer_vasc, 'h2_min')
  ),
  tar_target(
    mean_vasc_summer,
    calc_rss_popn(p_h1_vasc_summer, 'h1_vasc_mean', p_h2_summer_vasc, 'h2_mean')
  ),
  tar_target(
    max_vasc_summer,
    calc_rss_popn(p_h1_vasc_summer, 'h1_vasc_max', p_h2_summer_vasc, 'h2_max')
  ),

  tar_target(
    popn_vasc_summer,
    join_rss_popn(min_vasc_summer, mean_vasc_summer, max_vasc_summer)
  ),
  tar_target(
    p_vasc_summer_plot,
    plot_rss_popn(popn_vasc_summer, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Population-level RSS for vascular %N in summer')),

  # And then the inverses, where we expect a neutral response
  tar_target(
    p_h1_vasc_winter,
    predict_h1_vasc_popn(winter_prep, winter_popn)
  ),
  tar_target(
    p_h2_winter_vasc,
    predict_h2_popn(winter_prep, winter_popn, "vasc")
  ),
  
  tar_target(
    min_vasc_winter,
    calc_rss_popn(p_h1_vasc_winter, 'h1_vasc_min', p_h2_winter_vasc, 'h2_min')
  ),
  tar_target(
    mean_vasc_winter,
    calc_rss_popn(p_h1_vasc_winter, 'h1_vasc_mean', p_h2_winter_vasc, 'h2_mean')
  ),
  tar_target(
    max_vasc_winter,
    calc_rss_popn(p_h1_vasc_winter, 'h1_vasc_max', p_h2_winter_vasc, 'h2_max')
  ),
  
  tar_target(
    popn_vasc_winter,
    join_rss_popn(min_vasc_winter, mean_vasc_winter, max_vasc_winter)
  ),
  tar_target(
    p_vasc_winter_plot,
    plot_rss_popn(popn_vasc_winter, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Population-level RSS for vascular %N in winter')),
  
  # and Cladonia in summer
  
  tar_target(
    p_h1_Cladonia_summer,
    predict_h1_Cladonia_popn(summer_prep, summer_popn)
  ),
  tar_target(
    p_h2_summer_Cladonia,
    predict_h2_popn(summer_prep, summer_popn, "Cladonia")
  ),
  
  tar_target(
    min_Cladonia_summer,
    calc_rss_popn(p_h1_Cladonia_summer, 'h1_Clad_min', p_h2_summer_Cladonia, 'h2_min')
  ),
  tar_target(
    mean_Cladonia_summer,
    calc_rss_popn(p_h1_Cladonia_summer, 'h1_Clad_mean', p_h2_summer_Cladonia, 'h2_mean')
  ),
  tar_target(
    max_Cladonia_summer,
    calc_rss_popn(p_h1_Cladonia_summer, 'h1_Clad_max', p_h2_summer_Cladonia, 'h2_max')
  ),
  
  tar_target(
    popn_Cladonia_summer,
    join_rss_popn(min_Cladonia_summer, mean_Cladonia_summer, max_Cladonia_summer)
  ),
  tar_target(
    p_Cladonia_summer_plot,
    plot_rss_popn(popn_Cladonia_summer, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Population-level RSS for Cladonia %N in summer')),
  
  ## Putting winter + summer for each forage source on the same plot ----
  tar_target(
    indiv_Clad_seasonal,
    plot_rss_indiv_seasonal(rss_Cladonia_s, rss_Cladonia_w, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'RSS for Cladonia %N')),
  
  tar_target(
    indiv_vasc_seasonal,
    plot_rss_indiv_seasonal(rss_vasc_s, rss_vasc_w, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'RSS for vascular %N')),
  
  tar_target(
    seasonal_RSS,
    save_rss_plot(indiv_vasc_seasonal, "seasonal_vasc_rss",
                  indiv_Clad_seasonal, "seasonal_Cladonia_rss")
  ),
  
  tar_target(
    popn_vasc_seasonal,
    plot_rss_popn_seasonal(popn_vasc_summer, popn_vasc_winter, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Population-level RSS for vascular %N')),
  
  tar_target(
    popn_Clad_seasonal,
    plot_rss_popn_seasonal(popn_Cladonia_summer, popn_Cladonia_winter, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Population-level RSS for Cladonia %N')),
  
  tar_target(
    popn_RSS,
    save_rss_plot(popn_vasc_seasonal, "popn/seasonal_vasc_rss",
                  popn_Clad_seasonal, "popn/seasonal_Cladonia_rss")
  ),
  
  # Superimposing individual RSS over the pop'n level responses with 95% CIs
  
  tar_target(
    seasonal_Clad_popn_and_indiv,
    plot_rss_seasonal_pi(popn_Cladonia_summer, popn_Cladonia_winter,
                         rss_Cladonia_s, rss_Cladonia_w,
                         plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Population and individual RSS for Cladonia %N')),
  
  
  tar_target(
    seasonal_vasc_popn_and_indiv,
    plot_rss_seasonal_pi(popn_vasc_summer, popn_vasc_winter,
                         rss_vasc_s, rss_vasc_w,
                         plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Population and individual RSS for vascular %N')),
  
  tar_target(
    pi_seasonal_RSS,
    save_rss_plot(seasonal_Clad_popn_and_indiv, "popn/Clad_pop-and-indiv",
                  seasonal_vasc_popn_and_indiv, "popn/vasc_pop-and-indiv")
  ),
  
  
  
  # Now, combine both lichen + vascular within season, to have a summer plot and a winter plot
  tar_target(
    popn_summer_comp,
    plot_rss_popn_merge_seasons(popn_Cladonia_summer, popn_vasc_summer, plot_theme()) +
      labs(x = '%N in forage', y = 'logRSS',
           title = 'Population-level RSS for %N in summer')),
  
  tar_target(
    popn_winter_comp,
    plot_rss_popn_merge_seasons(popn_Cladonia_winter, popn_vasc_winter, plot_theme()) +
      labs(x = '%N in forage', y = 'logRSS',
           title = 'Population-level RSS for %N in winter')),
  
  tar_target(
    popn_seasonal_RSS,
    save_rss_plot(popn_winter_comp, "popn/seasonal_rss_winter",
                  popn_summer_comp, "popn/seasonal_rss_summer")
  )
)

# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
