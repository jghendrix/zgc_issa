# === Targets: Fogo stoich mapping ----------------------
# Jack G Hendrix
# 24 February 2025

# Source ------------------------------------------------------------------
targets::tar_source('R')

# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')

# Data --------------------------------------------------------------------
# Path to land cover rasters
# lc = Canadian Forest service, sdss = provincial product
#cfs_path <- file.path('input', 'cfs.tif')
sdss_path <- file.path('input', 'sdss.tif')
#legend_path <- file.path('input', 'cfs_legend.csv')
sdss_legend_path <- file.path('input', 'legend_sdss.csv')

# Stoich layer paths
lichen_path <- file.path('input', 'Cladonia_N.tif')
vascular_path <- file.path('input', 'Vascular_N.tif')

lichen_ratio_path <- file.path('input', 'Cladonia_CN.tif')
vascular_ratio_path <- file.path('input', 'Vascular_CN.tif')

# Caribou data path
gps_path <- file.path('input', 'all_Fogo_caribou_locs_20June2025.csv')
id_path <- file.path('input', 'collar-animal-ids.csv')

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


id_col <- 'Animal_ID'
datetime_col <- 'datetime'
x_col <- 'x_long'
y_col <- 'y_lat'
tz <- 'America/St_Johns'

# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
split_by <- id_col
seasonal_split <- "season"

# For iSSA tracks:
# Resampling rate
rate <- minutes(120)
# Tolerance
tolerance <- minutes(5)
# Number of random steps
n_random_steps <- 10


# Targets: data -----------------------------------------------------------
targets_data <- c(
  
   tar_file_read(
    caribou,
    gps_path,
    fread(!!.x)
  ),
  tar_file_read(
    ids,
    id_path,
    fread(!!.x)
  ),
  
 # tar_file_read(
#    cfs,
 #   cfs_path,
  #  raster(!!.x)
  #),
#  tar_file_read(
#    cfs_legend,
#    legend_path,
#    fread(!!.x)
#  ),
  
  tar_file_read(
    sdss,
    sdss_path,
    raster(!!.x)
  ),
  tar_file_read(
    sdss_legend,
    sdss_legend_path,
    fread(!!.x)
  ),
  
  tar_file_read(
    lichen,
    lichen_path,
    raster(!!.x)
  ),
  tar_file_read(
    vascular,
    vascular_path,
    raster(!!.x)
  ),

  tar_file_read(
    lichen_ratio,
    lichen_ratio_path,
    raster(!!.x)
  ),
  tar_file_read(
    vascular_ratio,
    vascular_ratio_path,
    raster(!!.x)
),
  tar_target(
    coast,
    get_coastline(bb, crs)
  ),
  
  tar_target(
    roads,
    get_roads(bb, crs)
  ),
  tar_target(
    water,
    get_lakes(bb, crs)
  )
  
)


# Targets: preparing caribou data ---------------------------
targets_prep <- c(
  
  tar_target(
    tidy_GPS,
    GPS_tidying(caribou, ids)
  ),
  tar_target(
    locs_prep,
    prepare_locs(tidy_GPS, id_col, datetime_col, tz, x_col, y_col, split_by),
    iteration = 'group'
  ),
  tar_target(
    split_key,
    unique(locs_prep[, .SD, .SDcols = c(split_by, 'tar_group')])
  )
  
)

# Targets: tracks ---------------------------------------------------------
targets_tracks <- c(
  tar_target(
    tracks,
    make_track(locs_prep, x_, y_, t_, all_cols = TRUE, crs = 4326) |>
      transform_coords(crs_to = crs),
    pattern = map(locs_prep)
  ),
  tar_target(
    tracks_resampled,
    resample_tracks(tracks, rate = rate, tolerance = tolerance),
    pattern = map(tracks)
  ),
  tar_target(
    tracks_random,
    random_steps(tracks_resampled, n = n_random_steps),
    pattern = map(tracks_resampled)
  )
)
## Extracting data for tracks ------------------------------

targets_extract <- c(
  
  tar_target(
    tracks_cleaned,
    remove_NA_steps(tracks_random)
    )
  ,
  
  tar_target(
    tracks_extract,
    extract_layers(
      tracks_cleaned,
      crs,
      sdss,
      sdss_legend
    )
  ),
  
  tar_target(
    tracks_lichen,
    extract_stoich(
      tracks_extract,
      crs,
      lichen,
      "N_Cladonia",
      6 # 6 = the interval by which to average out the 5m x 5m pixels, this takes the average stoich at 30m
      # calling all NAs zero
    )
  ),
  
  # less than 10% of clusters have a lichen value for each cell. lots of NAs... do we consider NA for lichen to be a zero?
  
  tar_target(
   tracks_lichen_fixed,
    tracks_lichen %>% 
    mutate(N_Cladonia = ifelse(is.na(N_Cladonia), 0, N_Cladonia))
  ),
  
  tar_target(
    tracks_vasc,
    extract_stoich(
      tracks_lichen_fixed,
      crs,
      vascular,
      "N_vasc",
      6
    )
  ),
  
  ## Lots of NAs in the stoich maps. Maybe using a radius around the point instead of that specific 5m cell would be better?
    
  # drawing the radius would take for bloody ever, like 70 DAYS of computation
  
  # aggregated the stoich maps to larger pixels instead, much faster,

  tar_target(
    tracks_lc,
    extract_forest(
      tracks_vasc,
      crs,
      sdss, # this version of sdss is actually also down to 5m cells, same as the stoich layers... so if we x3, we aren't getting 30 -> 90, it's only doing a 15 x 15m pixel for forest, in fact. yikes. If we make this 20 it'll be a 100x100 cell, aka a 50ish m radius
      20
    )
  ),
  
  tar_target(
    tracks_clad_ratio,
    extract_stoich(
      tracks_lc,
      crs,
      lichen_ratio,
      "CN_Cladonia",
      6
    )
  ),
  
  tar_target(
    tracks_vasc_ratio,
    extract_stoich(
      tracks_clad_ratio,
      crs,
      vascular_ratio,
      "CN_vasc",
      6
    )
  )

)

# Targets: distributions --------------------------------------------------
targets_distributions <- c(
  tar_target(
    dist_parameters,
    calc_distribution_parameters(tracks_random),
    pattern = map(tracks_random)
  ),
  tar_target(
    dist_sl_plots,
    plot_distributions(tracks_resampled, 'sl_'),
    pattern = map(tracks_resampled),
    iteration = 'list'
  ),
  #FO2023012 has a somewhatwonky bimodal distribution of step length, everyone else looks normal
  
  tar_target(
    dist_ta_plots,
    plot_distributions(tracks_resampled, 'ta_'),
    pattern = map(tracks_resampled),
    iteration = 'list'
  )
  # 2017013 has higher peaks at -3 and +3
  # 2017004 is basically flat
  # 2013012 is weird again
)

# Targets: seasonal models ----------------------------------------------------------
targets_winter <- c(
  tar_target(
    winter_prep,
    prepare_model(tracks_vasc_ratio, "winter")
  ),
  
  ## Le Goff and Peignier both call winter 1 Jan - 16 March
  
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
    prepare_model(tracks_vasc_ratio, "summer")
  ),
  
  # Peignier calls summer 15 July - 1 Sept (with calving starting late May?)
  tar_target(
    summer_model,
    stdm_model(summer_prep)
  ),
  tar_target(
    summer_model_check,
    model_check(summer_model)
  ),
  
  # Ratio models?
  tar_target(
    winter_ratio,
    ratio_model(winter_prep)
  ),
  # winter model is giving NA for step length output... cannot trust this model
  
  tar_target(
    summer_ratio,
    ratio_model(summer_prep)
  )
  # summer appears to be fine though?
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
  ),
  
  tar_target(
    indiv_summer_ratio,
    indiv_ratio_estimates(summer_ratio)
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
      labs(x = 'Proportion forested (90 m cell)', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_Cladonia_w,
    calc_speed(prep_speed_winter, 'Cladonia', seq(0, 1, length.out = 100L))
    # highest N_Clad obs is 0.75, let's use 1% as upper threshold
  ),
  tar_target(
    plot_speed_Cladonia_w,
    plot_dist(calc_speed_Cladonia_w, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_vasc_w,
    calc_speed(prep_speed_winter, 'vascular', seq(1, 4, length.out = 100L))
    # highest N_vasc obs is 3.9, let's use 4% as upper threshold
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
      labs(x = 'Proportion forested (90 m cell)', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_Cladonia_s,
    calc_speed(prep_speed_summer, 'Cladonia', seq(0, 1, length.out = 100L))
    # highest N_Clad obs is 0.75, let's use 1% as upper threshold
  ),
  tar_target(
    plot_speed_Cladonia_s,
    plot_dist(calc_speed_Cladonia_s, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'Speed (m/hr)')
  ),
  tar_target(
    calc_speed_vasc_s,
    calc_speed(prep_speed_summer, 'vascular', seq(1, 4, length.out = 100L))
    # highest N_vasc obs is 3.9, let's use 4% as upper threshold
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
#  tar_target(
 #   pred_h2_fire_forest,
#    predict_h2_forest(model_prep, fire_model, "fire")
#  ),
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
           title = 'Winter - RSS compared to mean proportion forest')
  ),
  tar_target(
    plot_rss_Cladonia_w,
    plot_rss(rss_Cladonia_w, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Winter - RSS for Cladonia %N relative to mean')
  ),
  tar_target(
    plot_rss_vasc_w,
    plot_rss(rss_vasc_w, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Winter - RSS for vascular plant %N relative to mean')
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
  #  tar_target(
  #   pred_h2_fire_forest,
  #    predict_h2_forest(model_prep, fire_model, "fire")
  #  ),
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
           title = 'summer - RSS compared to mean proportion forest')
  ),
  tar_target(
    plot_rss_Cladonia_s,
    plot_rss(rss_Cladonia_s, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'summer - RSS for Cladonia %N relative to mean')
  ),
  tar_target(
    plot_rss_vasc_s,
    plot_rss(rss_vasc_s, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'summer - RSS for vascular plant %N relative to mean')
  ),
  tar_target(
    summer_rss_plots,
    save_rss_plot(plot_rss_forest_s, "rss_forest_summer",
                  plot_rss_Cladonia_s, "rss_Cladonia_summer",
                  plot_rss_vasc_s, "rss_vascular_summer")
  )
)


# Targets: RSS from summer RATIO model -----------------------------------------------------------
targets_rss_summer_ratio <- c(
  tar_target(
    pred_h1_Cladonia_r,
    predict_h1_Cladonia_ratio(summer_prep, summer_ratio)
  ),
  
  tar_target(
    pred_h1_vasc_r,
    predict_h1_vasc_ratio(summer_prep, summer_ratio)
  ),
  
  tar_target(
    pred_h1_forest_r,
    predict_h1_forest_ratio(summer_prep, summer_ratio)
),

  tar_target(
    pred_h2_r,
    predict_h2_ratio(summer_prep, summer_ratio)
  ),

  tar_target(
    rss_forest_r,
    calc_rss(pred_h1_forest_r, 'h1_forest', pred_h2_r, 'h2')
  ),
  tar_target(
    rss_Cladonia_r,
    calc_rss(pred_h1_Cladonia_r, 'h1_Cladonia', pred_h2_r, 'h2')
  ),
  tar_target(
    rss_vasc_r,
    calc_rss(pred_h1_vasc_r, 'h1_vasc', pred_h2_r, 'h2')
  ),
  tar_target(
    plot_rss_forest_r,
    plot_rss(rss_forest_r, plot_theme()) +
      labs(x = 'Proportion forested (100m)', y = 'logRSS',
           title = 'summer - RSS compared to mean proportion forest')
  ),
  tar_target(
    plot_rss_Cladonia_r,
    plot_rss(rss_Cladonia_r, plot_theme()) +
      labs(x = 'C:N in Cladonia', y = 'logRSS',
           title = 'summer - RSS for Cladonia C:N relative to mean')
  ),
  tar_target(
    plot_rss_vasc_r,
    plot_rss(rss_vasc_r, plot_theme()) +
      labs(x = 'C:N in vascular plants', y = 'logRSS',
           title = 'summer - RSS for vascular plant C:N relative to mean')
  ),
  tar_target(
    summer_ratio_plots,
    save_rss_plot(plot_rss_forest_r, "rss_forest_ratio",
                  plot_rss_Cladonia_r, "rss_Cladonia_ratio",
                  plot_rss_vasc_r, "rss_vascular_ratio")
  )
)

# Selection for ratio over summer: seems like they avoid 'high-quality' Cladonia, which aligns with the %N summer model. But they also select HIGHER C:N ratio during summer, at the same time they're selecting higher %N? How is this possible?


# Targets: seasonal models with ta_ interaction ----------------------------------------------------------

# turning angle models have been added to example_targets.R in template code folder
# Targets: RSS for stoich categorised out by speed ----------------------------------

targets_rss_by_speed <- c(
  
  # First from the winter versions of these models -----
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
           title = 'winter - RSS for Cladonia %N by speed')
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
           title = 'winter - RSS for vascular %N by speed')
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
      labs(x = '% forest cover (100m)', y = 'logRSS',
           title = 'winter - RSS for forest cover')
  ),
  
  
  tar_target(
    winter_speed_rss_plots,
    save_rss_plot(plot_rss_Cladonia_speed_w, 'by_speed_Cladonia_winter',
                  plot_rss_vasc_speed_w, 'by_speed_vasc_winter',
                  plot_rss_forest_speed_w, 'by_speed_forest_winter'
                  )
  ),
  
  # Now the summer version of the speed-factored RSS ----
  
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
           title = 'summer - RSS for Cladonia %N by speed')
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
           title = 'summer - RSS for vascular %N by speed')
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
      labs(x = '% forest cover (100m)', y = 'logRSS',
           title = 'summer - RSS for forest cover')
  ),
  
  tar_target(
    summer_speed_rss_plots,
    save_rss_plot(plot_rss_Cladonia_speed_s, 'by_speed_Cladonia_summer',
                  plot_rss_vasc_speed_s, 'by_speed_vasc_summer',
                  plot_rss_forest_speed_s, 'by_speed_forest_summer'
    )
  )
  
)

## Population-level RSS plots showing standard error of estimates -----
targets_rss_social_popn <- c(
  tar_target(
    summer_popn,
    setDT(readr::read_csv('output/summer_betas.csv', show_col_types = FALSE))
  ),
  tar_target(
    winter_popn,
    setDT(readr::read_csv('output/winter_betas.csv', show_col_types = FALSE))
  ),

    ## Let's start with winter Cladonia, agnostic to speed
  
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
  
  # and now try vascular in summer?
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
  
  # how about the alternate where we'd expect a neutral response?
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
  
  # and Cladonia in summer for completion sake
  
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
  
  # Putting winter + summer for each stoich on the same plot
  tar_target(
    popn_vasc_seasonal,
    plot_rss_popn_seasonal(popn_vasc_summer, popn_vasc_winter, plot_theme()) +
      labs(x = '%N in vascular plants', y = 'logRSS',
           title = 'Population-level RSS for vascular %N')),
  
  tar_target(
    popn_Clad_seasonal,
    plot_rss_popn_seasonal(popn_Cladonia_summer, popn_Cladonia_winter, plot_theme()) +
      labs(x = '%N in Cladonia', y = 'logRSS',
           title = 'Population-level RSS for Cladonia %N'))
  
  
)

## Average N experienced by indiv as a function of... sociality? -----

targets_average_N <- c(
  
  tar_target(
    summer_N_avg,
    indiv_sum(summer_prep, 'vasc', plot_theme())
  )
  
  
)
# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
