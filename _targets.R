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
      6 # 6 = the interval by which to average out the 5m x 5m pixels, this takes the average stoich at 30m
      # calling all NAs zero
    )
  ),
  
  # less than 10% of clusters have a lichen value for each cell. lots of NAs... do we consider NA for lichen to be a zero?
  
  tar_target(
    tracks_lichen_fixed,
    tracks_lichen %>% 
      rename(N_Cladonia = pt_r3) %>%
      mutate(N_Cladonia = ifelse(is.na(N_Cladonia), 0, N_Cladonia))
  ),
  
  tar_target(
    tracks_vasc,
    extract_stoich(
      tracks_lichen_fixed,
      crs,
      vascular,
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
      sdss,
      3
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
  tar_target(
    dist_ta_plots,
    plot_distributions(tracks_resampled, 'ta_'),
    pattern = map(tracks_resampled),
    iteration = 'list'
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

# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
