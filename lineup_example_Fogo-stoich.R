library(amt)
library(tidyverse)
library(terra)
library(patchwork)

# This simulation system is built with amt... my model is glmmTMB

# amt has a make_issf_model() function
# you have to manually define the coefficients as a vector
# let's focus on the winter %N model for now?
summary(winter_model)

# ignoring the individual random effects, I suppose?
coefs <- c("log(sl_)" = 0.096163,
           "cos(ta_)" = 0.004050,
           "prop_forest" = 0.120643,
           "N_Cladonia" = -0.863676,
           "N_vasc" = 0.440493,
           "log(sl_):prop_forest" = -0.236011,
           "log(sl_):N_Cladonia" = 0.334932,
           "log(sl_):N_vasc" = -0.89289
           )
# what were the parameters of the distribution we drew from?
# each individual got its own distributions, but if we average those to the population
# kappa 0.1634281    
# shape 0.6341374   
# scale 378.2702

wint <- amt::make_issf_model(coefs = coefs,
                     sl = make_gamma_distr(shape = 0.6341374, scale = 378.2702),
                     ta = make_vonmises_distr(kappa = 0.1634281)
                     )
                      
rdk <- redistribution_kernel(wint, 
                             start = start, # what is this???
                             map = r, # the predictive surface on which we're modelling
                             as.rast= FALSE, # why??
                             landscape= "continuous")

# We need a single stacked raster with all of the predictors in it... easy enough for lichen + vascular stoich, but what about prop forest?
# we subsampled that out into a raster to extract from in the targets workflow, can we just pull that one in?

# .. Simulations ------------
# See here for how to simulate with amt: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14263
# And an application with lineup plots: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14336

# The idea is to first fit a SSF and then use this model to simulate trajectories.


# . Examples in R ----------
# .. Load data -------------
# We will use some tracking data from a red deer from Germany that is shipped with
# the amt package

data("deer")
forest <- get_sh_forest()

deer <- deer |>
  slice_head(n = 120)  # Lets only take first 120 observations

# Observed mean step length for day and night
p.truth <- deer |>
  steps_by_burst() |>
  extract_covariates(forest, where = "both") |>
  time_of_day(where = "both") |>
  ggplot(aes(tod_start_, sl_)) + geom_boxplot()

p.truth

# .. Fit SSF ----------------
# Prepare data
deer.rs <- deer |> steps_by_burst() |>
  random_steps() |>
  time_of_day(where = "both")


# ... Null model ------------
m0 <- deer.rs |> fit_clogit(case_ ~ sl_ + strata(step_id_))
summary(m0)

rdk <- amt::redistribution_kernel(
  x = m0,
  start = make_start(deer[1, ], dt = hours(6)),
  map = forest,
  n.control = 1e3,
  tolerance.outside = 0.5
)

r1 <- simulate_path(rdk, n.steps = 50)

r1 |> make_track(x_, y_, t_, crs = get_crs(deer)) |>
  steps() |>
  time_of_day(where = "both") |>
  ggplot(aes(tod_start_, sl_)) + geom_boxplot() +
  scale_y_continuous(limits = c(0, 2000))


# Now lets replicate this 19 times
set.seed(123)
plots <- replicate(19, {
  simulate_path(rdk, n.steps = 50) |>
    make_track(x_, y_, t_, crs = get_crs(deer)) |>
    steps() |>
    time_of_day(where = "both") |>
    ggplot(aes(tod_start_, sl_)) + geom_boxplot() +
    scale_y_continuous(limits = c(0, 2000))

}, simplify = FALSE
)

plots <- c(plots, list(p.truth))
wrap_plots(plots)

# Lets shuffle
plots <- sample(plots)
wrap_plots(plots)


# ... Day night model -------------

m1 <- deer.rs |> fit_clogit(case_ ~ sl_ + sl_:tod_end_ + strata(step_id_))
summary(m1)

rdk <- amt::redistribution_kernel(
  x = m1,
  start = make_start(deer[1, ], dt = hours(6)),
  fun = function(xy, map) {
    extract_covariates(xy, map, where = "both") |>
      time_of_day(where = "both")
  },
  map = forest,
  n.control = 1e3,
  tolerance.outside = 0.5
)

r1 <- simulate_path(rdk, n.steps = 50)

r1 |> make_track(x_, y_, t_, crs = get_crs(deer)) |>
  steps() |>
  time_of_day(where = "both") |>
  ggplot(aes(tod_start_, sl_)) + geom_boxplot() +
  scale_y_continuous(limits = c(0, 2000))


# Now lets replicate this 19 times
set.seed(123)
plots <- replicate(19, {
  simulate_path(rdk, n.steps = 50) |>
    make_track(x_, y_, t_, crs = get_crs(deer)) |>
    steps() |>
    time_of_day(where = "both") |>
    ggplot(aes(tod_start_, sl_)) + geom_boxplot() +
    scale_y_continuous(limits = c(0, 2000))

}, simplify = FALSE
)


plots <- c(plots, list(p.truth))
wrap_plots(plots)

# Lets shuffle
plots <- sample(plots)
wrap_plots(plots)

# For more sophisticated ways to shuffle, have a look at the nullabor package
# (https://cran.r-project.org/web/packages/nullabor/)
