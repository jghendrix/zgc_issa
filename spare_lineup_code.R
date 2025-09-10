library(lubridate)
library(terra)
library(raster)
library(tidyverse)
library(tidyterra)
library(conflicted)
library(ggplot2)
library(glmmTMB)
library(amt) # dev version needed: remotes::install_github("jmsigner/amt")
library(cowplot)
library(MASS)
library(mixedSSA)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

set.seed(11102023)
n <- 2 ### Number of individuals to simulate 

tc2 <- raster(nrows=1000, ncols=1000, xmn=-500, xmx=500, ymn=-500,ymx=500)  
tc2[] <- runif(1000000, 0,50) ## assigning simulated values to the raster
# producing a new raster with moving window of size 11x13 and calculating mean values for the neighborhood
tc2 <- raster::focal(tc2, w = matrix(1, 11, 13), mean) 
tc2 <- raster::focal(tc2, w = matrix(1, 11, 13), mean)
# we did the focal operation twice to produce a smoothed out edges of the interspersed habitats
reclass_df <- matrix(c(12,24.5, 1, 24.5,38, 0), ncol=3, byrow=T)
tc_reclass <- raster::reclassify(tc2, reclass_df)

r <- rast(tc_reclass)
names(r) <- "x1"
r <- as.factor(r)
cls <- data.frame(id=c(0,1), x1=c("Habitat A", "Habitat B"))
levels(r) <- cls
plot(r)

# Initializing
sig <- matrix(c(0.25,0.15,0.15, 0.15, 0.2, 0.15, 0.15,0.15, 0.25), ncol =3, byrow = T)
param.value <-data.frame(id = NA, shape = NA, scale = NA, kappa = NA)
ssf_sim <-list()

# WARNING: takes several days to run apparently
for(i in 1:n){
  start <- make_start(c(runif(1,-20,20), runif(1,-20,20)), ta_ = 0)
  param.value[i,1] <- i
  param.value[i,2:4] <- mvrnorm(n=1, mu=c(3,1,4), Sigma = sig) 
  ## simulating initial parameters using multivariate normal distribution
  
  m <- make_issf_model(coefs = c(x1_end = 0.05,  
                                 "sl_" = 0, "log(sl_)" = 0, "cos(ta_)" = 0,
                                 "sl_:x1_start" = 0.05, "log(sl_):x1_start" = 0.05,"cos(ta_):x1_start" = 0.05), 
                       sl = make_gamma_distr(shape = param.value[i,2], 
                                             scale = param.value[i,3]
                       ), 
                       ta = make_vonmises_distr(kappa = param.value[i,4]
                       ))
  
  rdk <- redistribution_kernel(m, start = start, map = r, as.rast= FALSE,
                               landscape= "continuous")
  
  ssf_sim[[i]] <- simulate_path(rdk, n.steps = floor(rnorm(1,2000,250))) ### simulate individual trajectories
  ssf_sim[[i]]$id <-i
}


# Example using simply generated simulated data - this has an inherent system to anonymize the real data that you run a line of decrypt() to identify ------
library(nullabor)
ggplot(lineup(null_permute('mpg'), mtcars), aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ .sample)
ggplot(lineup(null_permute('cyl'), mtcars),
       aes(mpg, .sample, colour = factor(cyl))) +
  geom_point()







# past troubleshooting: ----

# object "prop_forest" not found..... huh??

# can't figure out why it can't locate those variables... let's try another simpler model first

f_coef <- c("prop_forest_end" = 0.061596)

forest <- amt::make_issf_model(coefs = f_coef,
                               sl = make_gamma_distr(shape = 0.6341374, scale = 378.2702),
                               ta = make_vonmises_distr(kappa = 0.1634281)
)

rdk <- amt::redistribution_kernel(
  x = forest,
  start = start,
  map = ftrim,
  n.control = 10,
  tolerance.outside = 0.5
)












# Median of the rasters: pt <- c(704807.4, 5502579)
origin <- winter_steps %>% 
  #filter(x1_ > 704800 & x1_ < 704820) #%>%
  filter(y1_ > 5502500 & y1_ < 5502600)

obs <- winter_prep %>%  
  
  
  
  # Older examples -----
data("deer")
forest <- get_sh_forest()

deer <- deer |>
  slice_head(n = 50)  

# Observed mean step length for day and night
p.truth <- deer |>
  steps_by_burst() |>
  extract_covariates(forest, where = "both") |>
  time_of_day(where = "both") |>
  ggplot(aes(tod_start_, sl_)) + geom_boxplot()

p.truth

plots <- c(plots, list(p.truth))
wrap_plots(plots)

# Lets shuffle
plots <- sample(plots)
wrap_plots(plots)



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
