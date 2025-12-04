
#' goal: estimate delay-adjusted case fatality risk
#' 
#' task: 
#' - identify how the input changes running step by step

library(cfr)
library(epiparameter)
library(tidyverse)

dat <- readr::read_rds(
  "https://epiverse-trace.github.io/tutorials-middle/data/mers_linelist.rds"
)

dat

# goal: estimate the case fatality risk 
# adjusted by delays in the observation of deaths
# during an ongoing outbreak

mers_onset_death <- epiparameter::epidist_db(
  disease = "mers",
  epi_name = "onset to death",
  single_epiparameter = TRUE
)

plot(mers_onset_death)
mers_onset_death$summary_stats$mean
# 99% of those who die of MERS
# will do within 48 days.
quantile(mers_delay,p = 0.99)
# the y axis value of the probability density function
stats::density(mers_onset_death,at = 10)
# we need the function
as.function(mers_onset_death,func_type = "density")
# based on this, {cfr} will calculate
# the understimation factor of the 
# known outcomes 
# (expected outcomes)

dat %>%
  # dplyr::mutate(diff = dt_death - dt_onset) %>% 
  # skimr::skim(diff)
  incidence2::incidence(
    date_index = c(cases = "dt_onset", deaths = "dt_death"),
    complete_dates = TRUE
  ) %>%
  # plot(angle = 90)
  cfr::prepare_data() %>%
  # cfr::cfr_static()
  cfr::cfr_static(
    delay_density = as.function(mers_onset_death, func_type = "density")
  )

#' next steps:
#' - tutorials middle episode 1-5 https://epiverse-trace.github.io/tutorials-middle/
#' - {cfr} vignette https://epiverse-trace.github.io/cfr/articles/estimate_ascertainment.html
#' - how-to guide https://epiverse-trace.github.io/howto/analyses/estimate_severity/cfr-stratified-severity.html
#' - how-to guide https://epiverse-trace.github.io/howto/analyses/reconstruct_transmission/estimate_infections.html

