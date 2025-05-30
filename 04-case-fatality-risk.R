
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

mers_onset_death <- epiparameter::epidist_db(
  disease = "mers",
  epi_name = "onset to death",
  single_epiparameter = TRUE
) #%>%
  # epiparameter::parameter_tbl()

plot(mers_onset_death)
mers_onset_death$summary_stats$mean
stats::density(mers_onset_death,at = 10)
as.function(mers_onset_death,func_type = "density")

dat %>%
  incidence2::incidence(
    date_index = c(cases = "dt_onset", deaths = "dt_death"),
    complete_dates = TRUE,
    groups = "age_category"
  ) %>%
  # plot()
  # incidence2:::plot.incidence2(show_cases = TRUE,fill = "age_category")
  cfr::prepare_data() %>%
  dplyr::filter(age_category == "[70,90]") %>% 
  # cfr::cfr_static()
  # cfr::cfr_rolling()
  cfr::cfr_static(
    delay_density = as.function(mers_onset_death, func_type = "density")
  )

#' next steps:
#' - tutorials middle episode 1-5 https://epiverse-trace.github.io/tutorials-middle/
#' - {cfr} vignette https://epiverse-trace.github.io/cfr/articles/estimate_ascertainment.html
#' - how-to guide https://epiverse-trace.github.io/howto/analyses/estimate_severity/cfr-stratified-severity.html
#' - how-to guide https://epiverse-trace.github.io/howto/analyses/reconstruct_transmission/estimate_infections.html

