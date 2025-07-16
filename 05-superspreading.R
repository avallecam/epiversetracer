
#' goal: probability of a case originating from a cluster of infections
#' given the superspreading potential
#' 
#' task: 
#' - identify how the input changes running step by step

library(epicontacts)
library(fitdistrplus)
library(superspreading)
library(tidyverse)

mers_set <- outbreaks::mers_korea_2015

# contact data
# from infector 
# to infectee
head(mers_set$contacts)

# contact only have 98 relations
dim(mers_set$contacts)
dim(mers_set$linelist)


# estimate parameters from offspring distribution ------------------------

fit_estimates <- epicontacts::make_epicontacts(
  linelist = mers_set$linelist,
  contacts = mers_set$contacts,
  directed = TRUE
) %>%
  # epicontacts::vis_epicontacts()
  # outgoing edges emanating
  # from node
  epicontacts::get_degree(
    type = "out",
    only_linelist = TRUE
  ) %>% 
  # plot: distribution of secondary cases
  # hist(breaks= 40)
  # fitted: offspring distribution
  fitdistrplus::fitdist(distr = "nbinom")

# (individual-level) reproduction number
# mean = (population) reproduction number
# k = dispersion parameter
# the smallest - more dispersed

# estimate proportion cluster size ---------------------------------------

set.seed(33)
superspreading::proportion_cluster_size(
  R = fit_estimates$estimate["mu"],
  k = fit_estimates$estimate["size"],
  cluster_size = c(5, 10, 25)
)


# simulate transmission chains -------------------------------------------

fit_estimates

mers_gener <- epiparameter::epiparameter_db(
  disease = "mers",
  epi_name = "serial",
  single_epiparameter = TRUE
)

set.seed(33)

epichains::simulate_chains(
  n_chains = 100,
  statistic = "size",
  offspring_dist = rnbinom,
  mu = fit_estimates$estimate["mu"],
  size = fit_estimates$estimate["size"],
  generation_time = as.function(mers_gener,func_type = "generate")
) %>% 
  summary()

#' next steps:
#' - tutorials middle episode 6-7 https://epiverse-trace.github.io/tutorials-middle/superspreading-estimate.html
#' - {superspreading} vignettes https://epiverse-trace.github.io/superspreading/

