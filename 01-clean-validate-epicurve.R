# after cleanepi update change to date_sample

library(tidyverse)

dat <- rio::import(file = "https://epiverse-trace.github.io/tutorials-early/data/simulated_ebola_2.csv") %>%
  dplyr::as_tibble()

dat_dictionary <- readRDS(system.file("extdata", "test_dict.RDS", package = "cleanepi"))

dat_dictionary

linelist::tags_names()

# clean data
dat %>%
  cleanepi::standardize_column_names() %>%
  cleanepi::standardize_dates(target_columns = "date_onset") %>%
  cleanepi::clean_using_dictionary(dictionary = dat_dictionary) %>%
  
  # validate linelist
  linelist::make_linelist(date_onset = "date_onset", gender = "gender") %>%
  linelist::validate_linelist() %>%
  linelist::tags_df() %>%
  
  # plot epicurve
  incidence2::incidence(date_index = "date_onset",
                        interval = "epiweek",
                        groups = "gender") %>%
  plot(fill = "gender")
