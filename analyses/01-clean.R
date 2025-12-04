
#' goal: clean a messy epidemiological data frame
#' 
#' task: 
#' - identify how the input changes running step by step

library(cleanepi)
library(tidyverse)

dat <- rio::import(
  "https://epiverse-trace.github.io/tutorials-early/data/simulated_ebola_2.csv"
) %>% dplyr::as_tibble()

dat_dictionary <- readRDS(
  system.file("extdata", "test_dict.RDS", package = "cleanepi")
)

dat_dictionary

dat %>%
  cleanepi::scan_data() %>% 
  dplyr::mutate(across(-Field_names,~ .* 100))

out <- dat %>%
  cleanepi::replace_missing_values() %>%
  cleanepi::standardize_column_names() %>%
  cleanepi::convert_to_numeric(target_columns = "age") %>%
  cleanepi::clean_using_dictionary(dictionary = dat_dictionary) %>%
  cleanepi::standardize_dates(target_columns = c("date_onset", "date_sample")) %>% 
  cleanepi::check_date_sequence(target_columns = c("date_onset", "date_sample"))

out

cleanepi::print_report(out)

#' next steps:
#' - tutorials early episode 2 https://epiverse-trace.github.io/tutorials-early/clean-data.html
#' - {cleanepi} reference manual https://epiverse-trace.github.io/cleanepi/
#' - {numberize} reference manual https://epiverse-trace.github.io/numberize/
 
