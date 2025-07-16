# connect parameters to simulate transmission chains ---------------------

# access discrete generation time approximation --------------------------

#rnbinom
mers_serial_interval <- epiparameter::epiparameter_db(
  disease = "mers",
  epi_name = "serial",
  single_epiparameter = TRUE
) %>% 
  epiparameter::discretise()

# simulate transmission chains -------------------------------------------

mers_chains <- epichains::simulate_chains(
  n_chains = 1000,
  statistic = "size",
  offspring_dist = rnbinom,
  mu = fit_estimates$estimate["mu"],
  size = fit_estimates$estimate["size"],
  generation_time = as.function(mers_serial_interval, func_type = "generate")
)

summary(mers_chains)
# aggregate(mers_chains,by = "time") %>% plot()
# aggregate(mers_chains,by = "generation") %>% plot()

# likelihood -------------------------------------------------------------

epichains::likelihood(
  chains = mers_chains,
  statistic = "size",
  offspring_dist = rnbinom,
  mu = as.numeric(fit_estimates$estimate["mu"]),
  size = as.numeric(fit_estimates$estimate["size"])
)

# simulate stats ---------------------------------------------------------

mers_stats <- epichains::simulate_chain_stats(
  n_chains = 1000,
  statistic = "size",
  offspring_dist = rnbinom,
  mu = fit_estimates$estimate["mu"],
  size = fit_estimates$estimate["size"]#,
  # generation_time = as.function(mers_serial_interval, func_type = "generate")
)

mers_stats

# nested -----------------------------------------------------------------

epinested <- purrr::map(.x = seq_len(1000), .f = function(sim) {
  epichains::simulate_chains(
    n_chains = 1,
    statistic = "size",
    offspring_dist = rnbinom,
    mu = fit_estimates$estimate["mu"],
    size = fit_estimates$estimate["size"],
    generation_time = as.function(mers_serial_interval, func_type = "generate")
  ) %>% 
    mutate(simulation_id = sim)
}) %>% 
  purrr::list_rbind()

epinested %>%
  tibble::as_tibble() %>%
  mutate(day = ceiling(time)) %>%
  count(simulation_id, day, name = "cases") %>%
  dplyr::group_by(simulation_id) %>%
  dplyr::mutate(cases_cumsum = cumsum(cases)) %>%
  dplyr::ungroup() %>% 
  ggplot(aes(x = day, y = cases_cumsum, group = simulation_id)) +
  geom_line()

