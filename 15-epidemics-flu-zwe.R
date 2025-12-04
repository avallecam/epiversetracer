# load packages
library(epidemics)
library(socialmixr)
library(tidyverse)
library(epiparameter)
library(EpiEstim)

# Import linelist data ------------------------------------------

# load data
data("Flu2009")

# filter incidence data
early_flu_data <- Flu2009$incidence %>%
  dplyr::filter(dates < "2009-05-10")

# Access delays distributions -----------------------------------

# access serial interval
flu_si <- epiparameter::epiparameter_db(
  disease = "influenza",
  epi_name = "serial interval",
  single_epiparameter = TRUE,
)

# get a discrete gamma distribution
si_dist <- flu_si %>% 
  epiparameter::discretise() %>% 
  density(at = seq(0, 25))

# ensure probabilities add up to 1 by normalising them by the sum
si_dist %>% sum()

# Estimate reproduction number ----------------------------------

# Use EpiEstim to estimate R with uncertainty
output_Rt <- EpiEstim::estimate_R(
  incid = early_flu_data, 
  method = "non_parametric_si",
  config = make_config(list(si_distr = si_dist))
)

plot(output_Rt)

# get mean mean over time
Rt <- mean(output_Rt$R$`Mean(R)`)

# Set up the transmission model -------------------------------------------

# access to social contact survey data
zimb_survey <- socialmixr::get_survey("https://doi.org/10.5281/zenodo.3886638")

cnt_data <- socialmixr::contact_matrix(
  countries = "Zimbabwe",
  survey = zimb_survey,
  age.limits = c(0, 25, 50),
  symmetric = TRUE
)

#x = epiparameter::epiparameter_db(disease = "influenza", epi_name = "incubation period", single_epiparameter = T)
# convert_params_to_summary_stats(x)$mean

# get contact matrix
cnt_matrix <- t(cnt_data$matrix)

cnt_matrix

# get demography vector
demo_vector <- cnt_data$demography$population
names(demo_vector) = rownames(cnt_matrix)

demo_vector

# define initial conditions
initial_i <- 1e-4

init_cond <- c(
  S = 1 - initial_i,
  E = 0,
  I = initial_i,
  R = 0,
  V = 0
)

init_cond_matrix <- rbind(
  init_cond, 
  init_cond,
  init_cond
)

rownames(init_cond_matrix) <- rownames(cnt_matrix)

init_cond_matrix

# define the population object
zimb_pop <- epidemics::population(
  name = "Zimbabwe",
  demography_vector = demo_vector,
  contact_matrix = cnt_matrix,
  initial_conditions = init_cond_matrix
)

# Simulate scenario ------------------------------------------------

infectious_period <- 7
beta <- Rt / infectious_period

base_model <- epidemics::model_default(
  population = zimb_pop, 
  transmission_rate = beta,
  recovery_rate = 1/7,
  infectiousness_rate = 1/2, # 1/ pre-infectious = 
  time_end = 600,
  increment = 1
)

base_model %>% 
  filter(compartment=="infectious") %>% 
  ggplot(
    aes(
      x = time,
      y = value,
      col = demography_group
    )
  ) +
  geom_line() +
  scale_colour_brewer(
    palette = "Dark2",
    name = "Age group"
  )+
  coord_cartesian(expand = FALSE)+
  theme_bw() +
  theme(legend.position = "top")+
  labs(
    x = "Simulation time (days)",
    y = "Infectious individuals"
  )

close_work <- epidemics::intervention(
  name = "workplaces closure",
  type = "contacts",
  time_begin = 100,
  time_end = 100+60,
  reduction = matrix(c(0.1, 0.5, 0.3))
)

output <- epidemics::model_default(
  population = zimb_pop, 
  transmission_rate = 1.3/7,
  recovery_rate = 1/7,
  infectiousness_rate = 1/2,
  intervention = list(contacts = close_work),
  time_end = 600,
  increment = 1
)

output %>% 
  filter(compartment=="infectious") %>% 
  ggplot(
    aes(
      x = time,
      y = value,
      col = demography_group
    )
  ) +
  geom_line() +
  scale_colour_brewer(
    palette = "Dark2",
    name = "Age group"
  )+
  coord_cartesian(expand = FALSE)+
  theme_bw() +
  theme(legend.position = "top")+
  labs(
    x = "Simulation time (days)",
    y = "Infectious individuals"
  )


# Test yourself -------------------------------------

# Challenge 1
# - access the tutorial on simulating transmission
# - read how to account for uncertainty in simulations using {epidemics}
# link: https://epiverse-trace.github.io/tutorials-late/simulating-transmission.html

# Challenge 2
# - access the article vignettes in the package website
# - read how to model overlapping and sequential interventions using {epidemics}
# link: https://epiverse-trace.github.io/epidemics/dev/articles/

# Challenge 3
# - access the how-to guide on "Simulate Pandemic Scenarios with Uncertainty"
# - propose one package that could improve the traceability of the input parameters
# - fill in an issue on the GitHub repository with your proposal
# link: https://epiverse-trace.github.io/howto/analyses/simulate_transmission/epidemics-scenarios-uncertainty.html