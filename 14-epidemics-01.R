library(epidemics)
library(tidyverse)

# define infectious population
# 1 infectious person of 14 years
infectious_population <- 1/30000000

# initial conditions
matrix(c(
  S = 1,
  E = 0,
  I = 0,
  R = 0,
  V = 0
), nrow = 1)

# define the population object
# matrix with the initial proportion of population in compartments
population_object <- epidemics::population(
  name = "Peru",
  demography_vector = 30000000,
  contact_matrix = matrix(1),
  initial_conditions = matrix(
    c(
      S = 1 - infectious_population,
      E = 0,
      I = infectious_population,
      R = 0,
      V = 0
    ),
    nrow = 1
  )
)

population_object

# SEIR-V
# -----
# S = susceptible
# E = exposed (infected)
# I = infectious
# R = recovered
# V = vaccinated
# -----
# S > E : transmission rate   = R0 * recovery rate
# E > I : infectiousness rate = 1 / latent period
# I > R : recovery rate       = 1 / infectious period

# define the compartmental model structure to reuse
# influenza outbreak
output <- epidemics::model_default(
  population = population_object,
  transmission_rate = 1.46 / 7,
  infectiousness_rate = 1 / 3,
  recovery_rate = 1 / 7,
  time_end = 750
) 

# plot the simulation output
# plot
output %>%
  # dplyr::filter(compartment == "infectious") %>% 
  ggplot(aes(
    x = time,
    y = value,
    color = demography_group,
    linetype = compartment
  )) + geom_line()

# print the time and size of peak
# peak time and size
epidemics::epidemic_peak(output)

# tutorials-late
# episodes 1, 2, 4

# use socialmixr (before population)
# access social contact data
socialmixr::get_survey()
socialmixr::contact_matrix()

# for initial conditions you will
# bind multiple matrix per age group
# use interventions (within model default)
epidemics::intervention()
epidemics::vaccination()