library(epidemics)
library(socialmixr)
library(tidyverse)

# 1 infectious person of 14 years
infectious_population <- 1/30000000

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
# S > E : transmission rate   = R0 / infectious period
# E > I : infectiousness rate = 1 / latent period
# I > R : recovery rate       = 1 / infectious period

# influenza outbreak
simulation_baseline <- epidemics::model_default(
  population = population_object, 
  transmission_rate = 1.46 / 7,
  infectiousness_rate = 1 / 3,
  recovery_rate = 1 / 7,
  time_end = 750
)

simulation_baseline %>%
  # dplyr::filter(compartment == "infectious") %>% 
  ggplot(aes(
    x = time,
    y = value,
    colour = demography_group,
    linetype = compartment
  )) +
  geom_line()