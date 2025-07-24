library(epidemics)
library(socialmixr)
library(tidyverse)

# generate contact matrix
surveydata <- socialmixr::get_survey("https://doi.org/10.5281/zenodo.3874805")
contactdata <- socialmixr::contact_matrix(
  survey = surveydata,
  countries = "Peru",
  age.limits = c(0, 20),
  symmetric = TRUE
)

contactdata

# prepare contact matrix
t(contactdata$matrix)


# 1 infectious person of 14 years
infectious_population <- 1/11000000

initial_inf <- matrix(
  c(
    S = 1 - infectious_population,
    E = 0,
    I = infectious_population,
    R = 0,
    V = 0
  ),
  nrow = 1
)

initial_free <- matrix(
  c(
    S = 1,
    E = 0,
    I = 0,
    R = 0,
    V = 0
  ),
  nrow = 1
)

# 0-20, 20+
rbind(initial_inf, initial_free)

# matrix with the initial proportion of population in compartments
population_object <- epidemics::population(
  name = "Peru",
  demography_vector = contactdata$demography$population,
  contact_matrix = t(contactdata$matrix),
  initial_conditions = rbind(initial_inf, initial_free)
)

population_object

# SEIR-V
# S > E : transmission rate   = R0 * recovery rate
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
  dplyr::filter(compartment == "infectious") %>% 
  ggplot(aes(
    x = time,
    y = value,
    colour = demography_group,
    linetype = compartment
  )) +
  geom_line()

# compare peak time and size
epidemics::epidemic_peak(simulation_baseline)
