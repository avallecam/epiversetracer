library(epidemics)
library(socialmixr)
library(tidyverse)
library(epiparameter)
library(EpiEstim)

data("Flu2009")

early_flu_data <- Flu2009$incidence 

flu_si <- epiparameter::epiparameter_db(
  disease = "influenza",
  epi_name = "serial interval",
  single_epiparameter = TRUE,
)
params <- epiparameter::get_parameters(flu_si)
si_dist <- dgamma(seq(0, 25), shape = params["shape"], scale = params["scale"])
si_dist <- si_dist/sum(si_dist)


output_Rt <- EpiEstim::estimate_R(
  incid = early_flu_data, 
  method = "non_parametric_si",
  config = make_config(list(si_distr = si_dist))
)

plot(output_Rt)

Rt <- mean(output_Rt$R$`Mean(R)`)


zimb_survey <- socialmixr::get_survey("https://doi.org/10.5281/zenodo.3886638")

cnt_data <- socialmixr::contact_matrix(
  countries = "Zimbabwe",
  survey = zimb_survey,
  age.limits = c(0, 25, 50),
  symmetric = TRUE
)

#x = epiparameter::epiparameter_db(disease = "influenza", epi_name = "incubation period", single_epiparameter = T)
# convert_params_to_summary_stats(x)$mean

cnt_matrix <- t(cnt_data$matrix)

demo_vector <- cnt_data$demography$population
names(demo_vector) = rownames(cnt_matrix)

initial_i <- 1e-4

init_cond <- c(S = 1-initial_i, E = 0, I = initial_i, R =0, V = 0)
init_cond_matrix <- rbind(
  init_cond, 
  init_cond,
  init_cond
)

rownames(init_cond_matrix) <- rownames(cnt_matrix)

zimb_pop <- epidemics::population(
  name = "Zimbabwe",
  demography_vector = demo_vector,
  contact_matrix = cnt_matrix,
  initial_conditions = init_cond_matrix
)

infectious_period <- 7
beta <- Rt/infectious_period

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
    y = "Infected individuals"
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
    y = "Infected individuals"
  )



