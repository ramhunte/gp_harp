

# Script to run the model to produce figures for AGU 2019

library(tidyverse)

pop <- 'fall.chinook'
run_stochastic_eggtofry <- 'yes'
sensitivity.mode <- 'no'

spawner.init <- read.csv('outputs/fall_chinook/lcm/fall.chinook_abundance_by_subbasin.csv') %>%
  filter(scenario == 'Current') %>%
  pull(spawners)

# Peak flows at Doty
source('flow_ef_surv.R')


surv_gm <- usgs_gm %>%
  mutate(surv_cur = RI_to_surv(RI),
         surv_2040 = flow_to_RI_gm(Q_max * 1.3) %>% RI_to_surv(),
         surv_2080 = flow_to_RI_gm(Q_max * 1.6) %>% RI_to_surv())

surv_gm <- list(
  surv_gm %>%
    pull(surv_cur)
  ,
  surv_gm %>%
    pull(surv_2040)
  ,
  surv_gm %>%
    pull(surv_2080)
)

# Number of years the model will run for
years <- length(peak_gm$Q_max)
runs <- 3



source('lcm/scripts/funcs.R')
source('lcm/scripts/initialize.R')
source("lcm/scripts/assign.dat.R")
source('lcm/params/params.fall.chinook.R')


# Create arrays
# N <- N.init <- matrix(
#   0, 
#   nrow = length(lifestages), 
#   ncol = num.reaches, 
#   dimnames = list(
#     lifestages,
#     reach.names) 
# ) 


model.all.agu <- array(
  NA,
  c(runs,
    years,
    length(lifestages),
    num.reaches
  ),
  dimnames = list(
    1:runs,
    1:years, 
    lifestages, 
    reach.names
  )
)



# Run model 7 generations for each scenario ----

for (r in 1:runs) { # runs loop
  
  # if (r == 1) {ef_flows <- surv_gm_cur}
  # if (r == 2) {ef_flows <- surv_gm_2040}
  # if (r == 3) {ef_flows <- surv_gm_2080}
  
  ef_flows <- surv_gm[[r]]
  
  ef_flow <- 1
  # initialize
  for (y in 1:10) {
    N.initialize['spawners', ] <- spawner.init * 0.75
    N.initialize <- subbasin(mat = N.initialize) 
  }
  
  N <- N.initialize
  
  
  for (y in 1:years) {
    
    ef_flow <- ef_flows[y]
    
    N <- subbasin(mat = N)
    model.all.agu[r, y, , ] <- N
  }
}


x <- model.all.agu[,,'spawners',] %>%
  apply(., c(1,2), sum) %>%
  as.data.frame.table() %>%
  rename(run = Var1, year = Var2, n = Freq)

# x %>%
#   mutate(Q_max = daily_plot %>%
#            filter(param == 'Q_max') %>%
#            pull(Q)
#   ) %>%
#   gather(param, value, c(spawners, Q_max)) %>%
#   ggplot() +
#   geom_path(aes(years,value)) +
#   #scale_x_discrete(breaks=seq(0,100,10)) +
#   facet_wrap(~param, ncol = 1, scales = 'free_y') +
#   theme_bw()


x %>%
  mutate(year = as.numeric(year),
         years = ifelse(run == 1 , year,
                        ifelse(run == 2, year + years,
                               year + years*2))) %>%
  ggplot +
  geom_line(aes(year,n, color = run)) +
  facet_wrap(~run, ncol = 3)


usgs_gm %>%
  gather(metric, value, Q_max:RI) %>%
  ggplot +
  geom_line(aes(waterYear, value)) +
  facet_wrap(~metric, ncol = 1, scales = 'free_y')
