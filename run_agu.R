

# Script to run the model to produce figures for AGU 2019

library(tidyverse)
library(lubridate)

pops <-  c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')
run_stochastic_eggtofry <- 'yes'
sensitivity.mode <- 'no'


# Peak flows at Doty
source('cig.R')
source('flow_ef_surv.R')


# Number of years the model will run for
years <- length(peak_gm$Q_max)
runs <- 5
reach.names <- read.csv("lcm/data/subbasin_names.csv") %>%
  select(Subbasin) %>%
  mutate(Subbasin = as.character(Subbasin)) %>%
  unlist(use.names = FALSE)

model.all.agu <- array(
  NA,
  c(runs,
    years,
    1,
    length(pops)
  ),
  dimnames = list(
    1:runs,
    1:years, 
    'spawners', 
    pops
  )
)


incubation_months <- list(
  c(11,12,1:4), # coho
  c(9:12, 1:3), # spring chinook
  c(10:12, 1:4), # fall chinook
  c(3:9) # steelhead
)

for (pop in pops) {
  

  
  surv_df <- Daily_gm %>%
    mutate(month = month(Date)) %>%
    filter(month %in% incubation_months[[which(pop == pops)]]) %>%
    group_by(waterYear) %>%
    summarize(Q_max = max(Q)) %>%
    mutate(returnYr = flow_to_RI_gm(Q_max),
           returnYr = ifelse(is.infinite(returnYr), 100, returnYr)) %>%
    mutate(diff_perc_rcp45_2050 = predict(mods$fit[[1]], newdata = .),
           diff_perc_rcp45_2080 = predict(mods$fit[[2]], newdata = .),
           diff_perc_rcp85_2050 = predict(mods$fit[[3]], newdata = .),
           diff_perc_rcp85_2080 = predict(mods$fit[[4]], newdata = .),
           Q_rcp45_2050 = Q_max + Q_max * diff_perc_rcp45_2050,
           Q_rcp45_2080 = Q_max + Q_max * diff_perc_rcp45_2080,
           Q_rcp85_2050 = Q_max + Q_max * diff_perc_rcp85_2050,
           Q_rcp85_2080 = Q_max + Q_max * diff_perc_rcp85_2080) %>%
    mutate(surv_cur = RI_to_surv(returnYr),
           surv_rcp45_2050 = flow_to_RI_gm(Q_rcp45_2050) %>% RI_to_surv(),
           surv_rcp45_2080 = flow_to_RI_gm(Q_rcp45_2080) %>% RI_to_surv(),
           surv_rcp85_2050 = flow_to_RI_gm(Q_rcp85_2050) %>% RI_to_surv(),
           surv_rcp85_2080 = flow_to_RI_gm(Q_rcp85_2080) %>% RI_to_surv())
  #surv_2080 = flow_to_RI_gm(Q_max * 1.6) %>% RI_to_surv())
  
  surv_gm <- list(
    surv_df %>%
      pull(surv_cur)
    ,
    surv_df %>%
      pull(surv_rcp45_2050)
    ,
    surv_df %>%
      pull(surv_rcp45_2080)
    ,
    surv_df %>%
      pull(surv_rcp85_2050)
    ,
    surv_df %>%
      pull(surv_rcp85_2080)
  )
  
  source(list.files('lcm/params', pattern = str_extract(pop, "[^_]+"), full.names = TRUE))
  source('lcm/scripts/funcs.R')
  source('lcm/scripts/initialize.R')
  source("lcm/scripts/assign.dat.R")
  
  spawner.init <- list.files(file.path('outputs',pop), 
                             pattern = 'abundance_by_subbasin.csv', 
                             recursive = TRUE,
                             full.names = TRUE) %>%
    read.csv %>%
    filter(scenario == 'Current') %>%
    pull(spawners)
  
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
      
      model.all.agu[r, y, "spawners", pop] <- sum(N['spawners',])
    }
  }
}

x <- model.all.agu[,,'spawners',] %>%
  as.data.frame.table() %>%
  rename(run = Var1, year = Var2, species = Var3, n = Freq)

# print(
#   x %>%
#     mutate(year = as.numeric(year),
#            era = ifelse(run == 1 , 'Current',
#                         ifelse(run %in% c(2,4), 'Mid-century',
#                                'Late-century')),
#            era = factor(era, levels = c('Current', 'Mid-century', 'Late-century')),
#            climate = ifelse(run == 1, 'Current',
#                             ifelse(run %in% c(2,3), 'RCP 4.5',
#                                    'RCP 8.5'))) %>%
#     filter(era != 'Mid-century') %>%
#     ggplot +
#     geom_line(aes(year,n, color = climate)) +
#     facet_grid(species~era, scales = 'free_y') +
#     theme_bw() +
#     scale_color_manual(values = c('black','orange1','orangered2')) +
#     labs(x = 'Year', y = 'Spawners')
# )
# 
# ggsave('../misc/AGU_poster/spawners.jpg', dpi = 300, width = 8, height = 4)



print(
  x %>%
    mutate(year = as.numeric(year),
           era = ifelse(run == 1 , 'Current',
                        ifelse(run %in% c(2,4), 'Mid-century',
                               'Late-century')),
           era = factor(era, levels = c('Current', 'Mid-century', 'Late-century')),
           climate = ifelse(run == 1, 'Current',
                            ifelse(run %in% c(2,3), 'RCP 4.5',
                                   'RCP 8.5')),
           species = recode_factor(species, 
                                   coho = 'Coho', 
                                   spring_chinook = 'Spring Chinook', 
                                   fall_chinook = 'Fall Chinook', 
                                   steelhead = 'Steelhead')) %>%
    filter(!(n < 6500 & species == 'Steelhead')) %>%
    group_by(species) %>%
    mutate(n = (n - median(n[era == 'Current']))/median(n[era == 'Current'])) %>%
    ggplot(aes(era,n, color = climate)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position = position_jitterdodge(jitter.width = 0.1), alpha = 0.3) +
    facet_wrap(~species) +
    theme_bw() +
    scale_color_manual(values = c('black','orange1','orangered2')) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = 'Spawner Change from Current (%)', color = NULL)
)

ggsave('../misc/AGU_poster/spawners_boxplot.jpg', dpi = 300, width = 8, height = 4)



# Create figure of flows for each species

map_dfr(incubation_months, .id = 'species', function(x) {
  Daily_gm %>%
    mutate(month = month(Date)) %>%
    filter(month %in% x) %>%
    group_by(waterYear) %>%
    summarize(Q_max = max(Q)) %>%
    mutate(returnYr = flow_to_RI_gm(Q_max),
           returnYr = ifelse(is.infinite(returnYr), 100, returnYr)) %>%
    mutate(diff_perc_rcp45_2050 = predict(mods$fit[[1]], newdata = .),
           diff_perc_rcp45_2080 = predict(mods$fit[[2]], newdata = .),
           diff_perc_rcp85_2050 = predict(mods$fit[[3]], newdata = .),
           diff_perc_rcp85_2080 = predict(mods$fit[[4]], newdata = .),
           Q_rcp45_2050 = Q_max + Q_max * diff_perc_rcp45_2050,
           Q_rcp45_2080 = Q_max + Q_max * diff_perc_rcp45_2080,
           Q_rcp85_2050 = Q_max + Q_max * diff_perc_rcp85_2050,
           Q_rcp85_2080 = Q_max + Q_max * diff_perc_rcp85_2080) %>%
    mutate(surv_cur = RI_to_surv(returnYr),
           surv_rcp45_2050 = flow_to_RI_gm(Q_rcp45_2050) %>% RI_to_surv(),
           surv_rcp45_2080 = flow_to_RI_gm(Q_rcp45_2080) %>% RI_to_surv(),
           surv_rcp85_2050 = flow_to_RI_gm(Q_rcp85_2050) %>% RI_to_surv(),
           surv_rcp85_2080 = flow_to_RI_gm(Q_rcp85_2080) %>% RI_to_surv())
}) %>%
  mutate(species = pops[as.numeric(species)],
         waterYear = waterYear - min(waterYear)) %>%
  select(species, waterYear, Q_max, Q_rcp45_2050, Q_rcp85_2050) %>%
  gather(model, Q, Q_max:Q_rcp85_2050) %>%
  mutate(climate = ifelse(model == 'Q_max' , 'Current',
                          ifelse(str_detect(model,'rcp45'), 'RCP 4.5',
                                 'RCP 8.5')),
         species = recode_factor(species, 
                                 coho = 'Coho', 
                                 spring_chinook = 'Spring Chinook', 
                                 fall_chinook = 'Fall Chinook', 
                                 steelhead = 'Steelhead')) %>%
  ggplot +
  geom_line(aes(waterYear, Q, color = climate)) +
  facet_wrap(~species) +
  theme_bw() +
  scale_color_manual(values = c('black', 'orange1','orangered2')) +
  #scale_fill_manual(values = c('black', 'orange1','orangered2')) +
  scale_size_manual(values = c(1,1, 0.8)) +
  theme_bw() +
  labs(x = 'Model Year',
       y = bquote('Peak Annual Flow (cfs)'), # bquote('Peak Annual Flow (' ~ ft^3 ~s^-1* ')')
       color = NULL,
       size = NULL) +
  theme(legend.position = 'bottom')

ggsave('../misc/AGU_poster/modeled_flows_spp.jpg', dpi = 400, width = 8, height = 4)
