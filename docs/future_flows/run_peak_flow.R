

# Script to run the model to produce figures for AGU 2019

library(tidyverse)
library(lubridate)

rmarkdown::render('docs/future_flows/future_flows.Rmd')

fishtype <-  c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')

pops <- fishtype


# Convert fishtype (Hab model) to pop and species (LCM)

pops[pops == 'fall_chinook'] <- 'fall.chinook' 
pops[pops == 'spring_chinook'] <- 'spring.chinook'


run_stochastic_eggtofry <- 'no'
sensitivity.mode <- 'no'

scenarios <- c('Current.csv', 'Historical.csv')

# Number of years the model will run for
runs <- 2 # dummy variable for now
years <- 100 #length(peak_ch$q_cfs)
climates <- c('Current', '2050-4.5','2080-4.5','2050-8.5', '2080-8.5')
reach.names <- read.csv("lcm/data/subbasin_names.csv") %>%
  select(Subbasin) %>%
  mutate(Subbasin = as.character(Subbasin)) %>%
  unlist(use.names = FALSE)

model.all.pf <- array(
  NA,
  c(length(climates),
    years,
    1, # lifestages
    length(pops),
    length(scenarios)
  ),
  dimnames = list(
    1:length(climates),
    1:years, 
    'spawners', 
    pops,
    scenarios
  )
)


incubation_months <- list(
  c(11,12,1:4), # coho
  #c(9:12, 1:3), # spring chinook
  c(10:12, 1:4),
  c(10:12, 1:4), # fall chinook
  c(3:9) # steelhead
)

for (pop in pops) {
  
  surv_df <- daily_ch %>%
    mutate(month = month(Date)) %>%
    filter(month %in% incubation_months[[which(pop == pops)]]) %>%
    group_by(waterYear) %>%
    summarize(Q_max = max(Q) * 35.31467) %>%
    mutate(ri = q_to_ri(Q_max)) %>% # chehalis = 0.14 skewness
    mutate(diff_perc_rcp45_2050 = predict(mods$fit[[1]], newdata = .),
           diff_perc_rcp45_2080 = predict(mods$fit[[2]], newdata = .),
           diff_perc_rcp85_2050 = predict(mods$fit[[3]], newdata = .),
           diff_perc_rcp85_2080 = predict(mods$fit[[4]], newdata = .),
           Q_rcp45_2050 = Q_max + Q_max * diff_perc_rcp45_2050,
           Q_rcp45_2080 = Q_max + Q_max * diff_perc_rcp45_2080,
           Q_rcp85_2050 = Q_max + Q_max * diff_perc_rcp85_2050,
           Q_rcp85_2080 = Q_max + Q_max * diff_perc_rcp85_2080) %>%
    mutate(surv_cur = ri_to_surv_rescale(ri),
           surv_rcp45_2050 = q_to_ri(Q_rcp45_2050) %>% ri_to_surv_rescale(),
           surv_rcp45_2080 = q_to_ri(Q_rcp45_2080) %>% ri_to_surv_rescale(),
           surv_rcp85_2050 = q_to_ri(Q_rcp85_2050) %>% ri_to_surv_rescale(),
           surv_rcp85_2080 = q_to_ri(Q_rcp85_2080) %>% ri_to_surv_rescale())
  
  
  surv_ch <- list(
    surv_df %>%
      pull(surv_cur) %>%
      loop_series()
    ,
    surv_df %>%
      pull(surv_rcp45_2050) %>%
      loop_series()
    ,
    surv_df %>%
      pull(surv_rcp45_2080) %>%
      loop_series()
    ,
    surv_df %>%
      pull(surv_rcp85_2050) %>%
      loop_series()
    ,
    surv_df %>%
      pull(surv_rcp85_2080) %>%
      loop_series()
  )
  
  source(list.files('lcm/params', pattern = str_extract(pop, "[^_]+"), full.names = TRUE))
  source('lcm/scripts/funcs.R')
  
  outputs_hab <- file.path('outputs', fishtype[which(pop == pops)], 'hab.scenarios')
  source('lcm/scripts/initialize.R')
  habitat.file <- habitat.file[habitat.file %in% c('Current.csv', 'Historical.csv')]
  
  for (n in 1:length(habitat.file)) {
    
    
    source("lcm/scripts/assign.dat.R")
    
    egg.fry.surv.orig <- egg.fry.surv
    
    spawner.init <- list.files(file.path('outputs',fishtype[which(pop == pops)]), 
                               pattern = 'abundance_by_subbasin.csv', 
                               recursive = TRUE,
                               full.names = TRUE) %>%
      read.csv %>%
      filter(scenario == str_remove(habitat.file[n], '.csv')) %>%
      pull(spawners)
    
    
    for (c in 1:length(climates)) { # runs loop
      
      ef_scalar <- surv_ch[[c]]
      
      egg.fry.surv <- egg.fry.surv.orig
      
      # initialize
      for (y in 1:10) {
        N.initialize['spawners', ] <- spawner.init# * 0.75
        N.initialize <- subbasin(mat = N.initialize) 
      }
      
      N <- N.initialize
      
      # burn in
      for (y in 1:50) {
        egg.fry.surv <- ef_scalar[y] * egg.fry.surv.orig
        N <- subbasin(mat = N)
      }
      
      # 100 year run
      for (y in 1:years) { # length(surv_df$waterYear)
        
        egg.fry.surv <- ef_scalar[y] * egg.fry.surv.orig
        
        N <- subbasin(mat = N)
        
        model.all.pf[c, y, "spawners", pop, n] <- sum(N['spawners',])
      }
    }
  }
}  
  

x <- model.all.pf[,,'spawners',,] %>%
  as.data.frame.table() %>%
  rename(run = Var1, year = Var2, species = Var3, scenario = Var4, n = Freq) %>%
 # filter(n > 0) %>%
  mutate(year = as.numeric(year),
         scenario = str_remove(scenario, '.csv'),
         era = case_when(
           run == 1 ~ 'Current',
           run %in% c(2,4) ~ 'Mid-century',
           run %in% c(3,5) ~ 'Late-century'),
         climate = case_when(
           run == 1 ~ 'Current',
           run %in% c(2,3) ~ 'RCP 4.5',
           run %in% c(4,5) ~ 'RCP 8.5'),
         species = recode_factor(species,
                                 coho = 'Coho',
                                 spring.chinook = 'Spring Chinook',
                                 fall.chinook = 'Fall Chinook',
                                 steelhead = 'Steelhead')) %>%
  group_by(species, scenario) %>%
  mutate(median = median(n),
         diff = n - median(n[era == 'Current']),
         perc_diff = diff / median(n[era == 'Current']))

y <-  x %>%
  filter(scenario == 'Current') %>%
  group_by(species, era, climate) %>%
  summarize(median = median(n)) 

print(
  x %>%
    filter(scenario == 'Current') %>%
    ggplot(aes(era,perc_diff, color = climate)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position = position_jitterdodge(jitter.width = 0.1), alpha = 0.3) +
    facet_wrap(~species) +
    theme_bw() +
    scale_color_manual(values = c('black','orange1','orangered2')) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = 'Spawner Change from Current (%)', color = NULL)
)


print(
  
  x %>%
    filter(scenario == 'Current') %>%
    ggplot +
    theme_bw() +
    geom_line(aes(year, n, color = era, lty = climate)) +
    facet_wrap(~species, scales = 'free_y', ncol = 1) +
    geom_hline(data = y, aes(yintercept = median, color = era), lty = 5)
  
)


print(  
  
  x %>%
    ggplot +
    theme_bw() +
    geom_line(aes(year, n/1000, color = era, lty = climate)) +
    facet_grid(species~scenario, scales = 'free_y')
)

print(
  x %>%
   # filter(scenario == 'Current') %>%
    group_by(scenario, species, era, climate) %>%
    summarize(min = min(perc_diff) %>% scales::percent(),
              mean = mean(perc_diff) %>% scales::percent(),
              max = max(perc_diff) %>% scales::percent()) %>%
    as.data.frame()
)

