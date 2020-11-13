

# Script to run the model to produce figures for AGU 2019

library(tidyverse)
library(lubridate)

# Create location map (fig1)
#source('docs/future_flows/location_map.R')

rmarkdown::render('docs/future_flows/future_flows.Rmd')

fishtype <-  c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')

pops <- fishtype


# Convert fishtype (Hab model) to pop and species (LCM)

pops[pops == 'fall_chinook'] <- 'fall.chinook' 
pops[pops == 'spring_chinook'] <- 'spring.chinook'


run_stochastic_eggtofry <- 'no'
sensitivity.mode <- 'no'

scenarios <- c('Current.csv', 'Historical.csv')#Floodplain.csv', 'Shade.csv', 'Historical.csv')

# Number of years the model will run for
runs <- 2 # dummy variable for now
years <- 100 #length(peak_ch$q_cfs)
climates <- c('Current', '2050-4.5','2080-4.5','2050-8.5', '2080-8.5')
reach.names <- read.csv("lcm/data/subbasin_names.csv") %>%
  select(Subbasin) %>%
  mutate(Subbasin = as.character(Subbasin)) %>%
  unlist(use.names = FALSE)


store_lifestages <- c(paste0('age', 0:6,'.ocean'), paste0('ocean', 0:6), 'total.run', 'spawners', 'ef_scalar')

model.all.pf <- array(
  NA,
  c(length(climates),
    years,
    length(reach.names),
    length(store_lifestages), # lifestages
    length(pops),
    length(scenarios)
  ),
  dimnames = list(
    1:length(climates),
    1:years,
    reach.names,
    store_lifestages, 
    pops,
    scenarios
  )
)


# coho Nov 15 - May 15 (320-136) 181 days
# spring Sep 1 - April 15 (245-106) 226 days
# fall Oct 1 - May 1 (275-122) 216 days
# sthd feb 15 - Sep 15 (46-259) 152 days

incubation_range <- list(
  c(start = "1115",end = "0315"), 
  c(start = "0901",end = "0415"),
  c(start = "1001",end = "0501"),
  c(start = "0215",end = "0915")
)

for (pop in pops) {
  
  r <- incubation_range[[which(pop %in% pops)]]
  
  surv_df <- daily_ch %>%
    mutate(monthday = format(Date, "%m%d")) %>% 
    filter(monthday <= r['start'] | monthday >= r['end']) %>%
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
  habitat.file <- habitat.file[habitat.file %in% scenarios]
  
  store_stages_spp <- dimnames(N)[[1]][dimnames(N)[[1]] %in% store_lifestages]
  
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
        
        model.all.pf[c, y, , store_stages_spp, pop, n] <- t(N[store_stages_spp, ])
        model.all.pf[c, y, , "ef_scalar", pop, n] <- ef_scalar[y]
        
      }
    }
  }
}  
  

# Convert array to df for easier analysis

model.all.df <- model.all.pf %>%
  as.data.frame.table() %>%
  rename(run = Var1, 
         year = Var2, 
         subbasin = Var3, 
         lifestage = Var4, 
         species = Var5, 
         scenario = Var6, 
         n = Freq) %>%
  filter(n > 0) %>%
  mutate(year = as.numeric(year),
         scenario = str_remove(scenario, '.csv'),
         lifestage = case_when(
           str_detect(lifestage, '.ocean') ~ paste0('ocean',str_extract(lifestage, '([0-9]+)')),
           TRUE ~ as.character(lifestage)),
         era = case_when(
           run == 1 ~ 'Current',
           run %in% c(2,4) ~ 'Mid-century',
           run %in% c(3,5) ~ 'Late-century'),
         era = factor(era, levels = c('Current', 'Mid-century', 'Late-century')),
         climate = case_when(
           run == 1 ~ 'Current',
           run %in% c(2,3) ~ 'RCP 4.5',
           run %in% c(4,5) ~ 'RCP 8.5'),
         species = recode_factor(species,
                                 coho = 'Coho',
                                 spring.chinook = 'Spring Chinook',
                                 fall.chinook = 'Fall Chinook',
                                 steelhead = 'Steelhead'))

x <- model.all.df %>%
  filter(lifestage == 'spawners') %>%
  group_by(run, year, species, scenario, climate, era) %>%
  summarize(n = sum(n)) %>%
  group_by(species, scenario) %>%
  mutate(diff = n - median(n[era == 'Current']),
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



  
p7 <-  x %>%
  filter(scenario == 'Current',
         climate != 'RCP 4.5',
         era != 'Mid-century') %>%
  ggplot +
  cowplot::theme_half_open() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none") +
  geom_line(aes(year, n, color = era)) +
  facet_wrap(~species, ncol = 1, scales = 'free_y') +
  scale_y_continuous(labels = scales::label_comma()) +
  #                    breaks = c(-1, -.5, 0, 0.5),
  #                    limits = c(-1, 0.5)
  expand_limits(y = 0) +
  scale_color_manual(values = c('black', 'grey40')) +
  labs(y = 'Spawner abundance', x = 'Model Year', color = NULL)

ggsave('docs/future_flows/Fig7_timeseries.tiff', p7, height = 5, width = 6, dpi = 300, compression = 'lzw')

p7

summary_tab <-  x %>%
  # filter(scenario == 'Current') %>%
  group_by(scenario, species, era, climate) %>%
  summarize(min = min(perc_diff) %>% scales::percent(),
            #mean = mean(perc_diff) %>% scales::percent(),
            median = median(perc_diff) %>% scales::percent(),
            max = max(perc_diff) %>% scales::percent(),
            sd = sd(perc_diff) %>% scales::percent()) %>%
            #cv = sd(perc_diff)/mean(perc_diff) %>% round(3)) %>%
  arrange(species)


print(
 summary_tab
)


#write.csv(summary_tab, 'summary_tab.csv')
summary_tab %>% 
  ungroup() %>% 
  filter(scenario == 'Current', era != 'Current') %>%
  select(-scenario) %>% 
  write.csv('docs/future_flows/Table3.csv')


x %>% 
  group_by(species, scenario, climate, era) %>%
  filter(scenario == 'Current') %>%
  summarize(sd = sd(n),
            mean = mean(n),
            cv = sd/mean) %>%
  write_csv('docs/future_flows/cv_spawner_abundance.csv')
  


p6 <- x %>%
  filter(scenario == 'Current') %>%
  ggplot(aes(era,perc_diff, fill = climate)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.1), alpha = 0.1) +
  facet_wrap(~species) +
  cowplot::theme_half_open() +
  #theme_bw() +
  theme(legend.position = c(0.2, .07),
        legend.direction = 'horizontal',
        legend.background = element_rect(color = 'black', fill = 'white'),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(color = "grey80")) +
  scale_fill_manual(values = c('white','grey80','grey50')) +
  #scale_fill_grey() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = 'Change in spawners (%)', fill = NULL)

ggsave('docs/future_flows/Fig6_boxplot.tiff', p6, height = 5, width = 6, dpi = 300, compression = 'lzw')

print(p6)


######################### Extra figures showing current vs other scenarios



x %>%
  ggplot(aes(scenario,perc_diff, color = climate)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.1), alpha = 0.3) +
  facet_grid(species~era) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = 'Spawner Change from Current', color = NULL)

ggsave('docs/future_flows/FigExtra_boxplot.tiff', height = 5, width = 6, dpi = 300, compression = 'lzw')


x %>%
  filter(#scenario == 'Current',
         climate != 'RCP 4.5',
         era != 'Mid-century') %>%
  ggplot +
  cowplot::theme_half_open() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none") +
  geom_line(aes(year, perc_diff, color = era)) +
  facet_grid(species~scenario) +
  scale_y_continuous(labels = scales::label_percent(),
                     breaks = c(-1, -.5, 0, 0.5),
                     limits = c(-1, 0.5)) +
  scale_color_manual(values = c('black', 'grey40')) +
  labs(y = 'Change in spawners (%)', x = 'Model Year', color = NULL)

ggsave('docs/future_flows/FigExtra_timeseries.tiff', height = 5, width = 6, dpi = 300, compression = 'lzw')


x %>%
  filter(scenario == 'Current',
         climate != 'RCP 4.5',
         era != 'Mid-century') %>%
  ggplot +
  cowplot::theme_half_open() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none") +
  geom_line(aes(year, n, color = era)) +
  facet_wrap(~species, ncol = 1, scales = 'free_y') +
  scale_y_continuous(labels = scales::label_comma()) +
  #                    breaks = c(-1, -.5, 0, 0.5),
  #                    limits = c(-1, 0.5)
  expand_limits(y = 0) +
  scale_color_manual(values = c('black', 'grey40')) +
  labs(y = 'Spawner abundance', x = 'Model Year', color = NULL)
