# Create Pn and Cn metrics
# Fit models to spawner-recruits points

###### Warning ####
# Not setup to work with chinook yet
# model only loops 3 times for coho jacks and 3yo
########################

#################################################
#### TO RUN THIS SCRIPT FIRST RUN LCM.sim.R #####
#################################################

# Create folder to store all of these outputs ---
outputs_lcm.sr <- paste0(outputs_lcm,"/spawner-recruit")
if (dir.exists(outputs_lcm.sr) == F) {dir.create(outputs_lcm.sr)}

# Create data ----
# This is where we run the subbasin func with a bunch of n.init values

# Initializing pop
n.init <- c(1:9 %o% 10^(2:6)) #seq(10, 1e5, by = 50)


# Runs
runs <- length(n.init)

# Years
years <- 3

# Build the array to hold the output data
model.all.test <- array(
  NA,
  c(
    runs,
    years,
    length(lifestages) + 1,
    num.reaches,
    length(scenario.file)
  ),
  dimnames = list(
    1:runs, 
    1:years, 
    c(lifestages, 'n.init'), 
    reach.names, 
    scenario.file
  )
)

# Reset the matrix
N.test <- matrix(
  NA, 
  nrow = length(lifestages) + 1, 
  ncol = num.reaches, 
  dimnames = list(
    c(lifestages, 'n.init'),
    reach.names) 
) 


# Proportional allocation of n.init into subbasins
# Use proportion from Current scenario
p.init <- read.csv(file.path(hab.path,
                             'Current.csv'),  # Current scenario
                header = TRUE
                ) %>%
  select(-X) %>%
  gather(basin, value, 2:64) %>%
  filter(stage_nm == 'adults') %>%
  mutate(p.init = value/sum(value, na.rm = T)) %>%
  pull(p.init)

p.init[is.na(p.init)] <- 0


# Capture all prespawn productivies for later use

filename <- dir(hab.path,full.names = T)

all.scenarios <- filename %>%
  map_dfr(read.csv, h = T, .id = 'name') %>%
  mutate(scenario = filename[as.numeric(name)] %>% 
           basename %>%
           gsub('_','\\.',.) %>%
           sub('\\.201.*','',.) %>%
           sub('.csv', '', .)
         ) %>%
  select(-name, -X)

colnames(all.scenarios) <- c('stage_name', reach.names, 'scenario')

prespawn_surv <- all.scenarios %>%
  gather(Subbasin,value,reach.names) %>%
  filter(stage_name == 'prespawn_surv') %>%
  rename(prespawn_surv = value) %>%
  select(scenario, Subbasin, prespawn_surv)

#prespawn_surv[is.na(prespawn_surv)] <- 0


# Loop the subbasin func ----


for (n in 1:length(scenario.file)) { # loop across hab scenarios
  
  #Assign variables from habitat scenario files
  source("lcm/scripts/assign.dat.R")
  
  for (i in 1:runs) {
    spawner.init <- n.init[i] * p.init
    N.test['spawners',] <- spawner.init 
    
    for (k in 1:years) {
      N.test <- subbasin(mat = N.test) %>%
        rbind(., spawner.init)
      model.all.test[i, k, , , n] <- N.test
    }
  }
}



# Clean up data and create spawners - recruits ----
prod.stages <- c('ocean1','ocean2', 'spawners', 'n.init', 'natal.smolts','non.natal.smolts')

# This dataframe has S-R values
sr_dat <- model.all.test[, , prod.stages, , ] %>%
  round(0) %>% # round to whole fish
  as.data.frame.table() %>%
  rename(
    runs = Var1,
    year = Var2,
    lifestage = Var3,
    Subbasin = Var4,
    scenario = Var5,
    n = Freq
  ) %>%
  spread(lifestage, n) %>%
  arrange(scenario, n.init, Subbasin, year) %>%
  group_by(scenario, n.init, Subbasin) %>%
  mutate(total.run = (lead(ocean1) * b2 + lead(ocean2, 2))) %>%
  ungroup() %>%
  left_join(prespawn_surv) %>%
  mutate(recruits = total.run * prespawn_surv) %>%
  filter(year == 1,
         recruits >= 1,
         n.init >= 1) %>%
  select(runs, scenario, Subbasin, n.init, recruits, natal.smolts, non.natal.smolts)


# Define functions ----

library(FSA)

library(broom) # allows for call of tidy() equation


# Get the correct form of the BH out of the FSA package
# rFuns(type = "BevertonHolt", param = 2)
bh.eq <- log(recruits) ~ log(a * S/(1 + a * S/Rp)) 

# Create func for map() call later
create_nls_starts <- function(df) {
  # Function to find start values for nls()
  # FSA::srStarts() is the recommended way
  # srStarts(
  #   recruits ~ S, 
  #   data = df,
  #   type = "BevertonHolt",
  #   param = 2,
  #   fixed = list(a = 1))
  
  # NOTE: srStarts() returning negative Rp values,
  # instead forcing guess to be max of recruits
  
  cap <- max(df$recruits)
  
  return(list(a = 1, Rp = cap))
}

# Function to fit the beverton holt to the data
fit.bh  <- . %>%
  mutate(starts = map(data, possibly(create_nls_starts , NA)),
         bh_mod = map2(data, starts, possibly(
           ~nls(
             bh.eq, 
             data = .x, 
             start = .y,
             lower = c(a = 1),
             algorithm = 'port'
           ),
           otherwise = NA)),
         bh_mod_clean = bh_mod %>% map(possibly(tidy, NA))) #possibly() means skip the NAs


# Create list of 3 nested dfs ----

bh.dat.nested <- list(
  
  # 1. Spawner-recruit data at the subbasin-sceanrio level
  sr_dat %>%
    ungroup %>%
    select(scenario, Subbasin, n.init, recruits) %>%
    rename(spawners = n.init) %>%
    mutate(S = as.numeric(as.character(spawners))) %>%
    group_by(scenario, Subbasin) %>%
    nest(),
  
  # 2. Spawner-recruit data at the EDR-sceanrio level
  sr_dat %>%
    ungroup %>%
    left_join(read.csv('lcm/data/subbasin_names.csv')) %>%
    group_by(scenario, EcoRegion, runs) %>%
    summarize(n.init = sum(n.init, na.rm = T),
              recruits = sum(recruits, na.rm = T)) %>%
    ungroup %>%
    select(scenario, EcoRegion, n.init, recruits) %>%
    rename(spawners = n.init) %>%
    mutate(S = as.numeric(as.character(spawners))) %>%
    group_by(scenario, EcoRegion) %>%
    nest(),
  
  # 3. Spawner-recruit data at the basinwide-sceanrio level
  sr_dat %>%
    ungroup %>%
    group_by(scenario, runs) %>%
    summarize(n.init = sum(n.init, na.rm = T),
              recruits = sum(recruits, na.rm = T)) %>%
    ungroup %>%
    select(scenario, n.init, recruits) %>%
    rename(spawners = n.init) %>%
    mutate(S = as.numeric(as.character(spawners))) %>%
    group_by(scenario) %>%
    nest()
) # End list


# Write the raw S-R data to a csv ----
# sr_dat %>%
#   ungroup %>%
#   select(scenario, Subbasin, n.init, recruits) %>%
#   rename(spawners = n.init) %>%
#   mutate(S = as.numeric(as.character(spawners))) %>%
#   select(-S) %>%
#   write.csv(
#     file.path(
#       outputs_lcm.sr, 
#       paste0(
#         'spawner-recruit_data_subbasin_', 
#         pop, 
#         '.csv'
#       )
#     )
#   )
# 
# sr_dat %>%
#   ungroup %>%
#   group_by(scenario, runs) %>%
#   summarize(n.init = sum(n.init, na.rm = T),
#             recruits = sum(recruits, na.rm = T)) %>%
#   ungroup %>%
#   select(scenario, n.init, recruits) %>%
#   rename(spawners = n.init) %>%
#   mutate(S = as.numeric(as.character(spawners))) %>%
#   select(-S) %>%
#   write.csv(
#     file.path(
#       outputs_lcm.sr, 
#       paste0(
#         'spawner-recruit_data_basinwide_', 
#         pop, 
#         '.csv'
#       )
#     )
#   )

# Fit the data with a BH curve ----

bh.results <- lapply(
  bh.dat.nested,
  fit.bh
)


# Make results look pretty ----

bh.results.subbasin <- bh.results[[1]] %>%
  select(scenario, Subbasin, bh_mod_clean) %>%
  unnest() %>%
  select(scenario, Subbasin, term, estimate) %>%
  filter(!is.na(term)) %>%
  spread(term, estimate) %>%
  rename(Pn = a, Cn = Rp) 

bh.results.edr <- bh.results[[2]] %>%
  select(scenario, EcoRegion, bh_mod_clean) %>%
  unnest() %>%
  select(scenario, EcoRegion, term, estimate) %>%
  filter(!is.na(term)) %>%
  spread(term, estimate) %>%
  rename(Pn = a, Cn = Rp)  

bh.results.basinwide <- bh.results[[3]] %>%
  select(scenario, bh_mod_clean) %>%
  unnest() %>%
  select(scenario, term, estimate) %>%
  filter(!is.na(term)) %>%
  spread(term, estimate) %>%
  rename(Pn = a, Cn = Rp) 
  

# Join data to looping model fish abundance and write a csv

# Store fish abundance
fish.abundance <- model.all[, 50:100, summary.stages, , ] %>%
  apply(.,c(1,3,4,5), geo.mean) %>% # geomean across years
  apply(.,c(2,3,4), mean) %>% # mean of runs
  round(0) %>%# round to whole fish
  as.data.frame.table() %>%
  rename(Subbasin = Var2, scenario = Var3, n = Freq) 

# Write csv per subbasin
fish.abundance %>%
  spread(Var1, n) %>%
  left_join(bh.results.subbasin) %>%
  mutate(Pn = ifelse(spawners == 0, NA, Pn),
         Cn = ifelse(spawners == 0, NA, Cn)) %>%
  write.csv(
    file.path(
      outputs_lcm.sr, 
      paste0(
        'model_outputs_subbasin_', 
        pop, 
        '.csv'
      )
    )
  )
  
# Write csv per EDR
fish.abundance %>%
  left_join(read.csv('lcm/data/subbasin_names.csv')) %>%
  group_by(Var1, scenario, EcoRegion) %>%
  summarize(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  spread(Var1, n) %>%
  left_join(bh.results.edr) %>%
  mutate(Pn = ifelse(spawners == 0, NA, Pn),
         Cn = ifelse(spawners == 0, NA, Cn)) %>%
  write.csv(
    file.path(
      outputs_lcm.sr, 
      paste0(
        'model_outputs_EDR_', 
        pop, 
        '.csv'
      )
    )
  )


# Write csv for basinwide
fish.abundance %>%
  group_by(Var1, scenario) %>%
  summarize(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  spread(Var1, n) %>%
  left_join(bh.results.basinwide) %>%
  mutate(Pn = ifelse(spawners == 0, NA, Pn),
         Cn = ifelse(spawners == 0, NA, Cn)) %>%
  write.csv(
    file.path(
      outputs_lcm.sr, 
      paste(
        'model_outputs_basinwide_', 
        pop, 
        '.csv'
      )
    )
  )





# # Create plots ----
# 
predict_bh <- function(df, p, c) {
  # Create predictions of recruits for each model ----
  stock <- df$S
  x <- BH.func(
    S = stock,
    p = p,
    c = c)

  return(x)
}


# Plt points with B-H lines overlaid ----
# Basinwide

print(
  bh.dat.nested[[3]] %>%
    left_join(bh.results.basinwide) %>%
    mutate(pred = pmap(list(data, Pn, Cn), predict_bh)) %>%
    select(scenario, data, pred) %>%
    unnest %>%
    ggplot +
    geom_point(aes(S, recruits, color = scenario)) +
    geom_line(aes(S, pred, color = scenario)) +
    #facet_wrap(~Subbasin, scales = 'free') +
    labs(x = 'spawners')
)

ggsave(
  file.path(outputs_lcm.sr, 
            paste('spawner-recruit-basinwide_', 
                  pop,
                  '.jpg')
            ),
  width = 20,
  height = 10,
  dpi = 300
)

 

# Zoom in on 4 big subbasins
bh.dat.nested[[1]] %>%
  left_join(bh.results.subbasin) %>%
  mutate(pred = pmap(list(data, Pn, Cn), predict_bh)) %>%
  select(scenario, Subbasin, data, pred) %>%
  unnest %>%
  filter(Subbasin %in% c('Skookumchuck River',
                         'Newaukum River',
                         'Humptulips River',
                         'Satsop River')) %>%
  ggplot +
  geom_point(aes(S, recruits, color = scenario)) +
  geom_line(aes(S, pred, color = scenario)) +
  facet_wrap(~Subbasin, scales = 'free') +
  labs(x = 'spawners')

ggsave(
  file.path(outputs_lcm.sr, 
            paste('spawner-recruit-subset-large_', 
                  pop, '.jpg')
            ),
  width = 10,
  height = 8,
  dpi = 300
)


# Zoom in on 4 small subbasins
bh.dat.nested[[1]] %>%
  left_join(bh.results.subbasin) %>%
  mutate(pred = pmap(list(data, Pn, Cn), predict_bh)) %>%
  select(scenario, Subbasin, data, pred) %>%
  unnest %>%
  filter(Subbasin %in% c('Lincoln Creek',
                         'Dillenbaugh Creek',
                         'Elk Creek',
                         'Upper Chehalis: Above Proposed Dam')) %>%
  ggplot +
  geom_point(aes(S, recruits, color = scenario)) +
  geom_line(aes(S, pred, color = scenario)) +
  facet_wrap(~Subbasin, scales = 'free') +
  labs(x = 'spawners')

ggsave(
  file.path(outputs_lcm.sr, 
            paste('spawner-recruit-subset-small_', 
                  pop, 
                  '.jpg')
            ),
  width = 10,
  height = 8,
  dpi = 300
)



# bh.dat.nested[[2]] %>%
#   left_join(bh.results.edr) %>%
#   mutate(pred = pmap(list(data, Pn, Cn), predict_bh)) %>%
#   select(scenario, EcoRegion, data, pred) %>%
#   unnest %>%
#   ggplot +
#   geom_point(aes(S, recruits, color = scenario)) +
#   geom_line(aes(S, pred, color = scenario)) +
#   facet_wrap(~EcoRegion, scales = 'free') +
#   labs(x = 'spawners')
