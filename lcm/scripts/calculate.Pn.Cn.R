# The purpose of this script is to calculate the lifecycle productivty per subbasin
# Method: 
# 1. Initialize the subbasin() function with 1 fish in every basin the first year, 0 fish after that
# 2. Run for 10 generations, which will have all fish return from any species (oldest steelhead is 8)
# 3. Count spawners from each subbasin (density independent productivity)

# Future thoughts: To get a basin wide Pn find the slope from the origin (0,0) to 
# the sum of spawners (x) vs the sum of returns (y) [sum(returns)/sum(spawners)]



# Input params ----

sr.init <- 1 # What abundance to start with?
sr.years <- 10 # How many generations

csv.name <- paste0(pop, '_abundance_by_subbasin.csv')

# Lifestages for clean output
if (pop == 'coho') {summary.stages <- c('spawners','natal.smolts','non.natal.smolts')}

if (pop == "fall.chinook" | pop == "spring.chinook") { summary.stages <- c('spawners','fry.migrants','sub.yr')}

if (pop == 'steelhead') {summary.stages <- c('spawners','age1.smolts','age2.smolts')}

# Initialize arrays to capture data ----

N.sr <- matrix(
  0, 
  nrow = length(lifestages), 
  ncol = num.reaches, 
  dimnames = list(
    lifestages,
    reach.names) 
) 


model.all.sr <- array(
  NA,
  c(
    sr.years,
    length(lifestages),
    num.reaches,
    length(scenario.file)
  ),
  dimnames = list(
    1:sr.years, 
    lifestages, 
    reach.names, 
    scenario.file
  )
)


# Run model 7 generations for each scenario ----

for (n in 1:length(scenario.file)) { # loop across hab scenarios
  
  #Assign variables from habitat scenario files
  source("lcm/scripts/assign.dat.R")
  
  N.run <- N.sr
  spawner.init <- sr.init
  N.run['spawners',] <- spawner.init 
    
  for (k in 1:sr.years) {
    
    N.run <- subbasin(mat = N.run)
    model.all.sr[k, , , n] <- N.run
    N.run['spawners',] <- 0
  }
}


df.sr <- model.all.sr[ , "spawners", , ] %>%
  apply(., c(2,3), sum) %>%
  as.data.frame.table() %>%
  rename(natal.basin = Var1, scenario = Var2, Pn = Freq) %>%
  mutate(Pn = Pn/sr.init)


# Pn and Cn by subbasin
abundance_by_subbasin %>%
  select(scenario, natal.basin, summary.stages) %>%
  mutate_at(vars(summary.stages), list(~round(., 0))) %>% # round to whole fish 
  left_join(df.sr) %>%
  mutate(Cn = (spawners * Pn) / (Pn - 1),
         Cn = ifelse(Pn < 1, NA, Cn)) %>%
  #mutate_at(vars(summary.stages), list(~ifelse(Pn < 0, 0, .))) %>%   # Zero out abundance when Pn < 0  
  write.csv(file.path(outputs_lcm, csv.name))


# Pn and Cn basinwide
read.csv(file.path(outputs_lcm, csv.name)) %>%
  select(-X) %>%
  group_by(scenario) %>%
  summarize(basinwide_spawners = sum(spawners),
            Pn = weighted.mean(Pn, spawners)) %>%
  mutate(Cn = (basinwide_spawners * Pn) / (Pn - 1)) %>%
  write.csv(file.path(outputs_lcm, paste0(pop, '_abundance_basinwide.csv')))


# Pn and Cn by EDR
read.csv(file.path(outputs_lcm, csv.name)) %>%
  select(-X) %>%
  left_join(read.csv('lcm/data/Subbasin_names.csv') %>%
              rename(natal.basin = Subbasin)) %>%
  group_by(scenario, EcoRegion) %>%
  summarize(basinwide_spawners = sum(spawners),
            Pn = weighted.mean(Pn, spawners)) %>%
  mutate(Cn = (basinwide_spawners * Pn) / (Pn - 1)) %>%
  write.csv(file.path(outputs_lcm, paste0(pop, '_abundance_by_EDR.csv')))
