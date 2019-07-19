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


# Write to the abundance CSV
abundance_by_subbasin %>%
  left_join(df.sr) %>%
  mutate(Cn = (spawners * Pn) / (Pn - 1),
         Cn = ifelse(Cn > 0, Cn, 0),
         Pn = ifelse(Pn > 1, Pn, 0)) %>%
  write.csv(file.path(outputs_lcm, csv.name))

