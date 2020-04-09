# Parameters for Chehalis steelhead


#Initial conditions for the model ------------------------------------------------------------------------------------------------

pop.init <- 8700 

respawners.init <- rep(50, 19)


# Fecundity ----------------------------------------------------------------------------------------------------------------------
fecund.first <- 5400 # First time spawner fecundity
fecund.respawn <- 8000 # Respawner fecundity


# fall redistribution ------------------------------------------------------------------------------------------------
# Percent of fish that move from natal basin down to mainstem in the fall
# See trello card: https://trello.com/c/DjGWaYoX/204-add-fall-movement-for-age-0-steelhead-end-of-first-summer

percent.fall.migrants <- read.csv('lcm/data/Subbasin_names.csv') %>%
  mutate(prcnt_movers = case_when(
    Area_km2 > 450             ~ 0.0,
    between(Area_km2, 50, 450) ~ 0.02,
    Area_km2 < 50              ~ 0.1
  )) %>%
  pull(prcnt_movers)


# spring redistribution ------------------------------------------------------------------------------------------------
# Percent of fish that move from natal basin down to mainstem in the spring
# See memo from Tim (NOAA LCM steelhead juvenile movement 2019-07-17)

percent.spring.migrants <- read.csv('lcm/data/Subbasin_names.csv') %>%
  mutate(prcnt_movers = case_when(
    Area_km2 > 450              ~ 0.0,
    between(Area_km2, 150, 450) ~ 0.2,
    Area_km2 < 150              ~ 0.5
  )) %>%
  pull(prcnt_movers)


# Updated 10/30/2019 per Larry Lestelle. See 1974-2019 Summaries for Queets-Chehalsi-Hump Sth Age
# values adjusted here so age 1-3 smolt composition matches age composition of adults from QIN
# Age 1: 2%, Age 2: 87%, Age 3: 11%

prop.age1.smolts <- 0.003
prop.age2.smolts <- 0.70
prop.age3.smolts <- 1 - prop.age2.smolts - prop.age1.smolts


# Delta survivals and capacities --------------------------------------------------------------------------------------------------
# Included in Bay survivals




# Bay survival ------------------------------------------------------------------------------------------------------------
# Capacity is assumed unlimited (transient)
# stochastic survival between min and max

bay.surv <- 0.14




# Post-s3 ocean survival ---------------------------------------------------------------------------------------------------
# stochastic survival between min and max

So <- 0.80# ocean survival after first year
so.min <- 0.80
so.max <- 0.80


# Propensity to return from the ocean as age bx ----------------------------------------------------------------------------


# Return from ocean to spawn probabilities, first time spawners
#  [note: assume 'respawn.return' governs return rate of respawners (see farther below)]
# Used agecomposition.xlxs to calibrate b values to match age structured return

b3 <- 0.01#0.05 # propensity to return as 3 year olds
b4 <- 0.43#0.4 # propensity to return as 4 year olds
b5 <- 0.76#0.6 # propensity to return as 5 year olds
# Any age 6 that still hasn't spawned will return


# Respawn rate --
# spawners that outmigrate back to ocean as kelts, from Clakamas River estimates and other Willamette populations;
# Howell et al. (1985)
respawn.rate <- 0.80 * 0.50 # rate * sex ratio

# reconditioning of kelts in ocean
kelt.recond <- 0.60

# kelts from ocean to returning respawners
respawn.return <- 0.50

# product of all above correspond to a rate of 0.12;
# Clemens (2015) suggests that coastal populations generally have a higher iteroparity rate than non-coastal, this
# is in the range for coastal populations

# Condensed kelt rate for simplicity
kelt.rate <- respawn.rate * kelt.recond * respawn.return


# Harvest rate -------------------------------------------------------------------------------------------------------------
Hr <- 0 #.16 # latest estimate (from Eric Walther, May 2017; average)








