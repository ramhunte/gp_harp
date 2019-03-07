# Parameters for Chehalis Coho salmon


#Initial condtions for the model ------------------------------------------------------------------------------------------------
pop.init <- 75000 #initial spawner population




# Fecundity ----------------------------------------------------------------------------------------------------------------------
fecund <- 2500 # Coho = 2500




#Egg capacity --------------------------------------------------------------------------------------------------------------------
# Values calculated using GIS
egg.cap.adj <- 1.0 #



# Egg-fry survival ---------------------------------------------------------------------------------------------------------------
# Values calculated using GIS
egg.fry.surv.adj <- 1.0 #egg to fry survival adjustment


# Fry redistribution -------------------------------------------------------------------------------------------------------------
percent.fry.migrants <- 0.05

# Fry colonization ---------------------------------------------------------------------------------------------------------------
# From Reeves et al. 1989
fry.colonization <- 0.78


# Fry to parr survival & capacity (Summer) ---------------------------------------------------------------------------------------
# Values calculated using GIS
sub.yr.surv.adj <- 1.0 #summer surv, sub-yearling adjustment
sub.yr.cap.adj <- 1.0 #summer capacity adjustment


# Fall redistribution ------------------------------------------------------------------------------------------------------------
# Values are fixed, depending on the scenario
# Current habitat = 11% of fish move, Historical wood = 6% of fish move, Historical ponds = 3% of fish move
redist.current <- 0.11
redist.histwood <- 0.07
redist.histpond <- 0.03



# Parr-to-smolt survival and capacity (Winter) -------------------------------------------------------------------------------------
# Values calculated using GIS
ps.surv.adj <- 1.0 #winter surv, parr to smolt adjustment
ps.cap.adj <- 1.0 #winter capacity adjustment



# Delta survivals and capacities --------------------------------------------------------------------------------------------------
# Delta survivals lumped with bay




# Bay survival ------------------------------------------------------------------------------------------------------------
#Capacity is assumed unlimited (transient)
# Back calculated to get SAR of .04
bay.min <- 0.08#0.07
bay.max <- 0.08#0.09



# Post-s3 ocean survival ---------------------------------------------------------------------------------------------------
# stochastic survival between min and max
#So = 0.70
# Ricker 1976, from Jeff's original model
so.min <- 0.7#0.60
so.max <- 0.7#0.80
ocean.correction <- 1.0




# Propensity to return from the ocean as age bx ----------------------------------------------------------------------------
# Jacking rate
#  the remainder return as age 3
# From Eric Walther WDFW
b2 <- 0.033




# Harvest rate -------------------------------------------------------------------------------------------------------------
# In HarvestRates.xlsx spreadsheet 0.13 since 2000 same recent spreadsheet; long term average = 0.21
# Using harvest rate from Mara's SAR spreadsheet
Hr.cur <- 0#0.31 #current harvest rate
#Hr.hist <- 0.75 #historical harvest rate

# harvest adjustment
hr.adj <- 1.0




# Upstream survival ---------------------------------------------------------------------------------------------------------
# Calculated in the hab model using barriers
S.up <- 1.0
S.up.adj <- 1.0


# Prespawn survival ---------------------------------------------------------------------------------------------------------
S.sb <- 1#0.9



