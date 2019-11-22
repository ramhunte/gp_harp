# Parameters for Chehalis Coho salmon


#Initial condtions for the model ------------------------------------------------------------------------------------------------
pop.init <- 75000 #initial spawner population



# Fecundity ----------------------------------------------------------------------------------------------------------------------
fecund <- 2500 # Coho = 2500


# Fry redistribution -------------------------------------------------------------------------------------------------------------
percent.fry.migrants <- 0.05

# Fry colonization ---------------------------------------------------------------------------------------------------------------
# From Reeves et al. 1989
fry.colonization <- 0.78


# Fall redistribution ------------------------------------------------------------------------------------------------------------
# Values are fixed, depending on the scenario
# Current habitat = 11% of fish move, Historical wood = 6% of fish move, Historical ponds = 3% of fish move
redist.current <- 0.11
redist.histwood <- 0.07
redist.histpond <- 0.03


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
Hr <- 0




# Upstream survival ---------------------------------------------------------------------------------------------------------
# Calculated in the hab model using barriers
S.up <- 1.0
S.up.adj <- 1.0



