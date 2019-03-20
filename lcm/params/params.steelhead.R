# Parameters for Chehalis steelhead


#Initial conditions for the model ------------------------------------------------------------------------------------------------

pop.init <- 8700 

respawners.init <- rep(50, 19)


# Fecundity ----------------------------------------------------------------------------------------------------------------------
fecund.first <- 4000 # First time spawner fecundity
fecund.respawn <- 5200 # Respawner fecundity

# fecundity factor
fecund.fac <- 1.00 



#Egg capacity --------------------------------------------------------------------------------------------------------------------
# Values calculated using GIS
#
egg.cap.adj <- 1.0 #



# Egg-fry survival ---------------------------------------------------------------------------------------------------------------
# Values calculated using GIS

egg.fry.surv.adj <- 1.0 #egg to fry survival adjustment



#Proportions of migrants ----------------------------------------------------------------------------------------------------------
# Steelhead are spring migrants
prop.fry.migrants <- c(rep(0, num.reaches))
prop.sub.migrants <- c(rep(0, num.reaches))
prop.sp.yr <- c(rep(1, num.reaches))



# Fry to migrants survival & capacity (summer) --------------------------------------------------------------------------------------------
# Values calculated in GIS
sub.yr.surv.adj <- 1.0 #summer surv, sub-yearling adjustment
sub.yr.cap.adj <- 1.0 #summer capacity adjustment



# Parr to smolt survival & capacity (Overwinter) --------------------------------------------------------------------------------------------
# Values calculated in GIS
ow.surv.adj <- 1.0 #winter surv, parr to smolt adjustment
ow.cap.adj <- 1.0 #winter capacity adjustment

# Age1 smolts [Larsen and Ward (1955) scale analysis]
prop.age1.smolts <- 0.10


# Delta survivals and capacities --------------------------------------------------------------------------------------------------
# Included in Bay survivals




# Bay survival ------------------------------------------------------------------------------------------------------------
# Capacity is assumed unlimited (transient)
# stochastic survival between min and max

bay.surv <- 0.30

bay.surv.adj <- 1




# Post-s3 ocean survival ---------------------------------------------------------------------------------------------------
# stochastic survival between min and max

So <- 0.80# ocean survival after first year
so.min <- 0.80
so.max <- 0.80

# Lowering of survival for age 1 smolts
age1.ocean.correction <- 1


# Propensity to return from the ocean as age bx ----------------------------------------------------------------------------


# Return from ocean to spawn probabilities, first time spawners
#  [note: assume 'respawn.return' governs return rate of respawners (see farther below)]
# Used agecomposition.xlxs to calibrate b values to match age structured return

b3 <- 0.05 # propensity to return as 3 year olds
b4 <- 0.4 # propensity to return as 4 year olds
b5 <- 0.6 # propensity to return as 5 year olds
# Any age 6 that still hasn't spawned will return


# Pre harvest SAR
so.weighted <- 
  So +
  So^2 * b3 +
  So^3 * b4 +
  So^4 * b5 +
  So^5 * (1 - (b3 + b4 + b5))


SAR <- bay.surv * so.weighted %>% round(3)



# Harvest rate -------------------------------------------------------------------------------------------------------------
Hr <- 0 #.16 # latest estimate (from Eric Walther, May 2017; average)
# harvest adjustment
hr.adj <- 1.0 # rep(1.0, num.reaches)



# Upstream survival ------------------------------------------------------------------------------------------------------
# Calculated in the hab model using barriers
S.up <- 1.0
S.up.adj <- 1.0



# prespawn survival ------------------------------------------------------------------------------------------------------
S.sb <- 1#0.90 # 1.0



# Respawn rate ------------------------------------------------------------------------------------------------------------
# spawners that outmigrate back to ocean as kelts, from Clakamas River estimates and other Willamette populations;
# Howell et al. (1985)
respawn.rate <- 0.80*0.50
# 
# reconditioning of kelts in ocean
kelt.recond <- 0.60
 
# kelts from ocean to returning respawners
respawn.return <- 0.50

# product of all above correspond to a rate of 0.12;
# Clemens (2015) suggests that coastal populations generally have a higher iteroparity rate than non-coastal, this
# is in the range for coastal populations

# Condensed kelt rate for simplicity
kelt.rate <- respawn.rate * kelt.recond * respawn.return

fecund.fac <- 1.0

