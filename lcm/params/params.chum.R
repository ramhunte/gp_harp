# Parameters for Chehalis chum salmon


#Initial conditions for the model ------------------------------------------------------------------------------------------------

pop.init <- 10000



# Fecundity ---------------------------------------------------------------------------------------------------------------------
fecund <- 3200



#Egg capacity -------------------------------------------------------------------------------------------------------------------

egg.cap.adj <- 1.0


# Egg-fry survival --------------------------------------------------------------------------------------------------------------

egg.fry.surv.adj <- 1.0 # egg to fry survival adjustment


# Fry to migrants survival and capacity -----------------------------------------------------------------------------------------

fry.surv.adj <- 1



# Bay survival ------------------------------------------------------------------------------------------------------------------
# Capacity is assumed to be unlimited (transient)
# stochastic survival between min and max

bay.surv <- .13


bay.range <- c(bay.surv, bay.surv)#c(bay.surv*.8, bay.surv*1.2)

bay.surv.adj <- 1



# Post-s3 ocean survival ---------------------------------------------------------------------------------------------------

# Age structured ocean survivals
# so.1 <- .6
# so.2 <- .7
# so.3 <- .8
# so.4 <- .9
# so.5 <- .9

# stochastic survival between min and max
# min and max for each year in the ocean
so.1 <- c(.28, 0.28)#c(0.5,0.7)
so.2 <- c(0.7, 0.7)#c(0.6,0.8)
so.3 <- c(0.8, 0.8)#c(0.7,0.9)
so.4 <- c(0.9, 0.9)#c(0.8,1)
so.5 <- c(0.9, 0.9)


ocean.correction <- 1.0



# Propensity to return from the ocean as age bx ----------------------------------------------------------------------------
# Modified from Corey's paper. Table 2
# Used ageconmposition.xlxs to match values

b2 <- 0
b3 <- 0.15
b4 <- 0.85
b5 <- 1

# All age 6 return

# Pre harvest SAR
age0 <- 1

age1 <- age0 * so.1
age2 <- age1 * so.2 * (1)
age3 <- age2 * so.3 * (1 - b3)
age4 <- age3 * so.4 * (1 - b4)


age3_return <- age2 * b3
age4_return <- age3 * b4
age5_return <- age4

returns <- age3_return + age4_return + age5_return

so_w <- returns / age0

SAR.fry <- (so_w * bay.surv)[1]


# Harvest rate -------------------------------------------------------------------------------------------------------------

Hr <- 0#.056 #  latest estimate from harvest spreadsheet (from Eric Walther, May 2017; average)

# harvest adjustment
hr.adj <- 1.0 # rep(1.0, num.reaches)




# Upstream survival ---------------------------------------------------------------------------------------------------------
# USGS paper on Spring Chinook radio tracking (tagged 12 fish and 1 died)
# Calculated in the hab model using temperature and barriers
S.up.curr <- 1.0  
S.up.hist <- 1.0

S.up.adj <- 1.0


# Prespawn survival ---------------------------------------------------------------------------------------------------------
S.sb <- 1.0

