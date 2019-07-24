# Parameters for Chehalis spring Chinook salmon


#Initial condtions for the model ------------------------------------------------------------------------------------------------

pop.init <- 3000 



# Fecundity ----------------------------------------------------------------------------------------------------------------------
fecund <- 5400



#Egg capacity --------------------------------------------------------------------------------------------------------------------
# Values calculated using GIS

egg.cap.adj <- 1.0 #



# Egg-fry survival ---------------------------------------------------------------------------------------------------------------
# Values calculated using GIS

egg.fry.surv.adj <- 1.0 #egg to fry survival adjustment



# Fry to migrants survival & capacity (summer) --------------------------------------------------------------------------------------------
# Values calculated in GIS

sub.yr.surv.adj <- 1 #summer surv, sub-yearling adjustment
sub.yr.cap.adj <- 1.0 #summer capacity adjustment. x3 to represent 3 cohorts of fish emerging over ~24 weeks and staying for ~8 weeks

fry.surv.adj <- 1 



# Delta survivals and capacities --------------------------------------------------------------------------------------------------




# Bay survival ------------------------------------------------------------------------------------------------------------
# Capacity is assumed unlimited (transient)
# stochastic survival between min and max

# Values including delta
bay.parr.surv <- .062
bay.fry.surv <- .001

bay.parr.range <- c(bay.parr.surv, bay.parr.surv)#c(bay.parr.surv*.8, bay.parr.surv*1.2)
bay.fry.range <- c(bay.fry.surv, bay.fry.surv)#c(bay.fry.surv*.8, bay.fry.surv*1.2)

bay.parr.surv.adj <- 1
bay.fry.surv.adj <- 1





# Post-s3 ocean survival ---------------------------------------------------------------------------------------------------

# Age structured ocean survivals
# so.1 <- .6
# so.2 <- .7
# so.3 <- .8
# so.4 <- .9
# so.5 <- .9

# stochastic survival between min and max
# min and max for each year in the ocean
so.1 <- c(0.6, 0.6)#c(0.5,0.7)
so.2 <- c(0.7, 0.7)#c(0.6,0.8)
so.3 <- c(0.8, 0.8)#c(0.7,0.9)
so.4 <- c(0.9, 0.9)#c(0.8,1)
so.5 <- c(0.9, 0.9)#c(0.8,1)


ocean.correction <- 1.0



# Propensity to return from the ocean as age bx ----------------------------------------------------------------------------
# Modified from Corey's paper. Table 2
# Used ageconmposition.xlxs to match values

b2 <- 0.005
b3 <- 0.097
b4 <- 0.6
b5 <- 0.8

# All age 6 return

# Age compositon of returns (spreadsheet from Jeff)
# Different than propensity to return

a2 <- .02
a3 <- .17
a4 <- .59
a5 <- .22


# Pre harvest SAR
age0 <- 1

age1 <- age0 * so.1
age2 <- age1 * so.2 * (1 - b2)
age3 <- age2 * so.3 * (1 - b3)
age4 <- age3 * so.4 * (1 - b4)
age5 <- age4 * so.5 * (1 - b5)

age2_return <- age1 * b2
age3_return <- age2 * b3
age4_return <- age3 * b4
age5_return <- age4 * b5
age6_return <- age5

returns <- age2_return + age3_return + age4_return + age5_return + age6_return

so_w <- returns / age0

SAR.parr <- (so_w * bay.parr.surv)[1]
SAR.fry <- (so_w * bay.fry.surv)[1]


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

