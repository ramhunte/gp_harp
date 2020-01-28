# Parameters for Chehalis Fall Chinook salmon


#Initial condtions for the model ------------------------------------------------------------------------------------------------

pop.init <- 20000 



# Fecundity ----------------------------------------------------------------------------------------------------------------------
fecund <- 5400


# Bay survival ------------------------------------------------------------------------------------------------------------
# Capacity is assumed unlimited (transient)
# stochastic survival between min and max

# Values including delta
bay.parr.surv <- .061 # Delta and bay for parr (freshwater rearing)
bay.fry.surv <- .001 # Freshwater outmigration, delta and bay for fry


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

b2 <- 0.008
b3 <- 0.07
b4 <- 0.35
b5 <- 0.85


# Pre harvest SAR

# Weighted average ocean survival
#so_w <- 0.3210761737

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

Hr <- 0#.36 # latest estimate (from Eric Walther, May 2017; average)



# Upstream survival ---------------------------------------------------------------------------------------------------------
# Calculated in the hab model using barriers
S.up.curr <- 1.0
S.up.hist <- 1.0

S.up.adj <- 1.0


# Prespawn survival ---------------------------------------------------------------------------------------------------------
S.sb <- 1.0

