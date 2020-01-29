
# Chehalis LCM functions


# Check to see if a package is installed, install it if not ----
pkgCheck <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if (!require(x,character.only = TRUE)) 
      stop(paste0("Packages ", x, " not found"))
  }
}


# Geometric mean ----
geo.mean <- function(x){
  x[which(x <= 0)] <- 0.001
  exp(mean(log(x)))
}

# Spawners to eggs function ----
eggs.func <- function(NOR.total, egg.total, fecund){
  num.eggs <- ifelse(NOR.total * fecund * 0.5 <= egg.total, 
                     NOR.total * fecund * 0.5,
                     egg.total)
  num.eggs
}


# Beverton holt function ----
BH.func <- function(S, p, c){
  # generic Beverton-Holt
  # S = parental state
  # p = productivity or survival
  # c = capacity
  p[which(p == 0)] <- 0.001
  c[which(c == 0)] <- 1
  S[which(S == 0)] <- 1
  recruits <- (S  * p) / (1 + (p / c) * S)
  recruits[recruits < 1] <- 0 # cleans up basins with less than 1 fish
  recruits
}


# Flow recurrance impact on egg to fry survival ----
# Functions to create multiplier for year to year
# variability from flow in egg-fry survival

# Inverse logit function
# helper function called by another function
inv.logit <- function(x){
  # transforms x on logit back to a survival
  temp <- exp(x)/(exp(x) + 1)
  temp
}

# Change rescale egg survival to 0:1 scale
# helper function called by another function
range.rescale <- function(x, min = 0.005554976, max = 0.1322917) {
  # rescales x the range of predictions,
  # where x is the inv.logit from a fitted 
  # logit(surv) to recurrence interval relationship:
  # logit(surv) ~ -1.88084588 - 0.05511075 * peak.winter.flow
  # from Skagit River Age0+ Chinook salmon
  # egg-outmigrating juveniles data, rescaled to
  # 0 to 1 scale, with the minimum surv of 0.005554976 
  # at high winter flows, and
  # a maximum surv of 0.1322917 at low winter flows

  temp <- (x - min) / diff(range(c(min, max))) * 1
  temp
}

# Egg survival as a function of flow recurrence
# called within subbasin() function
# at the egg --> fry line, as in:
# pre.fry <- eggs * egg.fry.surv * egg.flow.dec() # Eggs --> freshly emerged fry
if (run_stochastic_eggtofry == 'yes') {
  egg.flow.dec <- function(q){
    # requires inv.logit(), range.rescale()
    temp.decl <- range.rescale(inv.logit(-1.88084588 - 0.05511075 * q))
    temp.decl  
  }
} else {
  egg.flow.dec <- function() {
    1
  }
}



# Bay survival function ----
bay.surv.func <- function(min, max){
  s.bay <- runif(n = 1, min, max)
  s.bay
}



# Marine survival function ----
So.func <- function(min , max){
  so <- runif(n = 1, min, max)
  so
}



# Function to distribute fish from natal stream into mainstem ----
distribute.fish <- function(fish.in, move.matrix){
  fish <- distribute.matrix

  move.into.rows <- grep("movers into", row.names(fish))
  return.percent.rows <- grep("return percent", row.names(fish))
  
  fish["before_movement", ] <- fish.in
  fish[move.into.rows, ] <- sweep(move.matrix,MARGIN = 2,fish[1,],`*`) # multiply fish by move.matrix
  fish[is.nan(fish)] <- 0
  fish["after_movement", trib.reaches] <- fish["before_movement", trib.reaches] - colSums(fish[move.into.rows, trib.reaches]) 
  fish["after_movement", ms.reaches]   <- fish["before_movement", ms.reaches]   + rowSums(fish[move.into.rows, trib.reaches])
  fish[return.percent.rows, trib.reaches] <- fish[move.into.rows, trib.reaches]  / fish["after_movement", ms.reaches]
  fish[is.nan(fish)] <- 0 # Fix 0/0 NaNs
  
  fish
}


# Function to redistribute fish from mainstem back to natal streams ----
reallocate.fish <- function(fish.in = 'row 2 of output from distribute.fish()',
                              redist.matrix = 'return percent from distribute.fish() function'){
  f <- redistribute.matrix
  
  move.from.rows <- grep("returns from", row.names(f))
  
  f["before_movement", ] <- fish.in
  f[move.from.rows, ] <- sweep(redist.matrix,MARGIN = 1, f['before_movement', ms.reaches], `*`)
  f["after_movement", trib.reaches] <- f["before_movement", trib.reaches] + colSums(f[move.from.rows, trib.reaches])
  f["after_movement", ms.reaches] <- f["before_movement", ms.reaches] - rowSums(f[move.from.rows, trib.reaches])
  
  f
}


# Main coho  function ----
#Lifestages
# 1)  Eggs
# 2)  Pre Fry
# 3)  Fry (after colonization)
# 3)  Parr
# 4)  Smolts
# 5)  Ocean0 mat[1],
# 6)  Ocean1 mat[2,]
# 7)  Ocean2 mat[3,]
# 8)  Total Run
# 9)  Spawners

if (pop == "coho") {
  
  subbasin <- function(mat = N, ...){
    
    NOR.total <- mat['spawners', ]
    
    eggs <- eggs.func(NOR.total, egg.total = egg.cap, fecund = fecund) # Hockey stick
    #eggs <- BH.func(S = NOR.total, p = fecund/2, c = egg.cap) # B-H
    pre.fry <- eggs * egg.fry.surv * ef_flow# Eggs --> freshly emerged fry
    
    # Spring distribution
    fry.distributed <- distribute.fish(fish.in = pre.fry, move.matrix = move.matrix.spring * percent.fry.migrants)
  
    parr <- BH.func(fry.distributed['after_movement', ] * fry.colonization, p = parr.surv, c = parr.cap)# summer parr
    parr.distributed <- distribute.fish(fish.in = parr, sweep(move.matrix, MARGIN = 2, redist, '*')) # Fall distribution into mainstem fixed by scenario
    
    smolts <- BH.func(parr.distributed['after_movement', ], p = parr.smolt.surv, c = parr.smolt.cap)
    
    # Redistribution of migrants back to natal subbasins
    smolts.redistributed.parr <- reallocate.fish(fish.in = smolts,
                                                   redist.matrix = parr.distributed[return.rows, ]) # Redistribute to natal basins
    smolts.redistributed.fry <- reallocate.fish(fish.in = smolts.redistributed.parr['after_movement', ],
                                                   redist.matrix = fry.distributed[return.rows, ]) # Redistribute to natal basins
   
    # Ocean stages
    mat.new['ocean0', ] <- smolts.redistributed.fry['after_movement', ] * bay.surv.func(bay.min, bay.max) * ocean.correction # ocean0
    mat.new['ocean1', ] <- mat['ocean0', ] * So.func(so.min, so.max) # ocean1
    mat.new['ocean2', ] <- mat['ocean1', ] * (1 - b2) * So.func(so.min, so.max) # ocean2, 'spawners' pre harvest
    
    # Other stored stages
    mat.new['eggs', ] <- eggs
    mat.new['pre.fry', ] <- pre.fry 
    mat.new['fry', ] <- fry.distributed['after_movement', ] * fry.colonization #fry 
    mat.new['parr', ] <- parr 
    mat.new['smolts', ] <- smolts
    mat.new['total.run', ] <- (b2*mat['ocean1', ] + mat['ocean2', ]) # Total run
    mat.new['spawners', ] <- mat.new['total.run', ] * S.up * (1 - Hr) #spawners
    
    mat.new['fry.movers', ] <- fry.distributed['after_movement', ] - fry.distributed['before_movement', ]
    mat.new['parr.movers', ] <- parr.distributed['after_movement', ] - parr.distributed['before_movement', ]
    mat.new['natal.smolts', ] <- c(smolts[trib.reaches], smolts.redistributed.fry['after_movement', ms.reaches])
    mat.new['non.natal.smolts', ] <- c(smolts.redistributed.fry['after_movement', trib.reaches] - smolts[trib.reaches],
                                      smolts[ms.reaches] * 0)
    
    N <- mat.new
    N
    
  } #End sub basin function 
} # end if coho



# Main Chinook function -----
#Lifestages
# 1)  Eggs
# 2)  Fry
# 3)  Parr migrants
# 4)  Fry migrants
# 5)  Ocean0 mat[1],
# 6)  Ocean1 mat[2,]
# 7)  Ocean2 mat[3,]
# 8)  Ocean3 mat[4,]
# 9)  Ocean4 mat[5,]
# 10)  Ocean5 mat[6,]
# 11)  Total Run
# 12)  Spawners


if (pop == "fall_chinook" | pop == "spring_chinook") {
  
  subbasin <- function(mat = N, ...){
    
    NOR.total <- mat['spawners',]
    #NOR.total <- N["spawners",] # Run this line to be able to step through func
    
    eggs <- eggs.func(NOR.total, egg.total = egg.cap, fecund = fecund) # Number of eggs in adults
    #eggs <- BH.func(S = NOR.total, p = fecund/2, c = egg.cap) # B-H
    pre.fry <- eggs * egg.fry.surv * ef_flow
    
    # Natal fry - All basins
    natal.fry <- BH.func(S = pre.fry, p = weekly.surv^1, c = cap * 2) # Density dependent survival in fresh (first week after fry), 3x capacity
    
    # Non natal fry 
    fry.migrants <- (pre.fry * weekly.surv) - natal.fry # Density dependent slice to create non natal fry
    
    # Natal sub yearlings - all basins
    natal.fry.distrib <- distribute.fish(fish.in = natal.fry, move.matrix = move.matrix.w.natal)
    sub.yr.distrib <- BH.func(S = natal.fry.distrib['after_movement', ], p = weekly.surv.temp^11, c = cap * 2)
    sub.yr <- reallocate.fish(fish.in = sub.yr.distrib,
                              redist.matrix = natal.fry.distrib[return.rows, ])['after_movement', ]
    
    
    # Apply bay survival (ds migration, delta, bay, nearshore productivity)
    fry.migrants.bay <- fry.migrants * bay.fry.surv
    sub.yr.bay <- sub.yr * bay.parr.surv
    
    # Ocean survival
    mat.new['ocean0', ] <- fry.migrants.bay + sub.yr.bay # smolts leaving bay, ocean0
    mat.new['ocean1', ] <- mat[1, ] * So.func(so.1[1], so.1[2]) # 2 year olds, ocean1
    mat.new['ocean2', ] <- mat[2, ] * (1 - b2) * So.func(so.2[1], so.2[2]) # 3 year olds, ocean2
    mat.new['ocean3', ] <- mat[3, ] * (1 - b3) * So.func(so.3[1], so.3[2]) # 4 year olds, ocean3
    mat.new['ocean4', ] <- mat[4, ] * (1 - b4) * So.func(so.4[1], so.4[2]) # 5 year olds, ocean4
    mat.new['ocean5', ] <- mat[5, ] * (1 - b5) * So.func(so.5[1], so.5[2]) # 6 year olds, ocean5
    
    mat.new['total.run',] <- (b2 * mat['ocean1', ] + b3 * mat['ocean2', ] + b4 * mat['ocean3', ] + b5 * mat['ocean4',] + mat['ocean5', ]) #Total run
    mat.new['spawners',] <- mat.new['total.run', ] * S.up * (1 - Hr) # Spawners
    
    mat.new['eggs', ] <- eggs
    mat.new['pre.fry', ] <- pre.fry 
    mat.new['natal.fry', ] <- natal.fry
    mat.new['fry.migrants', ] <- fry.migrants
    mat.new['natal.fry.distrib', ] <- natal.fry.distrib['after_movement', ]
    mat.new['sub.yr.distrib', ] <- sub.yr.distrib
    mat.new['sub.yr', ] <- sub.yr
    mat.new['fry.migrants.bay', ] <- fry.migrants.bay
    mat.new['sub.yr.bay', ] <- sub.yr.bay
    
    N <- mat.new
    N
  } # end subbasin func
  
  
} # end if() chinook

# Steelhead function ----
if (pop == "steelhead") {
  subbasin <- function(mat = N, ...){
    
    NOR.total <- mat['spawners', ]
    # NOR.total <- N.init
    # egg.cap.wt <- egg.cap
    # fecund <- fecund.first
    
    # Weighted fecundity
    wts.firstspawn <- colSums(mat[firstspawn.stages, ]) / mat['total.run', ]
    wts.respawn <- colSums(mat[kelt.stages, ]) / mat['total.run', ]
    
    # Need this becuase of initialization. It says:
    # If the firstspawn weight is NA, use the firstspawn fecundity, otherwise use weighted fecundity
    if (all(is.na(wts.firstspawn))) {
      fecund <- fecund.first
    } else {
      fecund <- fecund.first * wts.firstspawn + fecund.respawn * wts.respawn
      fecund[is.na(fecund)] <- 0
    }
    
    # Weight egg cap by number of respawners
    egg.cap.wt <- (egg.cap / fecund.first) * fecund
    
    
    # 1st year
    eggs <- eggs.func(NOR.total, egg.total = egg.cap.wt, fecund = fecund) # Hockey stick
    pre.fry <- eggs * egg.fry.surv * ef_flow # Eggs --> freshly emerged fry

    parr <- BH.func(pre.fry, p = parr.surv, c = parr.cap)# summer parr
    
    parr.distributed <- distribute.fish(fish.in = parr, 
                                        move.matrix = sweep(move.matrix.spring, 
                                                            MARGIN = 2, 
                                                            percent.fall.migrants, 
                                                            '*'))
    
    age1 <- BH.func(parr.distributed['after_movement', ], p = first.winter.surv, c = first.winter.cap) # overwinter
    
    
    # Age1 smolts leave, age1 stay for another year
    age1.smolts               <- age1 * prop.age1.smolts       
    mat.new['age1.stayers', ] <- age1 * (1 - prop.age1.smolts)
    
    
    # 2nd year, age 1+, Start with previous year's fish - first fish redistribute to MS, then go through summer and winter
    # Spring redistribution of 1+ (0, 20, 50%, depending on basin size)
    age1.stayers.dist <- distribute.fish(fish.in = mat['age1.stayers', ], 
                                         move.matrix = sweep(move.matrix.spring, 
                                                             MARGIN = 2, 
                                                             percent.spring.migrants, 
                                                             '*'))
    
    age1.plus <- BH.func(age1.stayers.dist['after_movement', ], p = second.summer.surv, c = second.summer.cap)# end of 2nd summer, 1.5 years old
    age2 <- BH.func(age1.plus, p = second.winter.surv, c = second.winter.cap) # End of 2nd winter, 2 yo
    
    age2.smolts               <- age2 * prop.age2.smolts
    mat.new['age2.stayers', ] <- age2 * (1 - prop.age2.smolts)
    
    
    # 3rd year, age 2+ stayers from previous year rear for one more year, then all smolt
    age3 <- BH.func(mat['age2.stayers', ], p = second.summer.surv, c = second.summer.cap)# in their rearing basins
    age3.smolts <- BH.func(age3, p = second.winter.surv, c = second.winter.cap)# in their rearing basins
    
    
    # Reallocate fish back into natal basins -- helps for keeping track in the ocean
    age2.smolts.realloc1 <- reallocate.fish(fish.in = age2.smolts,
                                           redist.matrix = age1.stayers.dist[return.rows, ])['after_movement', ]
    age2.smolts.realloc <- reallocate.fish(fish.in = age2.smolts.realloc1,
                                           redist.matrix = parr.distributed[return.rows, ])['after_movement', ]
    
    age3.smolts.realloc1 <- reallocate.fish(fish.in = age3.smolts,
                                           redist.matrix = age1.stayers.dist[return.rows, ])['after_movement', ]
    age3.smolts.realloc <- reallocate.fish(fish.in = age3.smolts.realloc1,
                                            redist.matrix = parr.distributed[return.rows, ])['after_movement', ]
    
    
    age1.bay <- age1.smolts         * bay.surv # age1 smolts after bay
    age2.bay <- age2.smolts.realloc * bay.surv # age2 smolts after bay
    age3.bay <- age3.smolts.realloc * bay.surv # age3 smolts after bay

    
    # Ocean stages - before any spawning
    mat.new['age2.ocean', ] <-   age1.bay * So.func(so.min, so.max) # age1 smolts 1st ocean year
    mat.new['age3.ocean', ] <-  (mat['age2.ocean', ] + age2.bay) * So.func(so.min, so.max) # age1 smolts 2nd ocean year, age2 smolts 1st ocean year
    mat.new['age4.ocean', ] <-  (mat['age3.ocean', ] + age3.bay) * (1 - b3)  * So.func(so.min, so.max) # age1 smolts 3rd ocean year, age2 smolts 2nd ocean year, age3 smolts 1st ocean year
    mat.new['age5.ocean', ] <-   mat['age4.ocean', ] * (1 - b4)  * So.func(so.min, so.max) # age1 smolts 4th ocean year, age2 smolts 3rd ocean year, age3 smolts 2nd ocean year
    mat.new['age6.ocean', ] <-   mat['age5.ocean', ] * (1 - b5)  * So.func(so.min, so.max) # age1 smolts 5th ocean year, age2 smolts 4th ocean year, age3 smolts 3rd ocean year
    
    # Adult returns
    mat.new['age3.firstspawn', ] <- mat['age3.ocean', ] * b3
    mat.new['age4.firstspawn', ] <- mat['age4.ocean', ] * b4
    mat.new['age5.firstspawn', ] <- mat['age5.ocean', ] * b5
    mat.new['age6.firstspawn', ] <- mat['age6.ocean', ] # All age 6 fish return to spawn
    
    # Kelt (respawners) returns
    mat.new['age4.kelt', ] <- (mat['age3.firstspawn', ] + mat['age3.kelt', ]) * kelt.rate
    mat.new['age5.kelt', ] <- (mat['age4.firstspawn', ] + mat['age4.kelt', ]) * kelt.rate
    mat.new['age6.kelt', ] <- (mat['age5.firstspawn', ] + mat['age5.kelt', ]) * kelt.rate
    mat.new['age7.kelt', ] <- (mat['age6.firstspawn', ] + mat['age6.kelt', ]) * kelt.rate
    mat.new['age8.kelt', ] <-  mat['age7.kelt', ] * kelt.rate
    
    # Other stored stages
    mat.new['eggs', ] <- eggs
    mat.new['pre.fry', ] <- pre.fry 
    mat.new['parr', ] <- parr 
    mat.new['age1', ] <- age1
    mat.new['age1.smolts', ] <- age1.smolts
    mat.new['age2.smolts', ] <- age2.smolts
    mat.new['age3.smolts', ] <- age3.smolts
    # NOTE: How do we turn "off" harvest when we have kelts that have been subject to harvest twice, once as first time returning spawners
    # and again as returning respawners?
    mat.new['total.run', ] <- mat.new[c(firstspawn.stages, kelt.stages), ] %>% colSums() 
    mat.new['spawners', ] <- mat.new['total.run', ] * S.up * (1 - Hr)
  
    N <- mat.new
    N[N < 0 & N > -1] <- 0 # convert fractional negative fish to zero (some very small negative numbers from movement function)
    N
    
  } #End sub basin function 
  
} #end if() steelhead

# Main chum function -----
#Lifestages
# 1)  Eggs
# 2)  Pre Fry
# 4)  Fry/Smolts
# 5)  Ocean0 mat[1],
# 6)  Ocean1 mat[2,]
# 7)  Ocean2 mat[3,]
# 8)  Total Run
# 9)  Spawners

if (pop == 'chum') {
  
  subbasin <- function(mat = N, ...){
    
    NOR.total <- mat['spawners',]
    #NOR.total <- N["spawners",] # Run this line to be able to step through func
    
    eggs <- eggs.func(NOR.total, egg.total = egg.cap, fecund = fecund) # Number of eggs in adults
    # eggs <- BH.func(S = NOR.total * .5, p = fecund, c = egg.cap) # B-H
    pre.fry <- eggs * egg.fry.surv * egg.flow.dec()
    
    fry <- BH.func(pre.fry, p = fry.colonization.surv, c = fry.colonization.cap)
    
    fry.migrant <- fry * fry.migrant.surv

    
    # Apply bay survival (ds migration, delta, bay, nearshore productivity)
    fry.migrant.bay <- fry.migrant * bay.surv
    
    # Ocean survival
    mat.new['ocean0', ] <- fry.migrant.bay # smolts leaving bay, ocean0
    mat.new['ocean1', ] <- mat[1, ] * So.func(so.1[1], so.1[2]) # 2 year olds, ocean1
    mat.new['ocean2', ] <- mat[2, ] * (1 - b2) * So.func(so.2[1], so.2[2]) # 3 year olds, ocean2
    mat.new['ocean3', ] <- mat[3, ] * (1 - b3) * So.func(so.3[1], so.3[2]) # 4 year olds, ocean3
    mat.new['ocean4', ] <- mat[4, ] * (1 - b4) * So.func(so.4[1], so.4[2]) # 5 year olds, ocean4
    
    mat.new['total.run',] <- (b2 * mat['ocean1', ] + b3 * mat['ocean2', ] + b4 * mat['ocean3', ] + b5 * mat['ocean4',]) #Total run
    mat.new['spawners',] <- mat.new['total.run', ] * (S.up) * (S.sb) * (1 - (Hr * hr.adj)) # Spawners
    
    mat.new['eggs', ] <- eggs
    mat.new['pre.fry', ] <- pre.fry 
    mat.new['fry', ] <- fry
    mat.new['fry.migrant', ] <- fry.migrant
    # mat.new['estuary.resident', ] <- estuary.resident # 10 week survival in estuary
    mat.new['fry.migrant.bay', ] <- fry.migrant.bay

    N <- mat.new
    N
  } # end subbasin func
  
  
} # end if() chum