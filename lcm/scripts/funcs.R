
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
    pre.fry <- eggs * egg.fry.surv # Eggs --> freshly emerged fry
    
    # Spring distribution
    fry.distributed <- distribute.fish(fish.in = pre.fry, move.matrix = move.matrix.spring * percent.fry.migrants)
  
    parr <- BH.func(fry.distributed['after_movement', ] * fry.colonization, p = parr.surv, c = parr.cap)# summer parr
    parr.distributed <- distribute.fish(fish.in = parr, move.matrix * redist) # Fall distribution into mainstem fixed by scenario
    
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
    mat.new['spawners', ] <- mat.new['total.run', ] * (S.up) * (S.sb) * (1 - (Hr * hr.adj)) #spawners
    
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


if (pop == "fall.chinook" | pop == "spring.chinook") {
  
  subbasin <- function(mat = N, ...){
    
    NOR.total <- mat['spawners',]
    #NOR.total <- N["spawners",] # Run this line to be able to step through func
    
    eggs <- eggs.func(NOR.total, egg.total = egg.cap, fecund = fecund) # Number of eggs in adults
    #eggs <- BH.func(S = NOR.total, p = fecund/2, c = egg.cap) # B-H
    pre.fry <- eggs * egg.fry.surv
    
    # Natal fry - All basins
    natal.fry <- BH.func(S = pre.fry, p = fry.surv, c = fry.cap) # Density dependent survival in fresh (first week after fry), 3x capacity
    
    # Non natal fry 
    # Upper basin fry migrate to mainstem
    # GH and MS migrate straight to bay (fry migrants)
    non.natal.fry <- (pre.fry * fry.surv) - natal.fry # Density dependent slice to create non natal fry
    non.natal.fry.to.ms <- c(non.natal.fry[to.ms.reaches], non.natal.fry[to.bay.reaches] * 0)
    non.natal.fry.to.bay <- c(non.natal.fry[to.ms.reaches] * 0 , non.natal.fry[to.bay.reaches])
    
    # Natal sub yearlings - all basins
    natal.sub.yr <- BH.func(S = natal.fry, p = sub.yr.surv, c = sub.yr.cap)
    
    # Non natal subyearlings - only upper watershed fish that migrated to ms reaches
    non.natal.fry.dist <- distribute.fish(non.natal.fry.to.ms, move.matrix) # Distribute non-natal fry
    non.natal.sub.yr <- BH.func(S = non.natal.fry.dist[2,], p = sub.yr.surv, c = sub.yr.cap - natal.sub.yr) # Run non-natal fry through B-H to get subyearlings
    non.natal.sub.yr.redist <- reallocate.fish(non.natal.sub.yr, redist.matrix = non.natal.fry.dist[return.rows,])[2, ] # Re-distribute non-natal subyearlings
    
    # Fry migrants
    fry.migrants.ms <- (non.natal.fry.dist[2,] * sub.yr.surv) - non.natal.sub.yr # Group 4
    fry.migrants.redist <- reallocate.fish(fry.migrants.ms, redist.matrix = non.natal.fry.dist[return.rows, ])[2, ] # Re-distribute non-natal subyearlings
   
    fry.migrants.parr <- (natal.fry * sub.yr.surv) - natal.sub.yr # Group 2
    
    fry.migrants <-  non.natal.fry.to.bay + fry.migrants.redist + fry.migrants.parr
    
    
    # Sub yearlings to bay (ds migration, delta, bay, nearshore productivity)
    natal.sub.yr.bay <- natal.sub.yr * bay.parr.surv
    non.natal.sub.yr.bay <- non.natal.sub.yr.redist * bay.parr.surv
    
    # Fry migrants to bay
    fry.migrants.bay <- fry.migrants * bay.fry.surv
    
    # Ocean survival
    mat.new['ocean0', ] <- (fry.migrants.bay + natal.sub.yr.bay + non.natal.sub.yr.bay) # smolts leaving bay, ocean0
    mat.new['ocean1', ] <- mat[1, ] * So.func(so.1[1], so.1[2]) # 2 year olds, ocean1
    mat.new['ocean2', ] <- mat[2, ] * (1 - b2) * So.func(so.2[1], so.2[2]) # 3 year olds, ocean2
    mat.new['ocean3', ] <- mat[3, ] * (1 - b3) * So.func(so.3[1], so.3[2]) # 4 year olds, ocean3
    mat.new['ocean4', ] <- mat[4, ] * (1 - b4) * So.func(so.4[1], so.4[2]) # 5 year olds, ocean4
    mat.new['ocean5', ] <- mat[5, ] * (1 - b5) * So.func(so.5[1], so.5[2]) # 6 year olds, ocean5
    
    mat.new['total.run',] <- (b2 * mat['ocean1', ] + b3 * mat['ocean2', ] + b4 * mat['ocean3', ] + b5 * mat['ocean4',] + mat['ocean5', ]) #Total run
    mat.new['spawners',] <- mat.new['total.run', ] * (S.up) * (S.sb) * (1 - (Hr * hr.adj)) # Spawners
    
    mat.new['eggs', ] <- eggs
    mat.new['pre.fry', ] <- pre.fry 
    mat.new['natal.fry', ] <- natal.fry
    mat.new['non.natal.fry', ] <- non.natal.fry
    mat.new['fry.migrants', ] <- fry.migrants
    mat.new['fry.migrants.ms', ] <- fry.migrants.ms
    mat.new['non.natal.sub.yr', ] <- non.natal.sub.yr
    mat.new['smolts.fry.migrants', ] <- fry.migrants
    mat.new['smolts.non.natal.sub.yr', ] <- non.natal.sub.yr.redist
    mat.new['smolts.natal.sub.yr', ] <- natal.sub.yr
    
    
    N <- mat.new
    N
  } # end subbasin func
  
  
} # end if() chinook

# Steelhead function ----
if (pop == "steelhead") {
  subbasin <- function(mat = N, ...){
    
    NOR.total <- mat['spawners', ]
    # NOR.total <- N.init
    
    # Weighted fecundity
    wts.firstspawn <- colSums(mat[firstspawn.stages, ]) / mat['total.run', ]
    wts.respawn <- colSums(mat[kelt.stages, ]) / mat['total.run', ]
    
    # Need this becuase of initialization. It says:
    # If the firstspawn weight is NA, use the firstspawn fecundity, otherwise use weighted fecundity
    if (all(is.na(wts.firstspawn))) {
      fecund <- fecund.first
    } else {
      fecund <- fecund.first * wts.firstspawn + fecund.respawn * wts.respawn
      }
    
    eggs <- eggs.func(NOR.total, egg.total = egg.cap, fecund = fecund) # Hockey stick
    pre.fry <- eggs*egg.fry.surv # Eggs --> freshly emerged fry
    # need movement here of fry prior to parr stage
    parr <- BH.func(pre.fry, p = parr.surv, c = parr.cap)# summer parr
    mat.new['age1', ] <- BH.func(parr, p = first.winter.surv, c = first.winter.cap)# overwinter
    mat.new['age1.smolts', ] <- mat.new['age1', ]*prop.age1.smolts# 1 year old smolts
    age1.plus <- BH.func(mat['age1', ]*(1 - prop.age1.smolts), p = second.summer.surv, c = second.summer.cap)# end of 2nd summer
    mat.new['age2.smolts', ] <- BH.func(age1.plus, p = second.winter.surv, c = second.winter.cap)# 2 year old smolts
    
    age1.bay <- mat['age1.smolts', ] * bay.surv # age1 smolts after bay
    age2.bay <- mat['age2.smolts', ] * bay.surv # age2 smolts after bay

    # Ocean stages - before any spawning
    mat.new['age2.ocean', ] <-   age1.bay * So.func(so.min, so.max) * age1.ocean.correction # age1 smolts 1st ocean year
    mat.new['age3.ocean', ] <-  (mat['age2.ocean', ] + age2.bay) * So.func(so.min, so.max) # age1 smolts 2nd ocean year, age2 smolts 1st ocean year
    mat.new['age4.ocean', ] <-   mat['age3.ocean', ] * (1 - b3)  * So.func(so.min, so.max) # age1 smolts 3rd ocean year, age2 smolts 2nd ocean year
    mat.new['age5.ocean', ] <-   mat['age4.ocean', ] * (1 - b4)  * So.func(so.min, so.max) # age1 smolts 4th ocean year, age2 smolts 3rd ocean year
    mat.new['age6.ocean', ] <-   mat['age5.ocean', ] * (1 - b5)  * So.func(so.min, so.max) # age1 smolts 5th ocean year, age2 smolts 4th ocean year
    
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
    # mat.new['age1', ] <- age1
    # mat.new['age1.smolts', ] <- age1.smolts
    # mat.new['age1.plus', ] <- age1.plus
    # mat.new['age2.smolts', ] <- age2.smolts
    # NOTE: How do we turn "off" harvest when we have kelts that have been subject to harvest twice, once as first time returning spawners
    # and again as returning respawners?
    mat.new['total.run', ] <- mat.new[c(firstspawn.stages, kelt.stages), ] %>% colSums() 
    mat.new['spawners', ] <- mat.new['total.run', ] * (S.up) * (S.sb) * (1 - (Hr * hr.adj))
  
    N <- mat.new
    N[is.na(N)] <- 0
    N
    
  } #End sub basin function 
  
} #end if() steelhead