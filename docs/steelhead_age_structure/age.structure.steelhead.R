


age.test <- function(age.target, grid.search){
  # This function does an exhaustive grid calculation
  # of age of returns for many combinations
  # of propensities of 3, 4, 5, and 6 year olds
  # returning from the ocean.
  # It produces a data.frame with seven columns:
  # columns 1-4 are the propensities,
  # column 5 is a comparison of percent age at return --
  # sum(abs(age.dif <- age.target - age.mod)),
  # where age.dif is the summed absolute value of the differences
  # of age at return between (obs - modeled)
  # Note, sort the function's resulting data.frame
  # by column "dif" (5; lowest to highest)
  # to get the combinations of propensities that minimize the difference
  # Cols 6-8 are the cumulative
  # ocean survivals for age1, age2, and age3 smolts respectively
  # for the specified propensities to return
  #
  # Observations:
  # WDFW data for proportion 
  # of first time spawning 
  # steelhead age of returns
  # averaged over several years
  age3.target <- 0.01
  age4.target <- 0.49
  age5.target <- 0.40
  age6.target <- 0.10
  age.target <- c(age3.target, age4.target, age5.target, age6.target)
  
  so <- 0.80
  # Grid of propensities to search:
  # propensities to return (b3, b4, b5, b6)
  # This is from the params.steelhead.R file:
  # b3 <- 0.05 # propensity to return as 3 year olds
  # b4 <- 0.4 # propensity to return as 4 year olds
  # b5 <- 0.6 # propensity to return as 5 year olds
  # # Any age 6 that still hasn't spawned will return
  # Differential bay survivals for smolts at age
  #  bay.surv.age1 
  #  bay.surv.age2
  #  bay.surv.age3
  
  grid.search <- expand.grid(b3=seq(0.01, 0.20, by=0.03), 
                             b4=seq(0.30, 0.55, by=0.03), 
                             b5=seq(0.65, 0.80, by=0.03), 
                             b6=1,
                             bay.surv.age1=seq(0.01, 0.20, by=0.03),
                             bay.surv.age2=seq(0.05, 0.40, by=0.03),
                             bay.surv.age3=seq(0.15, 0.60, by=0.03),
                             prop.age1=seq(0.01, 0.10, by=0.03),
                             prop.age2=seq(0.60, 0.80, by=0.03))
  # store outputs
  my.dif <- data.frame(grid.search=grid.search, dif=rep(NA, dim(grid.search)[1]), 
                       ocean.surv.age1 = rep(NA, dim(grid.search)[1]),
                       ocean.surv.age2 = rep(NA, dim(grid.search)[1]), 
                       ocean.surv.age3 = rep(NA, dim(grid.search)[1]),
                       SAR = rep(NA, dim(grid.search)[1]))
  
  for(i in 1:dim(grid.search)[1]) {  
    
    b3 <- grid.search[i,1] #0.05
    b4 <- grid.search[i,2] #0.40
    b5 <- grid.search[i,3] #0.60
    b6 <- grid.search[i,4] #1.0
    b <- c(b3, b4, b5, b6)
    bee3 <- 1
    bee4 <- 2
    bee5 <- 3
    bee6 <- 4
    bay.surv.age1 <- grid.search[i,5]
    bay.surv.age2 <- grid.search[i,6]
    bay.surv.age3 <- grid.search[1,7]
    
    total.smolts <- 3000
    # Proportions of each emigrant 
    #  (per LCM working group 10/30/2019)
    prop.age1.smolts <- grid.search[1,8]#0.003
    prop.age2.smolts <- grid.search[1,9]#0.8#0.87
    prop.age3.smolts <- 1 - (grid.search[1,8] + grid.search[1,9])#(prop.age1.smolts + prop.age2.smolts)
    
    # includes differential bay survivals
    age3.ocean.age1.smolts <- total.smolts*prop.age1.smolts*bay.surv.age1*(1 - b[bee3])*so*so
    age3.ocean.age2.smolts <- total.smolts*prop.age2.smolts*bay.surv.age2*(1 - b[bee3])*so
    age3.returns.age1.smolts <- total.smolts*prop.age1.smolts*bay.surv.age1*b[bee3]*so*so
    age3.returns.age2.smolts <- total.smolts*prop.age2.smolts*bay.surv.age2*b[bee3]*so
    # note: no age3.smolts return to spawn as 3 yr olds
    age4.ocean.age1.smolts <- age3.ocean.age1.smolts*(1 - b[bee4])*so
    age4.ocean.age2.smolts <- age3.ocean.age2.smolts*(1 - b[bee4])*so
    age4.ocean.age3.smolts <- total.smolts*prop.age3.smolts*bay.surv.age3*(1 - b[bee4])*so
    age4.returns.age1.smolts <- age3.ocean.age1.smolts*b[bee4]*so
    age4.returns.age2.smolts <- age3.ocean.age2.smolts*b[bee4]*so
    age4.returns.age3.smolts <- total.smolts*prop.age3.smolts*bay.surv.age3*b[bee4]*so
    age5.ocean.age1.smolts <- age4.ocean.age1.smolts*(1 - b[bee5])*so
    age5.ocean.age2.smolts <- age4.ocean.age2.smolts*(1 - b[bee5])*so
    age5.ocean.age3.smolts <- age4.ocean.age3.smolts*(1 - b[bee5])*so
    age5.returns.age1.smolts <- age4.ocean.age1.smolts*b[bee5]*so
    age5.returns.age2.smolts <- age4.ocean.age2.smolts*b[bee5]*so
    age5.returns.age3.smolts <- age4.ocean.age3.smolts*b[bee5]*so
    age6.returns.age1.smolts <- age5.ocean.age1.smolts*b[bee6]*so
    age6.returns.age2.smolts <- age5.ocean.age2.smolts*b[bee6]*so
    age6.returns.age3.smolts <- age5.ocean.age3.smolts*b[bee6]*so
    
    total.run.age1.smolts <- age3.returns.age1.smolts + age4.returns.age1.smolts + age5.returns.age1.smolts + age6.returns.age1.smolts
    total.run.age2.smolts <- age3.returns.age2.smolts + age4.returns.age2.smolts + age5.returns.age2.smolts + age6.returns.age2.smolts
    total.run.age3.smolts <- age4.returns.age3.smolts + age5.returns.age3.smolts + age6.returns.age3.smolts
    total.run <- total.run.age1.smolts + total.run.age2.smolts + total.run.age3.smolts
    
    # Ocean survival (including bay) by age of smolt
    ocean.surv.age1.smolts <- total.run.age1.smolts/(total.smolts*prop.age1.smolts)
    ocean.surv.age2.smolts <- total.run.age2.smolts/(total.smolts*prop.age2.smolts)
    ocean.surv.age3.smolts <- total.run.age3.smolts/(total.smolts*prop.age3.smolts)
    # SAR (smolts before bay, adult returns to mouth of Chehalis)
    sar <- (total.run.age1.smolts + total.run.age2.smolts + total.run.age3.smolts)/(total.smolts*prop.age1.smolts + total.smolts*prop.age2.smolts + total.smolts*prop.age3.smolts)
    
    # Age structure from this routine 
    age3.modeled <- (age3.returns.age1.smolts + age3.returns.age2.smolts)/total.run
    age4.modeled <- (age4.returns.age1.smolts + age4.returns.age2.smolts + age4.returns.age3.smolts)/total.run
    age5.modeled <- (age5.returns.age1.smolts + age5.returns.age2.smolts + age5.returns.age3.smolts)/total.run
    age6.modeled <- (age6.returns.age1.smolts + age6.returns.age2.smolts + age6.returns.age3.smolts)/total.run
    
    age.mod <- c(age3.modeled, age4.modeled, age5.modeled, age6.modeled)
    
    age.dif <- age.target - age.mod
    
    my.dif[i,10] <- sum(abs(age.dif))
    my.dif[i,11] <- ocean.surv.age1.smolts
    my.dif[i,12] <- ocean.surv.age2.smolts
    my.dif[i,13] <- ocean.surv.age3.smolts
    my.dif[i,14] <- sar
  }
  my.dif
}

sthd.propensity.ocean.surv <- age.test()# runs the above function and stores its output
# results, sorted by difference between target age structure and search grid age values
results <- sthd.propensity.ocean.surv[order(sthd.propensity.ocean.surv$dif),]
# first 10 results:
results[1:10,]
