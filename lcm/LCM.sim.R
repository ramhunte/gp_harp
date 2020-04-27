#################################################
#
# Chehalis matrix-type life cycle model
# 
# To run the model, see run_noaa_chehalis_model.R
#################################################



#--------   Use caution when adjusting code below here   -----------------------------------------------------------------



# Define which species to run ----

pop <- species <- fishtype


# Convert fishtype (Hab model) to pop and species (LCM)

if (fishtype == "fall_chinook") {
  species <- pop <- "fall.chinook"
} else if (fishtype == "spring_chinook") {
  species <- pop <- "spring.chinook"
}

# Number of years the model will run for
years <- 100

# Number of model runs (iterations)
if (run_stochastic_eggtofry == 'yes') {
  runs  <- 500
}

if (sensitivity.mode == 'yes') {
  runs <- 50
} else {
  runs <- 2
}



# Load functions ----

source("lcm/scripts/funcs.R")



# Load packages ----

invisible(pkgCheck("tidyverse"))


# Load scenario names, initialize arrays and load parameter settings ------

source("lcm/scripts/initialize.R")



# Run model -----------------------------------------------------------------------------------------------

# Print model output headers to the consol
if (sensitivity.mode == "no") {
  cat(paste0("Running LCM for ", pop), "\n")
  cat("runs", "\t")
  cat("years", "\t")
  cat("spawners", "\t")
  if (pop == "coho" | pop == "steelhead")
    cat("SAR", "\t")
  if (pop == "fall.chinook" | pop == "spring.chinook") {
    cat("Fry:Parr", "\t")
    cat("SAR", "\t")
    cat("SAR.fry", "\t")
    cat("SAR.parr", "\t")
  }
  cat("scenario", "\n")
} else {
  cat("Running LCM in Sensitivity Mode", "\n")
}

#Read in model parameter values
source(paste0('lcm/params/params.',pop,".R"))

# Habitat scenarios -----
for (n in 1:length(scenario.file)) {
  # loop across hab scenarios
  
  #Assign variables from habitat scenario files
  source("lcm/scripts/assign.dat.R")
  
  for (j in 1:runs) {
    # loop across model runs
    
    # Initialize -----
    for (i in 1:10) {
      # Run 10 years to fill initialize matrix
      
      # Initialize loop
      N.init <- pop.init * prop.init
      N.initialize['spawners', ] <- N.init
      N.initialize <- subbasin(mat = N.initialize)
      
      N <- N.initialize
      
    }# End initialize
    
    # Run model 100 times to reach equilibrium
    for (i in 1:100) {
      N.new <- subbasin(mat = N)
      N <- N.new
    }
    
    
    #  Sensitivity ----
    #  Randomly adjust params from hab scenario:
    if (sensitivity.mode == "yes") {
      source("lcm/scripts/sensitivity.R")
    } #End if() sensitivty mode
    
    # Main loop --------
    
    for (i in 1:years) {
      
      N.new <- subbasin(mat = N)
      N <- N.new
      
      # Store all model results in 5 dimensional array: runs (j) x years (i) x lifestage x DUs x scenarios (n)
      model.all[j,i,,,n] <- N 
      
    }# end years loop, i
    
    # Fill array with sensitivity results. 
    
    # Calculte the geometric mean of a specific model run
    tr.geomean <- model.all[j, , 'spawners', , scenario.file[n]] %>%
      apply(., 1, sum) %>%
      geo.mean
    
    
    # Fill sensitivity arrays
    # runs (j) x params x scenario.file (n)
    
    if (pop == "coho" & sensitivity.mode == "yes") {
      sensitivity[j, , n] <- c(
        egg.cap.adj,
        egg.fry.surv.adj,
        parr.surv.adj,
        parr.cap.adj,
        parr.smolt.surv.adj,
        parr.smolt.cap.adj,
        S.up.adj,
        tr.geomean)
    } #ends fill coho sensitivty[] array
    
    
    if (pop %in% c("fall.chinook", "spring.chinook") & sensitivity.mode == "yes") {
      sensitivity[j, , n] <- c(
        egg.cap.adj,
        egg.fry.surv.adj,
        surv.adj,
        cap.adj,
        surv.temp.adj,
        S.up.adj,
        tr.geomean)
    } #ends fill chinook sensitivty[] array
    
    if (pop == 'steelhead' & sensitivity.mode == "yes") {
      sensitivity[j, , n] <- c(
        egg.cap.adj,
        egg.fry.surv.adj,
        parr.surv.adj,
        parr.cap.adj,
        first.winter.surv.adj,
        first.winter.cap.adj,
        second.summer.surv.adj,
        second.summer.cap.adj,
        second.winter.surv.adj,
        second.winter.cap.adj,
        S.up.adj,
        tr.geomean)
    } #ends fill chinook sensitivty[] array
    
  }# end runs loop, j
  
  
  
  # Print model summaries to the consol ----
  
  # Spawners + harvest (spawners with no harvest, but yes prespawn mortality)
  spawn.hr <- model.all[, , 'spawners', , scenario.file[n]] %>%
    # apply(., c(1:3), function(x)
    #   x / (1 - Hr)) %>% # Add harvest back in (spawners + harvest)
    apply(., c(1,3), geo.mean) %>%
    apply(., 1, sum) %>%
    
    mean()
  
  if (pop == "fall.chinook" | pop == "spring.chinook" | pop == "coho" | pop == 'chum') {
    RS.preH <- Recruits.preharvest / Spawners
    RS.postH <- Recruits.postharvest / Spawners
  }
  
  
  if (pop == "fall.chinook" | pop == "spring.chinook") {
    # % fry migrant smolts
    SAR.ratio <- sum(N['fry.migrants',]) / 
      sum(N['fry.migrants',], 
          N['sub.yr',]
      ) 
    
    # % fry migrants in returning adults
    ratio <- sum(N['fry.migrants.bay',]) / 
      sum(N['fry.migrants.bay',], 
          N['sub.yr.bay',]
      ) 
    
    SAR.weighted <- (SAR.fry * SAR.ratio) + (SAR.parr * (1 - SAR.ratio))
  }
  
  if (pop == 'chum') {
    SAR.weighted <- SAR.fry
  }
  
  if (pop == "steelhead") {
    SAR <- (sum(N[firstspawn.stages,] %>% colSums) /
              sum(N['age1.smolts', ] + N['age2.smolts', ] + N['age3.smolts', ]))
    SAR <- SAR %>% round(3)
  }
  
  if (sensitivity.mode == "no") {
    cat(runs, "\t", years, "\t")
    cat(round(spawn.hr, 0), "\t", "\t")
    if (pop == "coho")
      cat(round(mean(c(bay.min, bay.max)) * (mean(c( so.min, so.max)) ^ 2), 4), "\t")
    if (pop == "steelhead")
      cat(SAR, "\t")
    if (pop == "fall.chinook" | pop == "spring.chinook") {
      cat(round(ratio, 3), "\t", "\t")
      cat(round(SAR.weighted, 4), "\t")
      cat(round(SAR.fry, 5), "\t")
      cat(round(SAR.parr, 4), "\t", "\t")
    }
    cat(scenario.file[n], "\n")
  }
  
}# end scenario loop, n


# Call the plots ---------------------------------------------------------------------------------------------------------

# Create a directory for today's outputs
outputs_lcm <- file.path("outputs", fishtype, "lcm")

if (dir.exists(outputs_lcm) == F) {dir.create(outputs_lcm, recursive = T)}


# Create summary csv. Spawners per subbasin
if (sensitivity.mode == 'no') {
  
  abundance_by_subbasin <- model.all[ ,50:100, lifestages, , ] %>%
    apply(c(1,3,4,5),geo.mean) %>% # geomean across years
    apply(c(2,3,4),mean) %>% # mean of runs
    #round(0) %>%# round to whole fish
    as.data.frame.table() %>%
    rename(lifestage = Var1, Subbasin = Var2, scenario = Var3, n = Freq) %>%
    spread(lifestage, n) %>%
    filter(scenario != 'ASRP.Current.asrp') %>%
    mutate(scenario = factor(scenario, levels = read.csv('lcm/data/scenarios.csv')$scenario)) %>%
    arrange(scenario) %>%
    rename(natal.basin = Subbasin)
  
  write.csv(abundance_by_subbasin, file.path(outputs_lcm, paste0(pop, '_abundance_by_subbasin_raw.csv')))
  
} # end if sensitivity.mode


# Call S-R curve plots
if (sensitivity.mode == 'no') {
  print('Creating spawner recruit (Pn and Cn) values')
  source('lcm/scripts/calculate.Pn.Cn.R')
}

# Call bar, box or sensitivity plots
print('Creating output plots')
packages.plots <- c("grid","scales")
invisible(lapply(packages.plots,pkgCheck))
source("lcm/scripts/plots.R")

# Call comparison plots
if (!branch %in% c('dev','master') & sensitivity.mode == 'no') {
  print('Compare current run to dev branch')
  source('lcm/scripts/compare.model.runs.R')
}
