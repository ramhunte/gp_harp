#################################################
#
# Chehalis matrix-type life cycle model
# 
# To run the model: 
# 1) Adjust model control parameters
# 2) "Source" this script
#################################################


# Model control parameters ----

# Choose number of years and number of model iterations
# Full model run is 100 years x 500 runs. Testing is 100 years x 50 runs
runs  <- 5


# Run model in sensitivity mode? "yes" or "no"
# Calculate the relative influence of certain parameters
sensitivity.mode <- 'no'



# Do you want to save plots? "yes" or "no"
# If not in sensitivity mode a box plot of spawner abundance will be saved,
# plots of total run changes per DU and maps of restoration potential will be created
# If in sensitivity mode, only the sensitivy bar plot will be saved
save.plots <- 'yes'



# Do you want to create diagnostic plots?
# These can be hard to interpret, but are intended to help diagnose model performance
# Takes ~5 min to generate the plots
diag.plots <- 'no'


 


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
  if (pop == "fall.chinook" | pop == "spring.chinook" |
      pop == "coho") {
    cat("RS.preH", "\t")
    cat("RS.postH", "\t")
  }
  cat("scenario", "\n")
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
      
      #  Sensitivity ----
      #   Values called for sensitivity
      #   this input file overrides or augments previously defined params:
      if (sensitivity.mode == "yes") {
        source("lcm/scripts/sensitivity.R")
        source("lcm/scripts/assign.dat.R")
        
      } #End if() sensitivty mode
      
      
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
    
    # Find true equilibrium before running 100 years - Takes a long time 
    # Better solution?
    # equilibrium <- FALSE 
    # while (!equilibrium) {  
    #   N.new <- subbasin(mat=N)
    #   if (all(N.new["spawners",] == N["spawners",])) {
    #     for (i in 1:5){
    #       N <- N.new
    #       N.new <- subbasin(mat=N)
    #     }
    #     equilibrium <- all(N.new["spawners", ] == N["spawners", ])
    #   }
    #   N <- N.new
    # } # End burn in
    
    # Main loop --------
    
    for (i in 1:years) {
      
      N.new <- subbasin(mat=N)
      N <- N.new
      
      # Store all model results in 5 dimensional array: runs (j) x years (i) x lifestage x DUs x scenarios (n)
      model.all[j,i,,,n] <- N 
      
      if (pop == "fall.chinook" | pop == "spring.chinook") {
        Spawners[i] <- sum(N['spawners', ])
        # Total run recruits (preharvest), 
        #  and spawner recruits (postharvest)
        #  referenced to brood year
        if (i > 2 && i <= (years - 1)) {
          Recruits.preharvest[i - 2] <- sum(b2*N[2, ])
          Recruits.postharvest[i - 2] <- sum(b2*N[2, ])*(S.up)*(S.sb)*(1 - (Hr*hr.adj)) }
        if (i > 3 && i <= (years - 1)) {
          Recruits.preharvest[i - 3] <- Recruits.preharvest[i - 3] + sum(b3*N[3, ])
          Recruits.postharvest[i - 3] <- Recruits.postharvest[i - 3] + sum(b3*N[3, ])*(S.up)*(S.sb)*(1-(Hr*hr.adj))}
        if (i > 4 && i <= (years - 1)) {
          Recruits.preharvest[i - 4] <- Recruits.preharvest[i - 4] + sum(b4*N[4, ])
          Recruits.postharvest[i - 4] <- Recruits.postharvest[i - 4] + sum(b4*N[4, ])*(S.up)*(S.sb)*(1-(Hr*hr.adj))}
        if (i > 5 && i <= (years - 1)) {
          Recruits.preharvest[i - 5] <- Recruits.preharvest[i - 5] + sum(b5*N[5, ])
          Recruits.postharvest[i - 5] <- Recruits.postharvest[i - 5] + sum(b5*N[5, ])*(S.up)*(S.sb)*(1-(Hr*hr.adj))}
        if (i > 6 && i <= (years - 1)) {
          Recruits.preharvest[i - 6] <- Recruits.preharvest[i - 6] + sum(N[6, ])
          Recruits.postharvest[i - 6] <- Recruits.postharvest[i - 6] + sum(N[6, ])*(S.up)*(S.sb)*(1-(Hr*hr.adj))}
        }
          
      if (pop=="coho") {
        Spawners[i] <- sum(N['spawners', ])
        if (i > 2 && i <= (years - 1)) {
          Recruits.preharvest[i - 2] <- sum(b2*N[2, ])
          Recruits.postharvest[i - 2] <- sum(b2*N[2, ])*(S.up)*(S.sb)*(1-(Hr*hr.adj)) }
        if (i > 3 && i <= (years - 1)) {
          Recruits.preharvest[i - 3] <- Recruits.preharvest[i - 3] + sum(N[3, ])
          Recruits.postharvest[i - 3] <- Recruits.postharvest[i - 3] + sum(N[3, ])*(S.up)*(S.sb)*(1-(Hr*hr.adj))}
        }
        
    }# end years loop, i
    
    # Fill array with sensitivity results. 
    
    # Calculte the geometric mean of a specific model run
    tr.geomean <- model.all[j, , 'total.run', , scenario.file[n]] %>%
      apply(., 1, sum) %>%
      geo.mean(.)
    
    
    # Fill sensitivity arrays
    # runs (j) x params x scenario.file (n)
    
    if (pop == "coho" & sensitivity.mode == "yes") {

      sensitivity[j, , n] <- c(
        #fecund,
        egg.cap.adj,
        egg.fry.surv.adj,
        sub.yr.surv.adj,
        sub.yr.cap.adj,
        ps.surv.adj,
        ps.cap.adj,
        #bay.min,        
        # so.min,
        #Hr,
        #S.sb,
        tr.geomean)
    } #ends fill coho sensitivty[] array

    if (pop == "fall.chinook" | pop == "spring.chinook" & sensitivity.mode == "yes") {
      
      sensitivity[j, , n] <- c(
        #fecund,
        egg.cap.adj,
        egg.fry.surv.adj,
        fry.surv.adj,
        sub.yr.cap.adj,
        sub.yr.surv.adj,
        #fry.migrant.surv.adj,
        #bay.parr.surv.adj,
        #bay.fry.surv.adj,
        #Hr,
        #S.sb,
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
  
  if (pop == "fall.chinook" | pop == "spring.chinook" | pop == "coho") {
    RS.preH <- Recruits.preharvest / Spawners
    RS.postH <- Recruits.postharvest / Spawners
  }
  
  
  if (pop == "fall.chinook" | pop == "spring.chinook") {
    # % fry migrant smolts
    SAR.ratio <- sum(N['smolts.fry.migrants',]) / 
      sum(N['smolts.fry.migrants',], 
          N['smolts.non.natal.sub.yr',], 
          N['smolts.non.natal.sub.yr',]
          ) 
    
    # % fry migrants in returning adults
    ratio <- sum(N['smolts.fry.migrants', ] * bay.fry.surv) /
      sum(N['smolts.fry.migrants', ] * bay.fry.surv,
          N['smolts.non.natal.sub.yr', ] * bay.parr.surv,
          N['smolts.non.natal.sub.yr', ] * bay.parr.surv)
    
    SAR.fry <- so.weighted * bay.fry.surv
    SAR.parr <- so.weighted * bay.parr.surv
    SAR.weighted <- (SAR.fry * SAR.ratio) + (SAR.parr * (1 - SAR.ratio))
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
    if (pop == "fall.chinook" |
        pop == "spring.chinook" | pop == "coho") {
      cat(round(mean(RS.preH), 2), "\t", "\t")
      cat(round(mean(RS.postH), 2), "\t", "\t")
    }
    cat(scenario.file[n], "\n")
  }
  
}# end scenario loop, n


# Call the plots ---------------------------------------------------------------------------------------------------------

# Create a directory for today's outputs
outputs_lcm <- file.path("outputs", fishtype, "lcm")

if (dir.exists(outputs_lcm) == F) {dir.create(outputs_lcm, recursive = T)}


# Create summary csv. Spawners per subbasin

if (pop == 'coho') {summary.stages <- c('spawners','natal.smolts','non.natal.smolts')}

if (pop == "fall.chinook" | pop == "spring.chinook") {
  summary.stages <- c('spawners','smolts.fry.migrants','smolts.non.natal.sub.yr','smolts.natal.sub.yr')
}

if (pop == 'steelhead') {summary.stages <- c('spawners','age1.smolts','age2.smolts')}


abundance_by_subbasin <- model.all[ ,50:100, summary.stages, , ] %>%
  apply(c(1,3,4,5),geo.mean) %>% # geomean across years
  apply(c(2,3,4),mean) %>% # mean of runs
  round(0) %>%# round to whole fish
  as.data.frame.table() %>%
  rename(lifestage = Var1, Subbasin = Var2, scenario = Var3, n = Freq) %>%
  spread(lifestage, n) %>%
  mutate(scenario = factor(scenario, levels = read.csv('lcm/data/scenarios.csv')$scenario)) %>%
  filter(scenario != 'Historical.no.beaver') %>%
  arrange(scenario) %>%
  rename(natal.basin = Subbasin)

csv.name <- paste0(pop, '_abundance_by_subbasin.csv')
  
write.csv(abundance_by_subbasin, file.path(outputs_lcm, csv.name))


# Call bar, box or sensitivity plots
packages.plots <- c("grid","scales")
invisible(lapply(packages.plots,pkgCheck))
source("lcm/scripts/plots.R")


# Call spatial plots
# if (save.plots == "yes" & sensitivity.mode == "no"){
#   packages.spatial.plots <- c("gridExtra","rgdal", "rgeos", "maptools","TeachingDemos")
#   invisible(lapply(packages.spatial.plots,pkgCheck))
#   source("lcm/scripts/spatial.plots.R")
# } 


# Call diagnostic plots
if (diag.plots == 'yes') {
  print('Creating spawner abundance plots')
  source("lcm/scripts/diagnostic.plots.R")
} 


# Call S-R curve plots (currently only working for coho 3/19/2019)
if (pop == 'coho') {
  print('Creating spawner recruit (P and C) values and plots')
  source('lcm/scripts/spawner.recruit.curves.R')
}


# Call comparison plots
if (run_asrp == "yes") {
  if (!branch %in% c('dev','master')) {
    print('Compare current run to dev branch')
    source('lcm/scripts/compare.model.runs.R')
  }
}