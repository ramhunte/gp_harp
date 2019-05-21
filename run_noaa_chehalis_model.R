# Introduction ----

# Welcome to the NOAA Chehalis ASRP Model

# To run the model click the "Source" button in the top right corner of this box 
# You will revcieve several prompts, which you will need to select an answer

# Prompt #1 -- Would you like to clear your environment? 
#    Selecting yes will delete everything in your global environment

# Prompt #2 -- Choose a species
#    Choose a species to run.  Options are spring chinook, fall chinook, coho, steelhead or all_species
#    WARNING - the contents of the outputs folder will be deleted and recreated

# Prompt #3 -- Would you like to run asrp scenarios?
#    Choose if you want to run the model including the ASRP restoration scenarios (e.g. ASRP Scenario 1 - 2040)
#    If 'no', only the diagnostic scenarios will be run




#------------------- begin Noaa Chehalis Model script -------------------------------------------



# Load prompts ----

# Clear the environment?
clear_env_query <- c('yes', 'no')
clear_env <- clear_env_query[menu(clear_env_query, 
                                  title = "Clear your environment?", 
                                  graphics = TRUE)]

# Clear the environment
if (clear_env == 'yes') {
  rm(list = ls())
}

# Which species to run?
spp <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead', 'all_species')
fishtype <- spp[menu(spp,title = "Choose a species", graphics = TRUE)]


# Run the ASRP scenarios?
run_asrp_query <- c('yes', 'no')
run_asrp <- run_asrp_query[menu(run_asrp_query, title = "Run ASRP scenarios?", graphics = TRUE)]


# Create the spawner-recruit curves?
run_sr_curves_query <- c('yes', 'no')
run_sr_curves <- run_sr_curves_query[menu(run_sr_curves_query, title = "Create spawner-recruit curves?", graphics = TRUE)]

# Run stochastic egg to fry ?
run_stochastic_eggtofry_query <- c('yes', 'no')
run_stochastic_eggtofry <- run_stochastic_eggtofry_query[menu(run_stochastic_eggtofry_query, title = 'Run stochastic egg to fry?', graphics = TRUE)]

# Store branch name ----
branch <- system(command = "git rev-parse --abbrev-ref HEAD", intern = TRUE)


# Load packages ----

global.pkgs <- c('tidyverse', 'magrittr', 'lubridate', 'zoo')

invisible(
  lapply(global.pkgs, function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if (!require(x, character.only = TRUE)) 
        stop(paste0("Packages ", x, " not found"))
    }
  }
  )
)



# Source the scripts ----

if (fishtype == 'all_species') {
  
  unlink("outputs", recursive = TRUE) # WARNING -- If running all species the entire outptus folder will be deleted
  
  for (s in c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')) {
    fishtype <- s
    source("hab/R_files/Run_Habitat_Model.R")
    source("lcm/LCM.sim.R")
    print(paste0("finished ", s))
  }
  
} else {
  
  unlink(file.path("outputs", fishtype), recursive = TRUE) # WARNING -- Entire outptus folder for species will be deleted
  source("hab/R_files/Run_Habitat_Model.R")
  source("lcm/LCM.sim.R")
}



# Call plot of observed vs modeled spawner return ----

# Check to see if the necessary files exist
chk_files <- lapply(spp, function(s) {
  fp <- file.path('outputs', s, 'lcm')
  
  f <- list.files(fp, 
                  pattern = 'abundance_by_sub', # Name of csv with LCM spawner data
                  full.names = TRUE)
  
  file.exists(f)
}) %>%
  unlist


# If all files exist, run this script
if (all(chk_files)) {
  print('Creating plot of observed vs modeled abundance')
  source('lcm/scripts/plot_observed_vs_modeled.R')
}