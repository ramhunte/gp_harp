# Introduction ----

# Welcome to the NOAA Chehalis ASRP Model

# To run the model click the "Source" button in the top right corner of this box 
# You will revcieve several prompts, which you will need to select an answer

# Prompt #1 -- Would you like to clear your environment? 
#    Selecting yes will delete everything in your global environment

# Prompt #2 -- Choose a species
#    Choose a species to run.  Options are spring chinook, fall chinook, coho, steelhead or all_species
#    WARNING - the contents of the outputs folder will be deleted and recreated

# Prompt #3 -- Would you like to run stochastic egg to fry survival?
#    Choose if you want to run the model including the schostic effect of flow
#    If 'no', the model will be run in a steady state

# Prompt #4 -- Would you like to run single action scenarios?
#    Choose if you want to run additional scenarios testing the impact of each ASRP
#    action one at a time
#    If 'no', the model will run diagnostic and full ASRP scenarios only

# Prompt #5 -- Would you like to run in sensitivity mode?
#    Choose if you want to run the sensitivity analysis
#    If 'no', the model will run diagnostic and full ASRP scenarios only




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
spp <- c('all_species', 'coho', 'spring_chinook', 'fall_chinook', 'steelhead', 'chum')
fishtype <- spp[menu(spp,title = "Choose a species", graphics = TRUE)]


# Run stochastic egg to fry ?
run_stochastic_eggtofry_query <- c('no', 'yes')
run_stochastic_eggtofry <- run_stochastic_eggtofry_query[menu(run_stochastic_eggtofry_query, title = 'Run stochastic egg to fry?', graphics = TRUE)]

# Run single action scenarios?
run_single_action_query <- c('no', 'yes')
run_single_action <- run_single_action_query[menu(run_single_action_query, title = 'Run single action scenarios?', graphics = TRUE)]

# Run sensitivity mode?
run_sensitivity_query <- c('no', 'yes')
sensitivity.mode <- run_single_action_query[menu(run_single_action_query, title = 'Run sensitivity mode?', graphics = TRUE)]


# Store branch name ----
branch <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)


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

# Run function to determine operating system of computer.  This is used for the scripts that compare the current branch to dev
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
os <- get_os()


# Source the scripts ----

if (fishtype == 'all_species') {
  
  unlink("outputs", recursive = TRUE) # WARNING -- If running all species the entire outptus folder will be deleted
  
  for (s in c('coho', 'spring_chinook', 'fall_chinook', 'steelhead', 'chum')) {
    fishtype <- s
    source("hab/R_files/0-Run_Habitat_Model.R")
    source("lcm/LCM.sim.R")
    print(paste0("finished ", s))
  }
  
} else {
  
  unlink(file.path("outputs", fishtype), recursive = TRUE) # WARNING -- Entire outptus folder for species will be deleted
  source("hab/R_files/0-Run_Habitat_Model.R")
  source("lcm/LCM.sim.R")
}

source('temp_paper/spawner_plots.R')


# Call plot of observed vs modeled spawner return ----

# Check to see if the necessary files exist
chk_files <- lapply(spp[-1], function(s) {
  
  species <- str_replace(s, '_', '.')
  file.path('outputs', s, 'lcm', paste0(species, '_abundance_by_subbasin.csv')) %>% file.exists
  
}) %>%
  unlist


# If all files exist, run this script
if (all(chk_files)) {
  print('Creating plot of observed vs modeled abundance')
  source('lcm/scripts/plot_observed_vs_modeled.R')
} else {
  print('Skipping plot of observed vs modeled')
}

