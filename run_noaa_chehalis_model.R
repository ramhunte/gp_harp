# Introduction ----

# Welcome to the NOAA Chehalis Model. Click the "Source" button in the top right corner of this box to run the model. You will next recieve a prompt to choose a 
# species to run.  Options are spring chinook, fall chinook, coho, and steelhead.  Eventually we hope to add an option to run all species.
# You will also recieve a prompt to choose a version of Master if you are in the master branch

library(tidyverse)
library(magrittr)

clear_env_query <- c('yes', 'no')
clear_env <- clear_env_query[menu(clear_env_query, title = "Would you like to clear your environment?", graphics = TRUE)]

if (clear_env == 'yes') {
  rm(list = ls())
}

# begin Noaa Chehalis Model script ----

branch <- system(command = "git rev-parse --abbrev-ref HEAD", intern = TRUE)
if (branch == "master") {
  version_list <- paste0('v', 1:4)
  master_version <- version_list[menu(version_list, title = "which version are you running?", graphics = TRUE)]
}

spp <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead', 'all_species')
fishtype <- spp[menu(spp,title = "Choose a species",graphics = TRUE)]

if (fishtype == 'all_species') {
  for (s in c('coho', 'spring_chinook', 'fall_chinook', 'steelhead')) {
    fishtype = s
    source("hab/R_files/Run_Habitat_Model.R")
    source("lcm/LCM.sim.R")
    print(paste0("finished ", s))
  }
} else {source("hab/R_files/Run_Habitat_Model.R")
  source("lcm/LCM.sim.R")
}