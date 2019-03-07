# debug(utils:::unpackPkgZip)

library(tidyverse)
library(magrittr)

options(scipen = 999)

#### choose species to run ----

# Load a dropdown menu with species
spp <- c('coho', 'spring_chinook', 'fall_chinook', 'steelhead' 
         #, 'chum'
         )
fishtype <- spp[menu(spp,title = "Choose a species",graphics = TRUE)]

branch <- system(command = "git rev-parse --abbrev-ref HEAD", intern = TRUE)

if (branch == "master") {
  version_list <- paste0('v', 1:4)
  version <- version_list[menu(version_list, title = "which version are you running?", graphics = TRUE)]
}

Inputs = 'Inputs'

source(paste0("R_files/",fishtype, "_inputs.R")) #source fish specific variables (density, etc)

lr_length_raw <- read.csv("Excel_Files/LR_Length.csv") %>%
  rename(Reach = reach)

subbasin_names <- read.csv("Excel_Files/Subbasin_names.csv")

source("R_files/inputs.R")
source("R_files/functions.R")
source("R_files/flowline.R")
source("R_files/EggToFry_Survival.R")
source("R_files/LgRiver_Spawning_Capacity.R")
source("R_files/large_river.R")
source("R_files/small_stream.R")
source("R_files/backwater.R")
source("R_files/floodplain.R")
source("R_files/spawn.R")
source("R_files/prespawn.R")
source("R_files/compute_habmodel_outputs.R")
source("R_files/asrp_files/asrp_scenarios.R")
if (!branch %in% c("dev", "master")) {
  source("R_files/compare_scenarios.R")
}
# save.image(file ='R files/myEnvironment.RData')
