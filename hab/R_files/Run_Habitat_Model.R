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

Inputs = 'hab/Inputs'

source(file.path("hab", "R_files",paste0(fishtype, "_inputs.R"))) #source fish specific variables (density, etc)

lr_length_raw <- read.csv("hab/Excel_Files/LR_Length.csv") %>%
  rename(Reach = reach)

subbasin_names <- read.csv("hab/Excel_Files/Subbasin_names.csv")

source("hab/R_files/inputs.R")
source("hab/R_files/functions.R")
source("hab/R_files/flowline.R")
source("hab/R_files/EggToFry_Survival.R")
source("hab/R_files/LgRiver_Spawning_Capacity.R")
source("hab/R_files/large_river.R")
source("hab/R_files/small_stream.R")
source("hab/R_files/backwater.R")
source("hab/R_files/floodplain.R")
source("hab/R_files/spawn.R")
source("hab/R_files/prespawn.R")
source("hab/R_files/compute_habmodel_outputs.R")
source("hab/R_files/asrp_files/asrp_scenarios.R")
if (!branch %in% c("dev", "master")) {
  source("hab/R_files/compare_scenarios.R")
}
# save.image(file ='R files/myEnvironment.RData')
