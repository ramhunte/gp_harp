# debug(utils:::unpackPkgZip)
options(scipen = 999)

Inputs = 'hab/Inputs'

source(file.path("hab", "R_files",paste0(fishtype, "_inputs.R"))) #source fish specific variables (density, etc)

lr_length_raw <- read.csv("hab/Excel_Files/LR_Length.csv") %>%
  rename(Reach = reach)

subbasin_names <- read.csv("hab/Excel_Files/Subbasin_names.csv")


outputs_hab <- file.path("outputs", fishtype, "hab.scenarios")
if (dir.exists(outputs_hab) == F) {
  dir.create(outputs_hab, recursive = TRUE)
}

source("hab/R_files/inputs.R")
source("hab/R_files/functions.R")
source("hab/R_files/flowline.R")
source("hab/R_files/LgRiver_Spawning_Capacity.R")
source("hab/R_files/large_river.R")
source("hab/R_files/small_stream.R")
source("hab/R_files/backwater.R")
source("hab/R_files/floodplain.R")
source("hab/R_files/spawn.R")
source("hab/R_files/prespawn.R")
source("hab/R_files/EggToFry_Survival.R")
source("hab/R_files/compute_habmodel_outputs.R")
if (run_asrp == "yes") {
  source("hab/R_files/asrp_files/asrp_scenarios.R")}
source("hab/R_files/compare_scenarios.R")

# save.image(file ='R files/myEnvironment.RData')
