# debug(utils:::unpackPkgZip)
options(scipen = 999)

Inputs = 'hab/Inputs'

outputs_hab <- file.path("outputs", fishtype, "hab.scenarios")
if (dir.exists(outputs_hab) == F) {
  dir.create(outputs_hab, recursive = TRUE)
}

if (dir.exists(file.path(outputs_hab, "outputs_long")) == F) {
  dir.create(file.path(outputs_hab, "outputs_long"), recursive = T)
}

source(file.path("hab", "R_files", '1-species_input_files', paste0(fishtype, "_inputs.R"))) #source fish specific variables (density, etc)
source("hab/R_files/2-read_in_data.R")
source("hab/R_files/4-inputs.R")
source("hab/R_files/5-functions.R")
source("hab/R_files/6-chino_mult.R")
source("hab/R_files/7-flowline.R")
source("hab/R_files/8-wood_script.R")
source("hab/R_files/9-prep.R")
source("hab/R_files/10-reach_level_data.R")
source("hab/R_files/12-ss.R")
source("hab/R_files/13-lr.R")
source("hab/R_files/14-fp.R")
source("hab/R_files/15-capacity_and_productivity.R")
# Note: 16-movement.R is sourced from within 16-capacity_and_productivity
source("hab/R_files/17-lr_spawn_cap.R")

source("hab/R_files/18-spawn.R")
source("hab/R_files/19-egg_to_fry.R")
source("hab/R_files/20-prespawn.R")
source("hab/R_files/21-data_organization.R")

if (!branch %in% c('dev','master')) {
  source("hab/R_files/22-compare_scenarios.R")
}
