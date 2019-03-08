# Introduction ----

# Welcome to the NOAA Chehalis Model. Click the "Source" button in the top right corner of this box to run the model.  After clicking "Source" you 
# will be prompted to choose a model to run.  Choose "habmodel" to run the habitat model, "lcm" to run the life cycle model, or "both" to run both
# models.  If you select "both", the habitat model will run first, then the life cycle model will follow.  You will next recieve a prompt to choose a 
# species to run.  Options are spring chinook, fall chinook, coho, and steelhead.  Eventually we hope to add an option to run all species.
# You will also recieve a prompt to choose a version of Master if you are in the master branch

# begin Noaa Chehalis Model script ----
model_run <- c('habmodel', 'lcm', 'both')
choose_model_run <- model_run[menu(model_run, title = "Which model(s) would you like to run?", graphics = TRUE)]

if (choose_model_run == 'habmodel') {
  source("hab/R_files/Run_Habitat_Model.R")
} else if (choose_model_run == 'lcm') {
  source("lcm/LCM.sim.R")
} else {
source("hab/R_files/Run_Habitat_Model.R")
source("lcm/LCM.sim.R")
}