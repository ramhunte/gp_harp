# Capacity Calculations ----

# 1. Read in ASRP scenarios csv and create list of scenario names and dates to loop through ----
scenario.nums <- c(levels(unique(asrp_scenarios$Scenario_num)), "Current_asrp")
scenario.years <- c(2040, 2080, 2019)

# 2. Begin loop of scenario years (Current, 2040 and 2080) ----
asrp.scenarios.loop1 <- lapply(scenario.years, function(x){
  
  source("hab/R_files/asrp_files/asrp_prep.R", local = TRUE)
  
  # Begin loop of 3 ASRP restoration scenarios ----
  asrp.scenario.loop2 <- lapply(scenario.nums, function(y){
    if (x == 2019 & y == "Current_asrp" | x == 2040 & y %in% c("scenario_1", "scenario_2", "scenario_3") | x == 2080 & y %in% 
        c("scenario_1", "scenario_2", "scenario_3")) {
      
      asrp_scenarios %<>%
        filter(Scenario_num == as.character(y))
      
      # Match GSU info with noaaid, forest and and columns from asrp_scenarios csv ----
      
      source("hab/R_files/asrp_files/fl_to_gsu.R", local = TRUE)
      
      source("hab/R_files/asrp_files/primary_cr.R", local = TRUE)
      
      source("hab/R_files/asrp_files/movement.R", local = TRUE)
      
      source("hab/R_files/asrp_files/gsu_list.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_culvs.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_ss.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_lr.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_fp.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_cap.R", local = TRUE)
      
      source("hab/R_files/asrp_files/movement_2.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_prod.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_spawn.R", local = TRUE)
      
      source("hab/R_files/asrp_files/asrp_prespawn.R", local = TRUE)
      
      source("hab/R_files/asrp_files/data_organization.R", local = TRUE)
      
      
      write.csv(asrp_results, file = file.path(outputs_hab, paste0("ASRP_", y, "_", x, ".csv")))
      
      # asrp_results_df <- asrp_results %>%
      #   mutate(asrp_scenario = y,
      #          year = x)
      
      # print(paste0("FINISHED_", x, y))
    }
  }) %>%
    do.call('rbind',.)
  }) %>%
  do.call('rbind',.) 

write.csv(data, file.path(outputs_hab, "outputs_long", "habmodel_outputs.csv"))

# source("R_files/asrp_files/create_scenario_dataframes.R", local = TRUE)

# source("hab/R_files/asrp_files/asrp_diagnostics.R", local = TRUE)

# files_to_delete <- c("/ASRP_Current_asrp_2040_", "/ASRP_Current_asrp_2080_", "/ASRP_scenario_1_2019_", "/ASRP_scenario_2_2019_", "/ASRP_scenario_3_2019_")

# for (i in files_to_delete) {
  # file.remove(paste0(outputs_hab, i, ".csv"))
# }


