# Create lists of GSUs to be restored for each restoration factor, including wood, barriers, floodplains, beaver ponds and shade (temperature) ----
wood_gsu <- asrp_scenarios[asrp_scenarios$LW == "y", "GSU"]
barrier_gsu <- asrp_scenarios[asrp_scenarios$Barriers == "y", "GSU"]
floodplain_gsu <- asrp_scenarios[asrp_scenarios$Floodplain == "y", "GSU"]
beaver_gsu <- asrp_scenarios[asrp_scenarios$Beaver == "y", "GSU"]
shade_gsu <- asrp_scenarios[asrp_scenarios$Riparian == "y", "GSU"]
assign(paste0('shade_gsu_', y), shade_gsu , envir = .GlobalEnv)