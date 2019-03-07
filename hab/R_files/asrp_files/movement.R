# The goal is to calculate separate values of area under 3 different conditions: wood restoration only, fp restoration only, 
# and beaver restoration only

# One way to achieve this will be to create new gsu lists.

# These lists will allow me to calculate 

mvmt_gsu_list <- c("wood_only", "fp_only", "beaver_only")

movement_scenarios <- lapply(mvmt_gsu_list, function(d){
  
  mvmt_scenario <- d
  
  if (mvmt_scenario == "wood_only") {
    wood_gsu <- asrp_scenarios[asrp_scenarios$LW == "y", "GSU"]
  } else {wood_gsu <- asrp_scenarios[asrp_scenarios$LW == "q", "GSU"]}
  
  if (mvmt_scenario == "fp_only") {
    floodplain_gsu <- asrp_scenarios[asrp_scenarios$Floodplain == "y", "GSU"]
  } else {floodplain_gsu <- asrp_scenarios[asrp_scenarios$Floodplain == "q", "GSU"]}
  
  if (mvmt_scenario == "beaver_only") {
    beaver_gsu <- asrp_scenarios[asrp_scenarios$Beaver == "y", "GSU"]
  } else {beaver_gsu <- asrp_scenarios[asrp_scenarios$Beaver == "q", "GSU"]}
  
  barrier_gsu <- asrp_scenarios[asrp_scenarios$Barriers == "q", "GSU"]
  shade_gsu <- asrp_scenarios[asrp_scenarios$Riparian == "q", "GSU"]
  
  source("R_files/asrp_files/asrp_culvs.R", local = TRUE)
  
  source("R_files/asrp_files/asrp_ss.R", local = TRUE)
  
  source("R_files/asrp_files/asrp_lr.R", local = TRUE)
  
  source("R_files/asrp_files/asrp_fp.R", local = TRUE)
  
  mvmt_data <- bind_rows(asrp_ss, asrp_lr, asrp_fp) %>%
    mutate(mvmt_scenario_nm = d) %>%
    filter(life.stage == "winter") %>%
    group_by(Subbasin_num, mvmt_scenario_nm) %>%
    summarize(Area_mvmt = sum(Area, na.rm = T))
}) %>%
  do.call("rbind",.) 

