#### ASRP Scenario Diagnostics ----
if (!branch %in% c("dev", "master")) {
  
  Current <- read.csv(paste0(Outputs_dir,"/Current", ".csv", sep = "")) %>% 
    select(-1)
  
  asrp_scenario_nm <- c("ASRP_Current_asrp_2019", "ASRP_scenario_1_2040", "ASRP_scenario_1_2080", "ASRP_scenario_2_2040", "ASRP_scenario_2_2080", 
                        "ASRP_scenario_3_2040", "ASRP_scenario_3_2080")
  
  for (scenario in asrp_scenario_nm) {
    
    sub <- read.csv("Excel_Files/Subbasin_names.csv", header = TRUE) %>%
      pull(Subbasin)
    
    curr <- Current
    colnames(curr)[2:length(colnames(curr))] <- as.character(sub)
    curr_long <- gather(curr, 'Subbasin', 'curr_value', 2:length(colnames(curr)))
    
    asrp <- file.path(Outputs_dir, paste0(scenario, ".csv")) %>%
      read.csv(.) %>%
      select(-1)
    colnames(asrp)[2:length(colnames(asrp))] <- as.character(sub)
    asrp_long <- gather(asrp, 'Subbasin', 'asrp_value', 2:length(colnames(asrp)))
    
    hab <- left_join(curr_long, asrp_long) %>%
      mutate(abs_diff = abs(asrp_value - curr_value),
             percent_diff = abs_diff / curr_value) %>%
      gather(version, value, curr_value, asrp_value) %>%
      mutate(version = ifelse(version == 'curr_value', 
                              "Current", 
                              scenario)) %>%
      drop_na(value) %>%
      ggplot() +
      theme_bw() +
      geom_bar(aes(Subbasin, value, fill = version), stat = "identity", position = "dodge") +
      facet_wrap(~stage_nm, scales = "free_y") + 
      theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 4)) +
      labs(title = paste0("Comparison of ", scenario),
           y = "Absolute values of parameters",
           x = "",
           caption = fishtype)
    
    save.compare <- file.path("Outputs","feature", branch, "diagnostics", fishtype,'comparisons')
    if (dir.exists(save.compare) == F) 
      {dir.create(save.compare,recursive = T)}
    
    ggsave(file.path(save.compare, paste0(scenario,"compare_to_curr", ".jpg")), width = 10, height = 6, dpi = 300)
    
    
  }
}

