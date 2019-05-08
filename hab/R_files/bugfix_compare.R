View(ss_2 %>% filter(hab.scenario == "Current") %>% group_by(life.stage) %>% summarize(Area = sum(Area, na.rm = T)))
View(asrp_ss %>% filter(year == 2019 & Scenario_num == "Current_asrp") %>% group_by(life.stage) %>% summarize(Area = sum(Area, na.rm = T)))

View(ss_sp1 %>% filter(hab.scenario == "Current") %>% summarize(eggs = sum(eggs, na.rm = T)))
View(asrp_spawn_ss %>% filter(year == 2019 & Scenario_num == "Current_asrp") %>% summarize(eggs = sum(eggs, na.rm = T)))

View(lr_2 %>% bind_rows(., bw2) %>%filter(hab.scenario == "Current") %>% group_by(life.stage) %>% summarize(Area = sum(Area, na.rm = T)))
View(asrp_lr %>% filter(year == 2019 & Scenario_num == "Current_asrp") %>% group_by(life.stage) %>% summarize(Area = sum(Area, na.rm = T)))

View(lrsp1 %>% filter(hab.scenario == "Current") %>% summarize(eggs = sum(eggs, na.rm = T)))
View(asrp_spawn_lr %>% filter(year == 2019 & Scenario_num == "Current_asrp") %>% summarize(eggs = sum(eggs, na.rm = T)))



View(fp2 %>% filter(hab.scenario == "Current") %>% group_by(life.stage) %>% summarize(Area = sum(Area, na.rm = T)))
View(asrp_fp %>% filter(year == 2019 & Scenario_num == "Current_asrp") %>% group_by(life.stage) %>% summarize(Area = sum(Area, na.rm = T)))

View(fps1 %>% filter(hab.scenario == "Current") %>% summarize(eggs = sum(eggs, na.rm = T)))
View(asrp_spawn_fp %>% filter(year == 2019 & Scenario_num == "Current_asrp") %>% summarize(eggs = sum(eggs, na.rm = T)))
