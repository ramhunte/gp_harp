asrp_mvmt <- movement_scenarios %>%
  spread(mvmt_scenario_nm, Area_mvmt) %>%
  summarize(wood_only = sum(wood_only, na.rm = TRUE),
            fp_only = sum(fp_only, na.rm = TRUE),
            beaver_only = sum(beaver_only, na.rm = TRUE)) %>%
  left_join(., bind_rows(ss_2, lr_2, fp2, bw2) %>%
              filter(hab.scenario == "Current",
                     life.stage == "winter") %>%
              select(Subbasin_num, Area) %>%
              group_by(Subbasin_num) %>%
              summarize(area_curr = sum(Area, na.rm = TRUE)) %>%
              ungroup(),
            by = "Subbasin_num") %>%
  left_join(., bind_rows(asrp_ss, asrp_lr, asrp_fp) %>%
              filter(life.stage == "winter") %>%
              select(Subbasin_num, Area) %>%
              group_by(Subbasin_num) %>%
              summarize(area_scen = sum(Area, na.rm = TRUE)) %>%
              ungroup(), 
            by = "Subbasin_num") %>%
  mutate(wood_perc = (wood_only - area_curr) / area_scen,
         fp_perc = (fp_only - area_curr) / area_scen,
         beaver_perc = (beaver_only - area_curr) / area_scen,
         curr_perc = 1 - (wood_perc + beaver_perc + fp_perc),
         movement = (wood_perc * 7) + (fp_perc * 3) + (beaver_perc * 3) + (curr_perc * 11)) %>%
  select(Subbasin_num, movement)

# assign(paste0("asrp_mvmt_", x, "_", y), asrp_mvmt, env = globalenv())