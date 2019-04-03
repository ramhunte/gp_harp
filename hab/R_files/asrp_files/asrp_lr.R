#### 2.b. Large River and Backwater ----
asrp_bw <- asrp_bw_raw %>%
  left_join(., fl_to_gsu) %>%
  mutate(Area_ha = ifelse(Period == "Hist",
                          ifelse(GSU %in% floodplain_gsu & forest == "n",
                                 Area_ha * rest_perc_nf * fp_intensity_scalar_nf,
                                 0),
                          Area_ha))


asrp_lr <- asrp_lr_raw %>%
  filter(Period %in% c("Curr", "Both")) %>%
  left_join(., edt_width %>% 
              filter(year == x),
            by = "Reach_low") %>%
  bind_rows(., asrp_bw) %>%
  left_join(., wood_data) %>%
  left_join(., fl_to_gsu) %>%
  left_join(., asrp_culvs) %>%
  mutate(GSU = ifelse(primary_cr_only == "y",
                      ifelse(Reach %in% primary_cr,
                             GSU,
                             paste0(GSU, "_np")),
                      GSU),
         woodmult_s_asrp = ifelse(GSU %in% wood_gsu,
                                  ifelse(forest == "y",
                                         1 + ((woodmult_s - 1) * rest_perc_f * wood_intensity_scalar_f),
                                         1 + ((woodmult_s - 1) * rest_perc_nf * wood_intensity_scalar_nf)),
                                  1),
         woodmult_w_asrp = ifelse(GSU %in% wood_gsu,
                                  ifelse(forest == "y",
                                         1 + ((woodmult_w - 1) * rest_perc_f * wood_intensity_scalar_f),
                                         1 + ((woodmult_w - 1) * rest_perc_nf * wood_intensity_scalar_nf)),
                                  1),
         asrp_temp = ifelse(GSU %in% shade_gsu,
                            asrp_temp_w_growth,
                            ifelse(can_ang > 170,
                                   asrp_temp_cc_only,
                                   asrp_temp_w_growth)),
         species = fishtype,
         tempmult.asrp = ifelse(species %in% c("coho", "steelhead"),
                                temp_func(asrp_temp),
                                1),    # set tempmult to 1 for chinook scenarios so that temperature does not restrict rearing capacity and survival
         summer.area = area_s * tempmult.asrp * woodmult_s_asrp,
         winter.area = area_w * woodmult_w_asrp) %>%
  gather(life.stage, Area, summer.area:winter.area) %>%
  mutate(life.stage = ifelse(life.stage == "summer.area",
                             "summer",
                             "winter"),
         Area = ifelse(pass_tot_asrp == 0,
                       0,
                       Area)) %>%
  select(Subbasin_num, noaaid, Habitat, GSU, forest, pass_tot_asrp, woodmult_s_asrp, woodmult_w_asrp, tempmult.asrp, life.stage, Area, rest_perc_nf, 
         rest_perc_f, both_chk)

if (fishtype == "spring_chinook") {
  asrp_lr %<>%
    rename(Area_nochino = Area) %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area_nochino * schino_mult,
                         Area_nochino))
} else if (fishtype == "fall_chinook") {
  asrp_lr %<>%
    rename(Area_nochino = Area) %>%
    mutate(Area = ifelse(both_chk == "Yes" | Subbasin_num %in% mainstem.subs,
                         Area_nochino * fchino_mult,
                         Area_nochino))
}