# Match GSU info with noaaid, forest and and columns from asrp_scenarios csv ----
fl_to_gsu <- flowline %>%
  select(noaaid, GSU, forest) %>%
  mutate(forest = ifelse(forest == "Yes", 
                         "y", 
                         "n")) %>%
  left_join(., asrp_scenarios %>%
              select(GSU, rest_perc_f, rest_perc_nf, primary_cr_only),
            by = "GSU") %>%
  mutate(primary_cr_only = ifelse(is.na(primary_cr_only),
                                  "n", 
                                  as.character(primary_cr_only)),
         rest_perc_f = ifelse(is.na(rest_perc_f),
                              0,
                              rest_perc_f),
         rest_perc_nf = ifelse(is.na(rest_perc_nf),
                               0,
                               rest_perc_nf))
