# # Analyze the model runs from CIG
# 
# # Working towards percent difference in peak flow in 2040 and 2080 under low and high emissions scenario
# 
# gm_hist <- read.csv("C:/01_Projects/Chehalis/ASRP/misc/AGU_poster/CIG_model_results/streamflow_summaries/VIC/RAW/ChehalisR-nrGrandMound/MACA/ChehalisR-nrGrandMound_MACA-historical-full_VIC_RAW_PeakFlows.csv")
# 
# gm_rcp45 <- read.csv("C:/01_Projects/Chehalis/ASRP/misc/AGU_poster/CIG_model_results/streamflow_summaries/VIC/RAW/ChehalisR-nrGrandMound/MACA/ChehalisR-nrGrandMound_MACA-RCP45-full_VIC_RAW_PeakFlows.csv")
# 
# gm_rcp85 <- read.csv("C:/01_Projects/Chehalis/ASRP/misc/AGU_poster/CIG_model_results/streamflow_summaries/VIC/RAW/ChehalisR-nrGrandMound/MACA/ChehalisR-nrGrandMound_MACA-RCP85-full_VIC_RAW_PeakFlows.csv")
# 
# 
# gm_hist %>%
#   rbind(gm_rcp45) %>%
#   select(WYear:IPSL.CM5A.MR) %>%
#   gather(model, Q, bcc.csm1.1.m:IPSL.CM5A.MR) %>%
#   mutate(Q = Q * 0.0283168) %>%
#   ggplot +
#   geom_line(aes(WYear, Q, color = model))
# 
# 
# gm_rcp45 %>%
#   #full_join(data.frame(WYear = usgs_gm$waterYear)) %>%
#   gather(model, Q, bcc.csm1.1.m:NorESM1.M) %>%
#   filter(!WYear %in% usgs_gm$waterYear) %>%
#   mutate(Q = Q * 0.0283168) %>%
#   bind_rows(usgs_gm %>%
#               select(-RI) %>%
#               rename(WYear = waterYear,
#                      Q = Q_max) %>%
#               mutate(model = 'observed')) %>%
#   filter(model %in% c('observed','bcc.csm1.1.m')) %>%
#   ggplot +
#   geom_line(aes(WYear, Q, color = model))
# 
# 
# 
# #################################
# 
# gm_a1b <- read.csv('C:/01_Projects/Chehalis/ASRP/misc/AGU_poster/CIG_model_results/streamflow_summaries/VIC/BCday/ChehalisR-nrGrandMound/bc-WRF/ChehalisR-nrGrandMound_bc-WRF-SRES-A1B-full_VIC_BCday_PeakFlows.csv')
# 
# gm_a1b %>%
#   #full_join(data.frame(WYear = usgs_gm$waterYear)) %>%
#   gather(model, Q, CCSM3.A1B:ECHAM5.A1B) %>%
#   filter(!WYear %in% usgs_gm$waterYear) %>%
#   mutate(Q = Q * 0.0283168) %>%
#   bind_rows(usgs_gm %>%
#               select(-RI) %>%
#               rename(WYear = waterYear,
#                      Q = Q_max) %>%
#               mutate(model = 'observed')) %>%
#   #filter(model %in% c('observed','bcc.csm1.1.m')) %>%
#   ggplot +
#   geom_line(aes(WYear, Q, color = model))


###############################

# Peak stats

model_diff <- list.files('../misc/AGU_poster/CIG_model_results/streamflow_summaries/VIC/BCday/ChehalisR-nrGrandMound/MACA/',
           pattern =  'PeakStats.csv',
           full.names = TRUE)[1:5] %>%
  map_dfr(.f = function(x){
    x %>%
      read.csv %>%
      mutate(file = basename(x))
    }) %>%
  mutate(file = case_when(
    file == 'ChehalisR-nrGrandMound_MACA-historical-1980s_VIC_BCday_PeakStats.csv' ~ 'historical',
    file == 'ChehalisR-nrGrandMound_MACA-RCP45-2050s_VIC_BCday_PeakStats.csv' ~ 'RCP45-2050s',
    file == 'ChehalisR-nrGrandMound_MACA-RCP45-2080s_VIC_BCday_PeakStats.csv' ~ 'RCP45-2080s',
    file == 'ChehalisR-nrGrandMound_MACA-RCP85-2050s_VIC_BCday_PeakStats.csv' ~ 'RCP85-2050s',
    file == 'ChehalisR-nrGrandMound_MACA-RCP85-2080s_VIC_BCday_PeakStats.csv' ~ 'RCP85-2080s'
  )) %>%
  gather(model, Q, bcc.csm1.1.m:NorESM1.M) %>%
  group_by(returnYr, model) %>%
  mutate(hist = Q[file == 'historical'],
         diff = Q - hist,
         diff_perc = diff/hist) %>%
  filter(file != 'historical') %>%
  separate(file, c('emissions', 'era')) %>%
  group_by(returnYr, emissions, era) %>%
  summarize(diff_perc = mean(diff_perc))


mods <- model_diff %>%
  group_by(emissions, era) %>%
  nest %>%
  mutate(fit = map(data, ~ lm(diff_perc ~ 0 + log(returnYr), data = .x)))
         #params = map(fit, broom::tidy)) %>%
 # select(emissions, era, fit, params) %>%
  #unnest

# data.frame(returnYr = 1:500) %>%
#   mutate(pred_rcp45_2050 = predict(mods$fit[[1]], newdata = .))
# 
# predict(mods$fit[[1]], newdata = data.frame(returnYr = 1:6))
# 
# 
# lm(diff_perc ~ log(returnYr) + emissions + era, data = model_diff)
# 
# 
# 
# model_diff %>%
#  # filter(returnYr < 200) %>%
#   ggplot +
#   geom_point(aes(log(returnYr), diff_perc, color = emissions, shape = era)) +
#   geom_abline(slope = .18, intercept = -0.05)
