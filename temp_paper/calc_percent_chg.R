View(spawners.paper.all %>% 
             ungroup() %>% 
             select(year, scenario.label.nm, prcnt.change, species, n) %>% 
             mutate(scenario.label.nm = ifelse(scenario.label.nm == 'Climate Change',
                                               'cc',
                                               scenario.label.nm)) %>%
             gather(param, value, c(prcnt.change, n)) %>%
             spread(scenario.label.nm, value) %>%
             # filter(year != 'current') %>%
             mutate(rip_diff = Riparian - cc,
                    fp_diff = Floodplain - cc,
                    combined_diff = Combined - cc)) 
