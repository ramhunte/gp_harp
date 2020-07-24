View(spawners.paper.all %>% 
       ungroup() %>% 
       select(year, scenario.label.nm, prcnt.change, species) %>% 
       mutate(scenario.label.nm = ifelse(scenario.label.nm == 'Climate Change',
                                         'cc',
                                         scenario.label.nm)) %>%
       spread(scenario.label.nm, prcnt.change) %>%
       filter(year != 'current') %>%
       mutate(rip_diff = Riparian - cc,
              fp_diff = Floodplain - cc,
              combined_diff = Combined - cc)) 
