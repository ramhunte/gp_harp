# Plots of Chehalis LCM model outputs 

options(scipen = 999) # suppress scientific notation

# Create folder to save today's plots in
if (dir.exists(outputs_lcm) == F) {dir.create(outputs_lcm)}

# Read in file with predefined plot labels and matching colors

plot.params <- read.csv("lcm/data/scenarios.csv") %>%
  select(scenario,scenario.label,color) %>%
  mutate_if(is.factor,as.character)

subbasins <- read.csv('lcm/data/subbasin_names.csv')



# Create boxplot with total run data per scenario -------------


if (sensitivity.mode == 'no') {
  
  # Create a plotable dataframe of total run, sum of all DUs, geomean of all years, all runs plotted
  
  spawners <- model.all[ , ,'spawners', , ] %>%
    apply(.,c(1,3,4),geo.mean) %>%
    apply(.,c(1,3),sum) %>%
    as.data.frame.table(.) %>%
    rename(run = Var1, scenario = Var2, n = Freq) %>%
    mutate_if(is.factor,as.character) %>%
    left_join(plot.params) %>%
    mutate(scenario.label = ifelse(is.na(scenario.label),paste0(scenario,".Naming.ERROR"),as.character(scenario.label)),
           color = ifelse(is.na(color),'red',as.character(color))) %>%
    select(n, scenario.label,color)
  
  # Save summary output
  spawners %>%
    group_by(scenario.label) %>%
    summarize(n = mean(n,na.rm = T)) %>%
    mutate(scenario.label = as.character(scenario.label)) %>%
    inner_join(.,data.frame(habitat.file,scenario = scenario.file) %>%
                mutate(scenario = as.character(scenario)) %>%
                left_join(.,plot.params)) %>%
    select(scenario,scenario.label,habitat.file,n) %>%
    write.csv(.,file.path(outputs_lcm,paste0('spawners_',pop,'.csv')))
  
  # Organize labels to plot
  scenario.label <- plot.params$scenario.label[plot.params$scenario.label %in% unique(spawners$scenario.label)]
  
  cur.scenario.labs <- scenario.label[substr(scenario.label, 1, 1) != 'H']
  hist.scenario.labs <- scenario.label[substr(scenario.label, 1, 1) == 'H']
  
  if (pop == "coho") levs.obs <- c("Observed, 1982-2014","Observed, 1906-1914")
  if (pop == "fall.chinook") levs.obs <- c("Observed, 1986-2014","Observed, 1910-1919 (90% of reported)")
  if (pop == "spring.chinook") levs.obs <- c("Observed, 1982-2014","Observed, 1910-1919 (10% of reported Fall run)")
  if (pop == "steelhead") levs.obs <- c("Observed, 1983-2013","Observed, 1983-2013 Escapement")
  
  ordered.levels <- c(levs.obs[1],cur.scenario.labs,levs.obs[2],hist.scenario.labs)
  
  # Join oberseved and modeled data
  spawners <- spawners %>%
    bind_rows(read.csv(paste0("lcm/data/",pop,".observed.csv")) %>%
                mutate(scenario.label = as.character(ifelse(Year > 1980, levs.obs[1], levs.obs[2])),
                       color = "grey90") %>%
                filter(lifestage == 'total.run') %>%
                select(-Year,-period,-lifestage)) %>%
    mutate(scenario.label = factor(scenario.label,levels = ordered.levels)) %>%
    filter(scenario.label != levs.obs[2]) %>%
    droplevels()
  
# Create spawners data frame from diagnostic scenarios only
  spawners.diag <- spawners %>%  
    filter(!scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", "ASRP 1 - 2080", 
                                  "ASRP 2 - 2080", "ASRP 3 - 2080", '2040 No action', '2080 No action', 'ASRP 1 - no climate change 2040', 
                                  'ASRP 1 - no climate change 2080', 'ASRP 2 - no climate change 2040', 'ASRP 2 - no climate change 2080', 'ASRP 3 - no climate change 2040',
                                  'ASRP 3 - no climate change 2080')) %>%
    droplevels()
  
# Create spawners data frame from asrp scenarios only  
  spawners.asrp <- spawners %>%  
    filter(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", "ASRP 1 - 2080", 
                                 "ASRP 2 - 2080", "ASRP 3 - 2080", '2040 No action', '2080 No action', "Current",
                                 'ASRP 1 - no climate change 2040', 
                                 'ASRP 1 - no climate change 2080', 'ASRP 2 - no climate change 2040', 'ASRP 2 - no climate change 2080', 'ASRP 3 - no climate change 2040',
                                 'ASRP 3 - no climate change 2080')) %>%
    mutate(year = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", '2040 No action', 'ASRP 1 - no climate change 2040',
                                               'ASRP 2 - no climate change 2040', 'ASRP 3 - no climate change 2040'),
                          'mid-century',
                          ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080", '2080 No action', 'ASRP 1 - no climate change 2080',
                                                       'ASRP 2 - no climate change 2080', 'ASRP 3 - no climate change 2080'),
                                 'late-century',
                                 'current')),
           scenario.label.nm = case_when(
             scenario.label %in% c("ASRP 1 - 2040", "ASRP 1 - 2080") ~ "ASRP 1",
             scenario.label %in% c("ASRP 2 - 2040", "ASRP 2 - 2080") ~ "ASRP 2",
             scenario.label %in% c("ASRP 3 - 2040", "ASRP 3 - 2080") ~ "ASRP 3",
             scenario.label %in% c('ASRP 1 - no climate change 2040', 'ASRP 1 - no climate change 2080') ~ 'ASRP 1 no climate change',
             scenario.label %in% c('ASRP 2 - no climate change 2040', 'ASRP 2 - no climate change 2080') ~ 'ASRP 2 no climate change',
             scenario.label %in% c('ASRP 3 - no climate change 2040', 'ASRP 3 - no climate change 2080') ~ 'ASRP 3 no climate change',
             scenario.label %in% c("2040 No action", "2080 No action") ~ "No action",
             scenario.label %in% c("Current") ~ "Current"))

# reorder factors for asrp spawners so that x axis plotting order is correct
spawners.asrp$scenario.label.nm <- factor(spawners.asrp$scenario.label.nm, levels = c("Current", "No action", "ASRP 1", "ASRP 2", "ASRP 3",
                                                                                      'ASRP 1 no climate change', 'ASRP 2 no climate change', 'ASRP 3 no climate change'))
spawners.asrp$year <- factor(spawners.asrp$year, levels = c("current", "mid-century", "late-century"))
spawners.asrp %<>% 
  droplevels()
  
  colors.diag <- data.frame(scenario.label = levels(spawners.diag$scenario.label)) %>%
    mutate_if(is.factor,as.character) %>%
    left_join(spawners %>%
                distinct(scenario.label,color) %>%
                mutate_if(is.factor,as.character)) %>%
    select(color) %>%
    unlist(use.names = FALSE)
  
  colors.asrp <- data.frame(scenario.label = levels(spawners.asrp$scenario.label)) %>%
    mutate_if(is.factor,as.character) %>%
    left_join(spawners %>%
                distinct(scenario.label,color) %>%
                mutate_if(is.factor,as.character)) %>%
    select(color) %>%
    unlist(use.names = FALSE)
  
  colors.asrp.stack <- data.frame(scenario.label = levels(spawners.asrp$scenario.label)) %>%
    mutate_if(is.factor,as.character) %>%
    left_join(spawners %>%
                distinct(scenario.label,color) %>%
                mutate_if(is.factor,as.character)) %>%
    slice(c(1, 3, 5,7, 9, 11, 13, 15)) %>%
    select(color) %>%
    unlist(use.names = FALSE)
  
  label.df.diag <- spawners.diag %>%
    group_by(scenario.label) %>%
    summarize(n = mean(n)) %>%
    mutate(spawners.change = n - n[scenario.label == 'Current'],
           prcnt.change = ((n - n[scenario.label == 'Current']) / n[scenario.label == 'Current']),
           prcnt.change = ifelse(prcnt.change >= 0,
                                 paste0('+', percent(prcnt.change)),
                                 percent(prcnt.change)),
           y.pos = n,
           curr.spawn = n[scenario.label == 'Current']) %>%
    filter(!scenario.label %in% c('Current', levs.obs[1]))
  
  label.df.asrp <- spawners.asrp %>%
    group_by(scenario.label) %>%
    summarize(n = mean(n)) %>%
    mutate(spawners.change = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040"),
                                    n - n[scenario.label == '2040 No action'],
                                    ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080"),
                                           n - n[scenario.label == '2080 No action'],
                                           n - n[scenario.label == 'Current'])),
           prcnt.change = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040"),
                                 ((n - n[scenario.label == '2040 No action']) / n[scenario.label == '2040 No action']),
                                 ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080"),
                                        ((n - n[scenario.label == '2080 No action']) / n[scenario.label == '2080 No action']),
                                        ((n - n[scenario.label == 'Current']) / n[scenario.label == 'Current']))),
           prcnt.change = ifelse(prcnt.change >= 0,
                                 paste0('+', percent(prcnt.change)),
                                 percent(prcnt.change)),
           y.pos = n,
           base.spawn = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040"),
                               n[scenario.label == '2040 No action'],
                               ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080"),
                                      n[scenario.label == '2080 No action'],
                                      n[scenario.label == 'Current']))) %>%
    filter(!scenario.label %in% c('Current', levs.obs[1]))
  
  
  
  # Box plot of diagnostic scenarios ----
  print(
    ggplot() +
      theme_bw() +
      geom_boxplot(data = spawners.diag %>% filter(scenario.label == levs.obs[1]), 
                   aes(scenario.label, n, fill = scenario.label), 
                   outlier.shape = NA) +
      geom_jitter(data = spawners.diag %>% filter(scenario.label == levs.obs[1]),
                  aes(scenario.label, n, fill = scenario.label),
                  color = 'black', 
                  pch = 21,
                  size = 2, 
                  width = .3) +
      geom_bar(data = spawners.diag %>% filter(!scenario.label %in% c(levs.obs[1])) , 
                   aes(scenario.label, n, fill = scenario.label),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      geom_bar(data = spawners.diag %>% filter(!scenario.label %in% c(levs.obs[1])) %>% 
                 mutate(base.spawn = mean(n[scenario.label == 'Current'])), 
               aes(scenario.label, base.spawn, fill = scenario.label),
               color = 'black',
               stat = "summary", 
               fun.y = "mean",
               fill = 'grey',
               alpha = .6) +
      geom_text(data = label.df.diag, 
                aes(x = scenario.label , 
                    y = y.pos, 
                    label = prcnt.change),
                vjust = -0.5) +
      scale_x_discrete(drop = F) +
      scale_fill_manual(values = colors.diag, 
                        guide = F, drop = F) +
      scale_y_continuous(labels = comma, 
                         expand = c(0, 0,.05,0)) +
      labs(x = NULL,
           y = paste0('Spawners'),
      caption = paste0('Model version = ',hab.ver)
      ) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1),
            text = element_text( size = 16))
  ) #close print()
  
 
  ggsave(file.path(outputs_lcm, paste0('spawners_basinwide_',pop,'.jpg')), width = 10, height = 8, dpi = 300)#pdfs 10x10
 if (run_asrp == 'yes') { 
 # Bar plot of asrp scenarios ----
  print(
    ggplot() +
      theme_bw() +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1])) , 
               aes(scenario.label.nm, n, fill = scenario.label),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1])) %>% 
                 mutate(base.spawn = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040"),
                                            unique(n[scenario.label == '2040 No action']),
                                            ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080"),
                                                   unique(n[scenario.label == '2080 No action']),
                                                   0))), 
               aes(scenario.label.nm, base.spawn, fill = scenario.label),
               color = 'black',
               stat = "summary", 
               fun.y = "mean",
               fill = 'grey',
               alpha = .6) +
      facet_grid(~year, scales = 'free_x', space = 'free') +
      scale_x_discrete(drop = T) +
      scale_fill_manual(values = colors.asrp, 
                        guide = F, drop = F) +
      scale_y_continuous(labels = comma, 
                         expand = c(0, 0,.05,0)) +
      labs(x = NULL,
           y = paste0('Spawners'),
           caption = paste0('Model version = ',hab.ver)
      ) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1),
            text = element_text( size = 16))
  ) #close print()
 
  ggsave(file.path(outputs_lcm, paste0('spawners_basinwide_',pop,'_asrp','.jpg')), width = 10, height = 8, dpi = 300)#pdfs 10x10
  
 #bar plot of asrp scenarios with stacked scenarios ----
  
  print(
    ggplot() +
      theme_bw() +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 3')), 
               aes(year, n, fill = scenario.label.nm),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 2')), 
               aes(year, n, fill = scenario.label.nm),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 1')), 
               aes(year, n, fill = scenario.label.nm),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'No action')), 
               aes(year, n, fill = scenario.label.nm),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      scale_x_discrete(drop = T) +
      scale_fill_manual(values = colors.asrp.stack, drop = F, name = "Scenario") +
      scale_y_continuous(labels = comma, 
                         expand = c(0, 0,.05,0)) +
      labs(x = NULL,
           y = paste0('Spawners'),
           caption = paste0('Model version = ',hab.ver)
      ) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1),
            text = element_text( size = 16))
  ) #close print()
  
  ggsave(file.path(outputs_lcm, paste0('spawners_basinwide_',pop,'_asrp_stacked','.jpg')), width = 10, height = 8, dpi = 300) 
  
  print(
    ggplot() +
      theme_bw() +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 3 no climate change')) %>% droplevels(), 
               aes(year, n, fill = scenario.label.nm),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 2 no climate change')), 
               aes(year, n, fill = scenario.label.nm),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
      geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 1 no climate change')), 
               aes(year, n, fill = scenario.label.nm),
               color = 'black',
               stat = "summary", 
               fun.y = "mean") +
    geom_bar(data = spawners.asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'No action')) %>% 
               mutate(n = ifelse(scenario.label == 'Current',
                                 n,
                                 n[scenario.label == 'Current'])),
             aes(year, n, fill = scenario.label.nm),
             color = 'black',
             stat = "summary", 
             fun.y = "mean") +
      scale_x_discrete(drop = T) +
      scale_fill_manual(values = colors.asrp.stack, drop = F, name = "Scenario") +
      scale_y_continuous(labels = comma, 
                         expand = c(0, 0,.05,0)) +
      labs(x = NULL,
           y = paste0('Spawners'),
           caption = paste0('Model version = ',hab.ver)
      ) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1),
            text = element_text( size = 16))
  ) #close print()
  
ggsave(file.path(outputs_lcm, paste0('spawners_basinwide_',pop,'_asrp_stacked_tree_growth','.jpg')), width = 10, height = 8, dpi = 300)#pdfs 10x10
 }
} #close if not sensitivity mode if() statement






# Bar plots of each diagnostic unit --------------
if (sensitivity.mode == "no") {
  library(grid)
  
  save.path.edr <- file.path(outputs_lcm,'edr_plots')
  if (dir.exists(save.path.edr) == F) {dir.create(save.path.edr)}
  
  # Create dataframe of total run (geomean of year, mean of runs) grouped by DU and scenario 
  spawners.edr <- model.all[, , 'spawners', , ] %>%
    apply(., c(1:4), function(x)
      x / (1 - Hr)) %>% # Add harvest back in (spawners + harvest)
    apply(., c(1, 3, 4), geo.mean) %>%
    apply(., c(2, 3), mean) %>%
    round(., 0) %>% # round to whole fish
    as.data.frame.table()
  
  # If coho or fall chinook sum to EDR level
  if (pop %in% c('coho', 'fall.chinook', 'steelhead')) {
    spawners.edr <- spawners.edr %>%
      rename(Subbasin = Var1,
             scenario = Var2,
             n = Freq) %>%
      left_join(read.csv('lcm/data/subbasin_names.csv')) %>%
      group_by(scenario, EcoRegion) %>%
      summarize(spawners = sum(n, na.rm = T)) %>%
      ungroup()
  }
  
  # For spring chinook, EcoRegion is actually subbassin
  # Filter to just the few that they are in
  if (pop == 'spring.chinook') {
    spawners.edr <- spawners.edr %>%
      rename(EcoRegion = Var1, scenario = Var2, spawners = Freq) %>% #EcoRegion is actually Subbasin for Spring chinook (so few units with fish)
      filter(EcoRegion %in% c('Upper Chehalis: Above Proposed Dam',
                              'Elk Creek',
                              'South Fork Chehalis',
                              'Newaukum River',
                              'Skookumchuck River',
                              ms.reaches[1:4]))
  }
  
  spawners.edr <- spawners.edr %>%
    mutate(time.period = as.factor(ifelse(substr(scenario,1,1) == 'H', 'Historical', 'Current')),
           spawners = ifelse(EcoRegion %in% c('Upper Skookumchuck','Lower Chehalis Estuary'), 0, spawners),
           spawners = ifelse(spawners < 3,0,spawners)) %>% # Turn rounding error fish into 0
    group_by(EcoRegion) %>%
    mutate(spawners.change = spawners - spawners[scenario == 'Current'],
           prcnt.change = ((spawners - spawners[scenario == 'Current'])/spawners[scenario == 'Current']),
           spawners.change = ifelse(abs(spawners.change) < 3,0,spawners.change),
           prcnt.change = ifelse(spawners.change == 0, 0, prcnt.change)) %>%
    ungroup() %>%
    mutate(scenario = as.character(scenario)) %>%
    left_join(plot.params) %>%
    filter(EcoRegion != 'Lower Chehalis Estuary',
           EcoRegion != 'Upper Skookumchuck',
           scenario != 'Historical')
  
  # Write the total run per EDR to a file 
  spawners.edr %>%
    left_join(., data.frame(habitat.file,scenario = scenario.file) %>%
                mutate(scenario = as.character(scenario))) %>%
    write.csv(., file.path(save.path.edr, paste0('Total_Run_by_EDR_', pop, '.csv')))
  
  # Organize labels
  spawners.edr$scenario.label <- factor(spawners.edr$scenario.label, levels = scenario.label)
  dont.plot <- c('Current',"Historical.no.beaver", 'ASRP.Current.asrp')
  plot.scenario <- levels(as.factor(spawners.edr$scenario))
  plot.scenario <- setdiff(plot.scenario,dont.plot)
  plot.scenario.labs <- spawners.edr %>%
    filter(!scenario %in% dont.plot) %>%
    pull(scenario.label) %>%
    unique
  
  unique(spawners.edr$scenario.label[spawners.edr$scenario.label != 'Current'])
  
  
  #total.run.du$du <- factor(total.run.du$du,levels = reach.names)
  
  for (i in 1:(length(plot.scenario.labs))) {
    
    jpeg(paste0(save.path.edr,'/edr_plots_',pop,'_',plot.scenario[i],".jpg"),
         width = 6, height = 6, units = 'in', res = 300) # 300 dpi for digital report. Maybe 600 for print report
    
    df <- spawners.edr %>%
      filter(scenario == plot.scenario[i])
    
    total.prcnt.change <- spawners.edr %>%
      filter(scenario == 'Current' | scenario == plot.scenario[i]) %>%
      group_by(scenario) %>%
      summarize(spawners = sum(spawners)) %>%
      ungroup %>%
      summarize(prcnt.change = (spawners[scenario == plot.scenario[i]] - spawners[scenario == "Current"])/spawners[scenario == "Current"]*100) %>%
      as.numeric()
    
    prcnt.fig <- ggplot(df,aes(EcoRegion,prcnt.change)) +
      theme_bw() +
      geom_bar(stat = 'identity',fill = unique(df$color),color = 'black', position = 'dodge',na.rm = T) +
      theme(axis.text.x = element_blank(),
            axis.title.y = element_text(margin = margin(r = 10))) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL,
           y = 'Percent Difference\nfrom Current Scenario',
           title = plot.scenario.labs[i]) +
      annotate("text", x = 0, y = Inf, vjust = 1.02, hjust = -0.05,
                        label = paste0("Overall = +",format(round(total.prcnt.change, 0), big.mark = ","), "%"))
    
    tr.fig <- ggplot(df,aes(EcoRegion,spawners.change)) +
      theme_bw() +
      geom_bar(stat = 'identity',fill = unique(df$color),color = 'black',na.rm = T) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_text(margin = margin(r = 10))) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL,
           y = 'Change in Spawners\nfrom Current Scenario',
           caption = paste0('Model version = ',hab.ver)
           ) +
      annotate("text", x = 0, y = Inf, vjust = 1.02, hjust = -0.05,
              label = paste0("Total = +", format(round(df %>%
                                                      summarize(n = sum(spawners.change)) %>%
                                                      as.numeric(),
                                                    0), big.mark = ",")))
     
    grid.draw(rbind(ggplotGrob(prcnt.fig), ggplotGrob(tr.fig), size = "first"))
    
    dev.off()
  } # close scenario for loop
  

} # close save.edr.plots if statement






# If in sensitivity mode ---------
if (sensitivity.mode == 'yes') {
  
  #Create dataframe with parameter range
  param <- as.data.frame.table(sensitivity) %>%
    rename(run = Var1,
           parameters = Var2,
           value = Freq) %>%
    group_by(parameters) %>%
    summarize(min = min(value),
              max = max(value)) %>%
    mutate(
      min = ifelse(min < 2, round(min, 2), round(min, 0)),
      max = ifelse(max < 2, round(max, 2), round(max, 0)),
      range = "(x0.8 - 1.2)"
    ) %>%
    ungroup() %>%
    mutate(parameters = as.character(parameters)) %>%
    select(-min, -max) %>%
    filter(parameters != 'median')
  
  
  # Model relative influence of each parameter for 5 scenarios
  spawn.model <- as.formula(paste("geomean~", paste(colnames(sensitivity[, -length(sens.params), 1]), collapse = "+")))
  
  # Standardize the coefficients
  max.coef <- function(lmobject) {
    # finds maximum coefficient in lm() fitted object
    # 1. standardizes by dividing coefs by their standard errors
    # 2. finds the maximum absolute coefficient value after standardization
    max.value <-
      max(abs(summary(lmobject)$coef[-1, 1] / summary(lmobject)$coef[-1, 2]))
    max.value
  } #close max.coef() function
  
  
  # Function to create the lm based on the input array and the scenario
  # This function allows for iteration
  sens.model <- function(array, scenario) {
    x <- lm(spawn.model, data = as.data.frame(sensitivity[, , scenario]))
    x
  }
  
  # Create empty data.frame to store model outputs
  sens.inputs <- sens.params[-length(sens.params)]
  sens.df <- data.frame(matrix(ncol = 0, nrow = length(sens.inputs)))
  row.names(sens.df) <- sens.inputs
  
  # Iterate through each scenario and store the lm and put standardized coeffs into sens.df
  for (s in scenario.file) {
    lm <- paste0('lm.', s)
    assign(lm, sens.model(sensitivity, s))
    sens.df <- cbind(sens.df, summary(get(lm))$coef[-1, 1] / summary(get(lm))$coef[-1, 2] / max.coef(get(lm)))
  }
  colnames(sens.df) <- scenario.file #rename columns
  
  # Turn sens.df from wide to long format for easier plotting
  sens.df <- sens.df %>%
    mutate(parameters = row.names(.)) %>%
    gather(scenario, rel_influence, scenario.file[1]:scenario.file[length(scenario.file)]) %>%
    left_join(param) %>%
    mutate(range = ifelse(scenario == 'Historical', range, NA)) %>%
    left_join(plot.params, by = 'scenario')
  
  
  
  # Create histogram plot
  sens.df$parameters <- factor(sens.df$parameters, levels = rev(sens.params))
  sens.df$scenario.label <- factor(
      sens.df$scenario.label,
      levels = plot.params %>% filter(scenario %in% sens.df$scenario) %>% pull(scenario.label) %>% rev()
    )
  
  colors <- plot.params %>% filter(scenario %in% sens.df$scenario) %>% pull(color) %>% rev()
  
  print(
    ggplot(sens.df, aes(parameters, rel_influence, fill = scenario.label, label = range)) +
      theme_bw() +
      geom_bar(stat = 'identity',
               position = 'dodge',
               color = 'black') +
      coord_flip() +
      lims(y = c(-1, 1)) +
      scale_x_discrete(labels = rev(c(gsub(
        "\\.", " ", sens.params[-length(sens.params)]
      )))) +
      #scale_fill_manual(values = colors) +
      labs(x = NULL,
           y = 'Relative influence',
           title = paste0(gsub("\\.", " ", pop), ' sensitivity'),
           fill = "",
           caption = paste0('Model version = ',hab.ver)) +
      theme(axis.text = element_text(size = rel(1.2))) +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(aes(y = -1), hjust = 0, na.rm = T)
    
  )#close print()
  
  ggsave(file.path(outputs_lcm,paste0('sensitivity_',pop,'.jpg')), width = 10, height = 8, dpi = 300)
  
  # write.csv(rbind(broom::tidy(lm.current)%>%mutate(model = "current"),
  #                 broom::tidy(lm.hist)%>%mutate(model = "historical"))
  #           ,file.path(outputs_lcm,paste('sensitivity_',pop,'_lm.csv')))

  
} #close sensitivity plot if() statement





