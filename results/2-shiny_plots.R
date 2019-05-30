# Read in spawner data.  Spawner data csv is summed to the subbasin level.  It can be further summed to the EDR or basinwide level as needed ----
spawner_data <- read.csv(file.path(outputs_lcm, paste0(pop, '_abundance_by_subbasin.csv')), header = TRUE)
spawner_observed <- read.csv(paste0('lcm/data/', pop, '.observed.csv'))

# Read in file with predefined plot labels and matching colors and subbasin names file ----

plot.params <- read.csv("lcm/data/scenarios.csv") %>%
  select(scenario,scenario.label,color) %>%
  mutate_if(is.factor,as.character)

subbasins <- read.csv('lcm/data/subbasin_names.csv')

# Total run per scenario boxplot ----

# Organize labels to plot 

scenario.label <- plot.params$scenario.label[plot.params$scenario.label %in% unique(spawners$scenario.label)]

cur.scenario.labs <- scenario.label[substr(scenario.label, 1, 1) != 'H']
hist.scenario.labs <- scenario.label[substr(scenario.label, 1, 1) == 'H']

if (pop == "coho") levs.obs <- c("Observed, 1982-2014","Observed, 1906-1914")
if (pop == "fall.chinook") levs.obs <- c("Observed, 1986-2014","Observed, 1910-1919 (90% of reported)")
if (pop == "spring.chinook") levs.obs <- c("Observed, 1982-2014","Observed, 1910-1919 (10% of reported Fall run)")
if (pop == "steelhead") levs.obs <- c("Observed, 1983-2013","Observed, 1983-2013 Escapement")

ordered.levels <- c(levs.obs[1],cur.scenario.labs,levs.obs[2],hist.scenario.labs)

# Summarize spawner data to basin level and join observed and modeled data ----

spawners_basinwide <- spawner_data %>%
  group_by(scenario) %>%
  summarize(n = sum(spawners, na.rm = T)) %>%
  left_join(., plot.params) %>%
  bind_rows(., spawner_observed %>%
              mutate(scenario = as.character(ifelse(Year > 1980, levs.obs[1], levs.obs[2])),
                     color = 'grey90',
                     scenario.label = scenario) %>%
              filter(lifestage == 'total.run') %>%
              select(-Year, -period, -lifestage)) %>%
  mutate(scenario.label = factor(scenario.label,levels = ordered.levels)) %>%
  filter(scenario.label != levs.obs[2]) %>%
  droplevels()

# Plot diagnostic scenarios basinwide spawners ----  
   
spawners_basinwide_diag <- spawners_basinwide %>%  
  filter(!scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", "ASRP 1 - 2080", 
                                "ASRP 2 - 2080", "ASRP 3 - 2080", '2040 No action', '2080 No action')) %>%
  droplevels()

colors.diag <- data.frame(scenario.label = levels(spawners_basinwide_diag$scenario.label)) %>%
  mutate_if(is.factor,as.character) %>%
  left_join(spawners_basinwide_diag %>%
              distinct(scenario.label,color) %>%
              mutate_if(is.factor,as.character)) %>%
  select(color) %>%
  unlist(use.names = FALSE)

label.df.diag <- spawners_basinwide_diag %>%
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

diag_plot <- ggplot() +
  theme_bw() +
  geom_boxplot(data = spawners_basinwide_diag %>% filter(scenario.label == levs.obs[1]), 
               aes(scenario.label, n, fill = scenario.label), 
               outlier.shape = NA) +
  geom_jitter(data = spawners_basinwide_diag %>% filter(scenario.label == levs.obs[1]),
              aes(scenario.label, n, fill = scenario.label),
              color = 'black', 
              pch = 21,
              size = 2, 
              width = .3) +
  geom_bar(data = spawners_basinwide_diag %>% filter(!scenario.label %in% c(levs.obs[1])) , 
           aes(scenario.label, n, fill = scenario.label),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  geom_bar(data = spawners_basinwide_diag %>% filter(!scenario.label %in% c(levs.obs[1])) %>% 
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

# Plot asrp scenarios basinwide spawners ----
spawners_basinwide_asrp <- spawners_basinwide %>%  
  filter(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", "ASRP 1 - 2080", 
                               "ASRP 2 - 2080", "ASRP 3 - 2080", '2040 No action', '2080 No action', "Current")) %>%
  mutate(year = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", '2040 No action'),
                       'mid-century',
                       ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080", '2080 No action'),
                              'late-century',
                              'current')),
         scenario.label.nm = case_when(
           scenario.label %in% c("ASRP 1 - 2040", "ASRP 1 - 2080") ~ "ASRP 1",
           scenario.label %in% c("ASRP 2 - 2040", "ASRP 2 - 2080") ~ "ASRP 2",
           scenario.label %in% c("ASRP 3 - 2040", "ASRP 3 - 2080") ~ "ASRP 3",
           scenario.label %in% c("2040 No action", "2080 No action") ~ "No action",
           scenario.label %in% c("Current") ~ "Current"))

spawners_basinwide_asrp$scenario.label.nm <- factor(spawners_basinwide_asrp$scenario.label.nm, levels = c("Current", "No action", "ASRP 1", "ASRP 2", "ASRP 3"))
spawners_basinwide_asrp$year <- factor(spawners_basinwide_asrp$year, levels = c("current", "mid-century", "late-century"))
spawners_basinwide_asrp %<>% 
  droplevels()

colors.asrp.stack <- data.frame(scenario.label = levels(spawners_basinwide_asrp$scenario.label)) %>%
  mutate_if(is.factor,as.character) %>%
  left_join(spawners %>%
              distinct(scenario.label,color) %>%
              mutate_if(is.factor,as.character)) %>%
  slice(c(1, 3, 5,7, 9)) %>%
  select(color) %>%
  unlist(use.names = FALSE)

label.df.asrp <- spawners_basinwide_asrp %>%
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
                             n[scenario.label == '2080 No action'])) %>%
  filter(!scenario.label %in% c('Current', levs.obs[1]))

asrp_plot <- ggplot() +
  theme_bw() +
  geom_bar(data = spawners_basinwide_asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 3')), 
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  geom_bar(data = spawners_basinwide_asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 2')), 
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  geom_bar(data = spawners_basinwide_asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'ASRP 1')), 
           aes(year, n, fill = scenario.label.nm),
           color = 'black',
           stat = "summary", 
           fun.y = "mean") +
  geom_bar(data = spawners_basinwide_asrp %>% filter(!scenario.label %in% c(levs.obs[1]), scenario.label.nm %in% c("Current", 'No action')), 
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



#Create single lookup table with subbasin level data ----

shiny_lookup_tbl <- lookup_tbl %>%
  select(-Subbasin_num) %>%
  gather(param, data, c(adults.capacity:winter.survival, spawners:non.natal.smolts)) %>%
  group_by(Subbasin, param, species) %>%
  mutate(data = as.numeric(data),
         dat.chg = data - data[scenario == 'Current'],
         prcnt.chg = ((data - data[scenario == 'Current'])/data[scenario == 'Current'])) %>%
  gather(basin_type, basin, Subbasin:EcoRegion) %>%
  group_by(scenario, param, species, basin_type, basin) %>%
  summarize(data = sum(data, na.rm = T),
            dat.chg = sum(dat.chg, na.rm = T),
            prcnt.chg = sum(prcnt.chg, na.rm = T)) %>%
  left_join(., plot.params)

