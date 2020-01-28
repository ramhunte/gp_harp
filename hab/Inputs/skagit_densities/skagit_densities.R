# skagit_electrofish <- read.csv('hab/inputs/skagit_densities/skagit_electrofish_data_raw.csv', sep = ',', header = TRUE) %>%
#   rename(habtype = Unit.type_3_JT) %>%
#   select(-(1:9),-(11:39), -41, -(43:70), -(72:73), -(75:119), -(135:241)) %>%
#   mutate(habtype = ifelse(habtype %in% c('Bar-boulder', 'Bar-gravel'),
#                           "Bar",
#                           as.character(habtype))) %>%
# 
#   separate(col = 'Date', c('month', 'day', 'year'), sep = '/') 
# 
# skagit_electrofish$trt20.80 <- rowSums(skagit_electrofish[8:14])
# skagit_electrofish$trt80.550 <- rowSums(skagit_electrofish[15:22])
# 
# skagit_electrofish %<>%
#   mutate(density = trt20.80/Area.Sampled,
#          density_lg = trt80.550/Area.Sampled) %>%
#   group_by(month, habtype) %>%
#   summarize(sthd_95 = quantile(density, probs = .95, na.rm = T),
#             sthd_75 = quantile(density, probs = .75, na.rm = T),
#             sthd_mean = mean(density, na.rm = T), 
#             sthd_med = median(density, na.rm = T),
#             sthd_95_lg = quantile(density_lg, probs = .95, na.rm = T),
#             sthd_75_lg = quantile(density_lg, probs = .75, na.rm = T),
#             sthd_mean_lg = mean(density_lg, na.rm = T), 
#             sthd_med_lg = median(density_lg, na.rm = T)) %>%
#   filter(habtype %in% c('Bank', 'Bank HM', 'Bar', 'Backwater pool'))
#   
  

# skagit_electrofish_histogram <- read.csv('hab/inputs/skagit_densities/skagit_electrofish_data_raw.csv', sep = ',', header = TRUE) %>%
#   select(-(1:9),-(11:39), -41, -(43:73), -(75:119), -(135:241)) %>%
#   separate(col = 'Date', c('month', 'day', 'year'), sep = '/') %>%
#   gather(size_class, number, trt20.30:trt550) %>%
#   group_by(month, size_class) %>%
#   summarize(number = sum(number, na.rm = T)) 

# skagit_electrofish_histogram$size_class <- factor(skagit_electrofish_histogram$size_class, levels = c('trt20.30', 'trt30.40', 'trt30.50', 'trt40.50', 
#                                                                                                       'trt50.60', 'trt60.70', 'trt70.80', 'trt80.90',
#                                                                                                       'trt90.100', 'trt100.150', 'trt150.200', 'trt200.250',
#                                                                                                       'trt250.300', 'trt300.450', 'trt550'))
# skagit_electrofish_histogram$month <- factor(skagit_electrofish_histogram$month, levels = 1:12)
# 
# ggplot(skagit_electrofish_histogram, aes(x = size_class, y = number)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   facet_wrap(~month)+
#   theme(axis.text.x = element_text(angle = 45,hjust = 1))
# 
# ggsave('sthd_size_class.jpg', width = 10, height = 8, dpi = 300)

skagit_electrofish_2 <- read.csv('hab/inputs/skagit_densities/skagit_electrofish_data_raw.csv', sep = ',', header = TRUE) %>%
  rename(habtype = Unit.type_3_JT) %>%
  select(-(1:9),-(11:39), -41, -(43:70), -(72:73), -(75:105), -(135:241)) %>%
  mutate(habtype = ifelse(habtype %in% c('Bar-boulder', 'Bar-gravel'),
                          "Bar",
                          as.character(habtype))) %>%
  separate(col = 'Date', c('month', 'day', 'year'), sep = '/') 

skagit_electrofish_2$trt20.80 <- rowSums(skagit_electrofish_2[22:28])
skagit_electrofish_2$trt80.550 <- rowSums(skagit_electrofish_2[29:36])
skagit_electrofish_2$coho_juv <- rowSums(skagit_electrofish_2[10:18])

skagit_electrofish_2 %<>%
  select(-(10:36)) %>%
  gather('species', 'number', c(chksub, trt20.80, coho_juv, chkyrlng, trt80.550)) %>%
  mutate(species = case_when(
    species == 'trt20.80' ~ 'sthd_juv',
    species == 'trt80.550' ~ 'sthd_adult',
    species %in% c('chksub', 'coho_juv', 'chkyrlng') ~ species)) %>%
  mutate(density = number/Area.Sampled) 


coho_density_jt <- skagit_electrofish_2 %>%
  filter(species == 'coho_juv') %>%
  group_by(month, habtype) %>%
  summarize(density_95 = quantile(density, probs = .95, na.rm = T),
            density_75 = quantile(density, probs = .75, na.rm = T),
            density_mean = mean(density, na.rm = T),
            density_med = median(density, na.rm = T)) %>%
  gather('metric', 'density', density_95:density_med) %>% 
  spread(metric, density) %>%
  write.csv(., 'hab/Inputs/skagit_densities/coho_density_jt.csv')

chino_density_jt <- skagit_electrofish_2 %>%
  filter(species == 'chksub') %>%
  filter(month %in% c(2:6)) %>%
  group_by(habtype) %>%
  summarize(density_95 = quantile(density, probs = .95, na.rm = T),
            density_75 = quantile(density, probs = .75, na.rm = T),
            density_mean = mean(density, na.rm = T),
            density_med = median(density, na.rm = T)) %>%
  gather('metric', 'density', density_95:density_med)%>%
  spread(metric, density) %>%
  write.csv(., 'hab/Inputs/skagit_densities/chk_density_jt.csv')

sthd_density_jt <- skagit_electrofish_2 %>%
  filter(species == 'sthd_juv') %>%
  group_by(month, habtype) %>%
  summarize(density_95 = quantile(density, probs = .95, na.rm = T),
            density_75 = quantile(density, probs = .75, na.rm = T),
            density_mean = mean(density, na.rm = T),
            density_med = median(density, na.rm = T)) %>%
  gather('metric', 'density', density_95:density_med) %>%
  spread(metric, density) %>%
  write.csv(., 'hab/Inputs/skagit_densities/sthd_density_jt.csv')

#   
