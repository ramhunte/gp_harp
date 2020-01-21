bh_electrofish_2 <- read.csv('hab/inputs/skagit_densities/beamer_and_henderson_electrofish_raw.csv', sep = ',', header = TRUE) %>%
  select(-(2:3), -(6:16), -19, -(24:27), -(32:46)) %>%
  separate(col = 'Date', c('month', 'day', 'year'), sep = '/') %>%
  filter(!month %in% c('max', 'min')) %>%
  mutate(Unit.Type = ifelse(Unit.Type == 'Bar edge',
                            ifelse(Dominant.Substrate == 'Boulder',
                                   'Bar edge boulder',
                                   ifelse(Dominant.Substrate %in% c('Gravel', 'Cobble'),
                                          'Bar edge gravel',
                                          'Bar edge sand')),
                            as.character(Unit.Type))) %>%
  gather('species', 'number', Chinook.0.:Coho.0.) %>%
  # filter(Unit.Number == 3) %>%
  mutate(
    number = ifelse(number == '-0-', 0, as.numeric(number)),
    species = case_when(
      species == 'Chinook.0.' ~ 'chksub',
      species == 'Chinook.1.' ~ 'chkyrlng',
      species == 'Rainbow.0.' ~ 'sthd_juv',
      species == 'Coho.0.' ~ 'coho_juv',
      species %in% c('Rainbow.1..or.older', "Chum.0.", "Resident.Rainbow.Adult") ~ species),
    density = number / 3.14) %>%
  group_by(Unit.Type, month, species) %>%
  summarize(density_95 = quantile(density, probs = .95, na.rm = T),
            density_75 = quantile(density, probs = .75, na.rm = T),
            density_mean = mean(density, na.rm = T),
            density_med = median(density, na.rm = T)) %>%
  gather('metric', 'density', density_95:density_med)

bh_electrofish_chk <- read.csv('hab/inputs/skagit_densities/beamer_and_henderson_electrofish_raw.csv', sep = ',', header = TRUE) %>%
  select(-(2:3), -(6:16), -19, -(24:27), -(32:46)) %>%
  separate(col = 'Date', c('month', 'day', 'year'), sep = '/') %>%
  filter(!month %in% c('max', 'min')) %>%
  mutate(Unit.Type = ifelse(Unit.Type == 'Bar edge',
                            ifelse(Dominant.Substrate == 'Boulder',
                                   'Bar edge boulder',
                                   ifelse(Dominant.Substrate %in% c('Gravel', 'Cobble'),
                                          'Bar edge gravel',
                                          'Bar edge sand')),
                            as.character(Unit.Type))) %>%
  gather('species', 'number', Chinook.0.:Coho.0.) %>%
  # filter(Unit.Number == 3) %>%
  mutate(
    number = ifelse(number == '-0-', 0, as.numeric(number)),
    species = case_when(
      species == 'Chinook.0.' ~ 'chksub',
      species == 'Chinook.1.' ~ 'chkyrlng',
      species == 'Rainbow.0.' ~ 'sthd_juv',
      species == 'Coho.0.' ~ 'coho_juv',
      species %in% c('Rainbow.1..or.older', "Chum.0.", "Resident.Rainbow.Adult") ~ species),
    density = number / 3.14) %>%
  filter(month %in% c(2:6)) %>%
  group_by(Unit.Type, species) %>%
  summarize(density_95 = quantile(density, probs = .95, na.rm = T),
            density_75 = quantile(density, probs = .75, na.rm = T),
            density_mean = mean(density, na.rm = T),
            density_med = median(density, na.rm = T)) %>%
  gather('metric', 'density', density_95:density_med)

coho_density_bh <- bh_electrofish_2 %>%
  filter(species == 'coho_juv') %>%
  spread(metric, density) %>%
  select(-species) %>%
  write.csv(., 'hab/Inputs/skagit_densities/coho_density_bh.csv')

chino_density_bh <- bh_electrofish_chk %>%
  filter(species == 'chksub') %>%
  spread(metric, density) %>%
  select(-species) %>%
  group_by(Unit.Type) %>%
  # summarize(density_95 = mean(density_95))
  write.csv(., 'hab/Inputs/skagit_densities/chino_density_bh.csv')

sthd_density_bh <- bh_electrofish_2 %>%
  filter(species == 'sthd_juv') %>%
  spread(metric, density) %>%
  select(-species) %>%
  write.csv(., 'hab/Inputs/skagit_densities/sthd_density_bh.csv')
