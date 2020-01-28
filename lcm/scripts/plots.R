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
  
  
  # Organize labels to plot
  scenario.label <- plot.params$scenario.label[plot.params$scenario.label %in% unique(spawners$scenario.label)]
  
  cur.scenario.labs <- scenario.label[substr(scenario.label, 1, 1) != 'H']
  hist.scenario.labs <- scenario.label[substr(scenario.label, 1, 1) == 'H']
  
  ordered.levels <- c(cur.scenario.labs,hist.scenario.labs)
  
  spawners <- spawners %>%
    mutate(scenario.label = factor(scenario.label, levels = ordered.levels)) %>%
    droplevels()
  
  # Create spawners data frame from diagnostic scenarios only
  spawners.diag <- spawners %>%  
    filter(!scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", "ASRP 1 - 2080", 
                                  "ASRP 2 - 2080", "ASRP 3 - 2080", 'ASRP No action, with future development 2040', 
                                  'ASRP No action, with future development 2080')) %>%
    droplevels()
  
  # Create spawners data frame from asrp scenarios only  
  spawners.asrp <- spawners %>%  
    filter(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", "ASRP 1 - 2080", 
                                 "ASRP 2 - 2080", "ASRP 3 - 2080", 'ASRP No action, with future development 2040', 
                                 'ASRP No action, with future development 2080', "Current")) %>%
    mutate(year = ifelse(scenario.label %in% c("ASRP 1 - 2040", "ASRP 2 - 2040", "ASRP 3 - 2040", 'ASRP No action, with future development 2040'),
                         'mid-century',
                         ifelse(scenario.label %in% c("ASRP 1 - 2080", "ASRP 2 - 2080", "ASRP 3 - 2080", 
                                                      'ASRP No action, with future development 2080'),
                                'late-century',
                                'current')),
           scenario.label.nm = case_when(
             scenario.label %in% c("ASRP 1 - 2040", "ASRP 1 - 2080") ~ "ASRP 1",
             scenario.label %in% c("ASRP 2 - 2040", "ASRP 2 - 2080") ~ "ASRP 2",
             scenario.label %in% c("ASRP 3 - 2040", "ASRP 3 - 2080") ~ "ASRP 3",
             scenario.label %in% c('ASRP No action, with future development 2040', 'ASRP No action, with future development 2080') ~ "No action",
             scenario.label %in% c("Current") ~ "Current"))
  
  # reorder factors for asrp spawners so that x axis plotting order is correct
  spawners.asrp$scenario.label.nm <- factor(spawners.asrp$scenario.label.nm, levels = c("Current", "No action", "ASRP 1", "ASRP 2", "ASRP 3"))
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
    slice(c(1, 9, 3, 5,7)) %>%
    select(color) %>%
    unlist(use.names = FALSE)
  
  label.df.diag <- spawners.diag %>%
    group_by(scenario.label) %>%
    summarize(n = mean(n)) %>%
    mutate(spawners.change = n - n[scenario.label == 'Current'],
           prcnt.change = ((n - n[scenario.label == 'Current']) / n[scenario.label == 'Current']),
           prcnt.change = ifelse(prcnt.change >= 0,
                                 paste0('+', percent(prcnt.change, 1)),
                                 percent(prcnt.change, 1)),
           y.pos = n,
           curr.spawn = n[scenario.label == 'Current']) %>%
    filter(!scenario.label == 'Current')
  
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
                               n[scenario.label == '2080 No action'])) %>%
    filter(!scenario.label == 'Current')
  
  
  
  # Box plot of diagnostic scenarios ----
  
  ggplot() +
    theme_bw() +
    geom_bar(data = spawners.diag, 
             aes(scenario.label, n, fill = scenario.label),
             color = 'black',
             stat = "summary", 
             fun.y = "mean") +
    geom_bar(data = spawners.diag %>% 
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


  ggsave(file.path(outputs_lcm, paste0('spawners_basinwide_',pop,'.jpg')), width = 10, height = 8, dpi = 300)#pdfs 10x10
  
  
  #bar plot of asrp scenarios with stacked scenarios ----
  
  ggplot() +
    theme_bw() +
    geom_bar(data = spawners.asrp %>% filter(scenario.label.nm %in% c("Current", 'ASRP 3')), 
             aes(year, n, fill = scenario.label.nm),
             color = 'black',
             stat = "summary", 
             fun.y = "mean") +
    geom_bar(data = spawners.asrp %>% filter(scenario.label.nm %in% c("Current", 'ASRP 2')), 
             aes(year, n, fill = scenario.label.nm),
             color = 'black',
             stat = "summary", 
             fun.y = "mean") +
    geom_bar(data = spawners.asrp %>% filter(scenario.label.nm %in% c("Current", 'ASRP 1')), 
             aes(year, n, fill = scenario.label.nm),
             color = 'black',
             stat = "summary", 
             fun.y = "mean") +
    geom_bar(data = spawners.asrp %>% filter(scenario.label.nm %in% c("Current", 'No action')), 
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

  ggsave(file.path(outputs_lcm, paste0('spawners_basinwide_',pop,'_asrp_stacked','.jpg')), width = 10, height = 8, dpi = 300)#pdfs 10x10
} #close if not sensitivity mode if() statement




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
    guides(fill = guide_legend(reverse = TRUE))
  
  ggsave(file.path(outputs_lcm,paste0('sensitivity_',pop,'.jpg')), width = 10, height = 8, dpi = 300)
  
  # write.csv(rbind(broom::tidy(lm.current)%>%mutate(model = "current"),
  #                 broom::tidy(lm.hist)%>%mutate(model = "historical"))
  #           ,file.path(outputs_lcm,paste('sensitivity_',pop,'_lm.csv')))
  
  
} #close sensitivity plot if() statement





