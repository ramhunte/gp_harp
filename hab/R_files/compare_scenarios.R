# Script to compare two habitat scenarios

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


# Manually define the scenarios you want to compare ----


species <- fishtype# Could set this to fish type if we wanted to run it every time
old_file_folder <- 'dev'
new_file_folder <- paste0("feature/", branch)

# Everything below here should run automatically when sourced

#Define function to read in two hab csv files ----

# Create comparison dataframe

compare_hab <- function(hab_old,hab_new){
  
  sub <- read.csv("hab/Excel_Files/Subbasin_names.csv", header = TRUE) %>%
    pull(Subbasin)
  
  # Read in old hab scenario
  hab_old <- read.csv(hab_old) %>%
    select(-X)
  
  colnames(hab_old)[2:length(colnames(hab_old))] <- as.character(sub)
  
  hab_old_long <- gather(hab_old,'Subbasin','Old_Value',2:length(colnames(hab_old)))
  
  #Read in new habitat scenario
  hab_new <- read.csv(hab_new) %>%
    select(-X)
  
  colnames(hab_new)[2:length(colnames(hab_new))] <- as.character(sub)
  
  hab_new_long <- gather(hab_new,'Subbasin','New_Value',2:length(colnames(hab_new)))
  
  # Join both scenarios
  hab <- full_join(hab_old_long,hab_new_long) %>%
    mutate(diff = New_Value - Old_Value,
           prcnt_diff = diff/Old_Value)
  
  
  return(hab)
  
} # End compare_hab function



# Create folder for images
save.compare <- file.path('hab','Outputs', 'feature',branch,'diagnostics',species,'comparisons')
if (dir.exists(save.compare) == F) {dir.create(save.compare,recursive = T)}

# Loop through the comparison of each hab scenario
scenarios <- read.csv('hab/Excel_Files/scenarios.csv') %>%
  filter(scenario != 'Historical.no.beaver') %>%
  pull(scenario) %>%
  gsub('\\.','_',.)

compare_list <- list()
i <- 1

for (scenario in scenarios) {
  # Create file paths to scenario csvs
  
  old <- paste0('hab/Outputs/',old_file_folder,'/hab.scenarios/',species, "/", scenario,".csv")
                   
  
  new <- paste0('hab/Outputs/',new_file_folder,'/hab.scenarios/',species,'/',scenario,".csv")
  
  compare <- compare_hab(old,new)
  
  compare_list[[i]] <- compare %>%
    mutate(scenario_nm = scenario)
  
  compare %>%
    gather(version,value,Old_Value:New_Value) %>%
    mutate(version = ifelse(version == 'Old_Value',old_file_folder,new_file_folder)) %>%
    drop_na(value) %>%
    ggplot() +
    theme_bw() +
    geom_bar(aes(Subbasin,value,fill = version),stat= 'identity',position = 'dodge') +
    facet_wrap(~stage_nm,scales = 'free_y') +
    theme(axis.text.x = element_text(angle = 70,hjust=1,size = 4)) +
    #guides(fill=FALSE)+
    #scale_y_continuous(labels = scales::percent)+
    labs(title = paste0('Comparison of ', scenario),
         y = 'Absolute Values of Parameters',
         x = '',
         caption = species)
  
  ggsave(file.path(save.compare,paste0(scenario,'.jpg')), width = 10, height = 6, dpi = 300)
  
  i <- i + 1
}

compare_list %>%
  do.call(bind_rows, .) %>%
  filter(!is.na(scenario_nm)) %>%
  assign('compare_diagnostics', . , envir = .GlobalEnv) %>% 
  write.csv( file.path(save.compare,paste('Comparison', species,'.csv',sep = "_")))


compare_diagnostics %>%
  filter(scenario_nm == 'Current',
         stage_nm == 'eggs') %>%
  summarize(old_value = sum(Old_Value, na.rm = T),
            new_value = sum(New_Value, na.rm = T)) %>%
  mutate(prcnt_diff = old_value/new_value)

