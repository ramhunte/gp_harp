
# Create scenarios with restoration only in the mainstem

# Swap csv values for current in all basins except ms units

library(tidyverse)


# Make fishtype a vector of all fishtypes
# Then we can loop through all types
fishtype <- c(
  'coho',
  'spring chinook', 
  'fall chinook'
)


# Set the date of the files to create the mainstem scenarios from
in_date <- '20181115'

# Loop through the fishtypes
for (f in fishtype) {
  
  # Create the directory path to the input csvs (to make the ms scenarios from)
  in_dir <- file.path('Outputs', in_date, 'hab.scenarios', f)
  
  # Create the path to where to save the files to (today's outputs)
  out_dir <- paste0("Outputs/", format(Sys.Date(), "%Y%m%d"), "/hab.scenarios/", f)
  
  if (dir.exists(out_dir) == F) {dir.create(out_dir, recursive = TRUE)}
  
  # Store the current scenario for all non-mainstem basins
  current <- list.files(in_dir, pattern = 'Current', full.names = T) %>%
    read.csv %>%
    select(stage_nm:X51)
  
  scenarios <- list.files(in_dir, full.names = T)
  
  lapply(scenarios, function(s) {
  
    # Attach the mainstem basins from each scenario to the current csv
    # Save that csv into the outputs folder with the name *_FP_only.csv
    current %>%
      cbind(s %>%
              read.csv() %>%
              select(X52:X63)
      ) %>%
      write.csv(., file.path(out_dir, 
                             paste0(
                               basename(s) %>% sub('\\.csv','', .),
                               '_FP_only.csv'
                                   )
                             )
               )
  })
}

