source('srt_figures/spawners_edr.R')
source('srt_figures/spawners_subbasin.R')
source('srt_figures/spawners_diagnostic_scenarios.R')

read_filename <- function(fname) {
  read_csv(fname, col_names = TRUE) %>%
    mutate(filename = fname)
}

data_folder <- 'srt_figures/results/'
data_files <- c('spawners_edr.csv', 'spawners_subbasin.csv', 'spawners_diagnostics.csv')

files <- list.files(path = 'srt_figures/results/',
                    pattern = '*.csv',
                    full.names = TRUE)

mylist <- lapply(data_files, function(x) {
  read.csv(paste0(data_folder, x)) %>%
    select(-1)
})
names(mylist)

wb <- loadWorkbook('srt_figures/spawners_template.xlsx')
lapply(1:3, function(i){
  writeData(wb, sheet = i, mylist[[i]], colNames = TRUE, keepNA = FALSE)
})

saveWorkbook(wb, 'srt_figures/spawners_workbook.xlsx', overwrite = TRUE)
