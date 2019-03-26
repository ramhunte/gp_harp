# Script to compare two habitat scenarios

library(dplyr)
library(tidyr)

# show dev version of outputs
shell(paste0("git show dev:", outputs_hab, "/outputs_long/habmodel_outputs.csv>", outputs_hab, "/outputs_long/habmodel_outputs_dev.csv"))

save.compare <- file.path('outputs', fishtype,'hab.scenarios', 'diagnostics')
if (dir.exists(save.compare) == F) {dir.create(save.compare,recursive = T)}

# read in dev version
dev_file <- paste0(outputs_hab, "/outputs_long/habmodel_outputs_dev.csv") %>%
  read.csv(.) %>%
  select(-X) %>%
  gather(life_stage_2, dev_value, capacity:survival)
# read in feature version
feature_file <- paste0(outputs_hab, "/outputs_long/habmodel_outputs.csv") %>%
  read.csv(.) %>%
  select(-X) %>%
  gather(life_stage_2, feature_value, capacity:survival)


write.csv(full_join(dev_file, feature_file) %>%
            unite("life_stage", life.stage, life_stage_2, sep = "_") %>%
            mutate(diff = dev_value - feature_value,
                   perc_diff = diff / dev_value), 
          file.path(save.compare, paste0(fishtype, "_hab_scenario_compare.csv")))


write.csv(read.csv(file.path(save.compare, paste0(fishtype, "_hab_scenario_compare.csv"))) %>%
            group_by(hab.scenario, life_stage) %>%
            summarize(min_perc_diff = min(perc_diff, na.rm = T),
                      max_perc_diff = max(perc_diff, na.rm = T),
                      mean_perc_diff = mean(perc_diff, na.rm = T)), 
          file.path(save.compare, paste0(fishtype, "_hab_scenario_compare_summary.csv")))

unlink(paste0(outputs_hab, "/outputs_long/habmodel_outputs_dev.csv"))

