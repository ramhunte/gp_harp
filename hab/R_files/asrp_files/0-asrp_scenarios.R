# Run asrp scenarios ----
# This script runs all asrp related scripts to create asrp scenario outputs 
# 7 scenarios are created: Current_asrp, and scenarios 1-3 for years 2040 and 2080
# The structure of the asrp scenarios code is now much closer to the structure of the diagnostic scenario s code and eventually I hope that we will 
# combine these two sets of code

rm(ss_2)
rm(bw2)
rm(all_temps)
rm(edt_lyr)
rm(fp2)
rm(lr_2)
rm(hist_sc)
rm(flowline_noculv)
rm(egg_cap_weight)

source("hab/R_files/asrp_files/1-asrp_prep.R", local = TRUE)

source("hab/R_files/asrp_files/2-asrp_reach_level_data.R", local = TRUE)

source("hab/R_files/asrp_files/3-asrp_culvs.R", local = TRUE)

source("hab/R_files/asrp_files/4-asrp_ss.R", local = TRUE)

source("hab/R_files/asrp_files/5-asrp_lr.R", local = TRUE)

source("hab/R_files/asrp_files/6-asrp_fp.R", local = TRUE)

source("hab/R_files/asrp_files/7-asrp_capacity_and_productivity.R", local = TRUE)

source("hab/R_files/asrp_files/9-asrp_spawn.R", local = TRUE)

source("hab/R_files/asrp_files/10-asrp_egg_to_fry.R", local = TRUE)

source("hab/R_files/asrp_files/11-asrp_prespawn.R", local = TRUE)

source("hab/R_files/asrp_files/12-data_organization.R", local = TRUE)

# long form outputs used in comparison with dev
write.csv(data, file.path(outputs_hab, "outputs_long", "habmodel_outputs.csv"))

