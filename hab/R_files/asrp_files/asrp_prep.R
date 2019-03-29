# Prep work: define raw asrp temperature ----  
asrp_ss_raw <- asrp_ss_raw %>%
  mutate(tm_2019 = curr_temp,
         tm_2019_cc_only = curr_temp,
         asrp_temp_w_growth = UQ(as.name(paste0("tm_", x))),
         asrp_temp_cc_only = UQ(as.name(paste0("tm_", x, "_cc_only"))))

asrp_lr_raw <- asrp_lr_raw %>%
  mutate(tm_2019 = curr_temp,
         tm_2019_cc_only = curr_temp,
         asrp_temp_w_growth = UQ(as.name(paste0("tm_", x))),
         asrp_temp_cc_only = UQ(as.name(paste0("tm_", x, "_cc_only"))))

asrp_bw_raw  <- asrp_bw_raw %>%
  mutate(tm_2019 = curr_temp,
         tm_2019_cc_only = curr_temp,
         asrp_temp_w_growth = UQ(as.name(paste0("tm_", x))),
         asrp_temp_cc_only = UQ(as.name(paste0("tm_", x, "_cc_only"))))

asrp_fp_raw  <- asrp_fp_raw %>%
  mutate(tm_2019 = curr_temp,
         tm_2019_cc_only = curr_temp,
         asrp_temp_w_growth = UQ(as.name(paste0("tm_", x))),
         asrp_temp_cc_only = UQ(as.name(paste0("tm_", x, "_cc_only"))))

# Prep work: define intensity scalars ----
if (x == 2040) {
  temp_intensity_scalar_f = 1
  temp_intensity_scalar_nf = 1
  wood_intensity_scalar_f = .6
  wood_intensity_scalar_nf = .6
  fp_intensity_scalar_f = .3
  fp_intensity_scalar_nf = .5
  beaver_intensity_scalar_f = .3 
  beaver_intensity_scalar_nf = .3
  
} else if (x == 2080) {
  temp_intensity_scalar_f = 1
  temp_intensity_scalar_nf = 1
  wood_intensity_scalar_f = .6
  wood_intensity_scalar_nf = .6
  fp_intensity_scalar_f = .3
  fp_intensity_scalar_nf = .5
  beaver_intensity_scalar_f = .3 
  beaver_intensity_scalar_nf = .3
} else {
  temp_intensity_scalar_f = 0
  temp_intensity_scalar_nf = 0
  wood_intensity_scalar_f = 0
  wood_intensity_scalar_nf = 0
  fp_intensity_scalar_f = 0
  fp_intensity_scalar_nf = 0
  beaver_intensity_scalar_f = 0
  beaver_intensity_scalar_nf = 0
}

