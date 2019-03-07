library(ggplot2)
library(dplyr)
library(tidyr)

# Create plots?
plt <- "Y" #Y/N

# Read in flowline
# Gather into long format, group by DU and species, and summarize
ef <- flowline %>%
  select(noaaid, Subbasin_num, sed_current:ef_surv_hist, species, spawn_dist, both_chk) %>%
  filter(spawn_dist == 'Yes') %>% 
  select(-spawn_dist,-noaaid) %>%
  group_by(Subbasin_num, species) %>%
  summarize_all(mean,na.rm = T) %>%
  select(Subbasin_num, species, ef_surv_current, ef_surv_hist) %>%
  ungroup()

ef_survival <- full_join(ef %>%
                           select(Subbasin_num, species, ef_surv_current) %>%
                           mutate(ef = "curr_eggtofry") %>%
                           unite(species, ef, col = "efspec", sep = "_") %>%
                           group_by(Subbasin_num) %>%
                           spread(efspec, ef_surv_current),
                         ef %>%
                           select(Subbasin_num, species, ef_surv_hist) %>%
                           mutate(ef = "hist_eggtofry") %>%
                           unite(species, ef, col = "efspec", sep = "_") %>%
                           group_by(Subbasin_num) %>%
                           spread(efspec, ef_surv_hist)) %>%
  replace(is.na(.), 0) %>%
  select(Subbasin_num, paste0(fishtype, "_curr_eggtofry"), paste0(fishtype, "_hist_eggtofry")) %>%
  gather(eggtofry_period, eggtofry_surv,c( paste0(fishtype, "_curr_eggtofry"), paste0(fishtype, "_hist_eggtofry"))) %>%
  mutate(eggtofry_period = ifelse(eggtofry_period == paste0(fishtype, "_curr_eggtofry"), 
                                  "Curr",
                                  "Hist")) %>%
  ungroup()

curr.ef <- c("Barriers", "Beaver", "Current", "Shade", "Floodplain", "LR_bank", "LR_length", "Wood", "FP_wood_comb")
hist.ef <- c("Historical", "Fine_sediment")

ef.surv <- bind_rows(lapply(curr.ef, function(x){
  z <- ef_survival %>%
    filter(eggtofry_period == "Curr") %>%
    mutate(hab.scenario = x,
           survival = eggtofry_surv)
}) %>%
  do.call('rbind',.),
lapply(hist.ef, function(x){
  z <- ef_survival %>%
    filter(eggtofry_period == "Hist") %>%
    mutate(hab.scenario = x,
           survival = eggtofry_surv)
}) %>%
  do.call('rbind',.)) %>%
  select(-eggtofry_period, -eggtofry_surv) %>%
  mutate(life.stage = "egg.to.fry") %>%
  filter(Subbasin_num %in% c(1:63))






#Plots
if(plt == "N"){
  
  # Read in clean DU names
  DU_names <- read.delim("Excel Files/DU_names.txt", header = TRUE)%>%
    rename(du = SubBasin)%>%
    mutate(du = ifelse(du %in% 'Grays Harbor','Grays Harbor tribs',as.character(du)))
  
  # Turn into long format for easy plotting
  ef_plt <- ef%>%
    rename(Current = ef_surv_current,
           Historic = ef_surv_hist)%>%
    mutate(species = ifelse(species == 'FChino','Fall Chinook',
                            ifelse(species == 'SChino','Spring Chinook',
                                   ifelse(species == 'Steelh','Steelhead',species))))%>%
    gather(period,s,Current:Historic)%>%
    left_join(DU_names)%>%
    filter(species!='Chum')
  
  # Plot
  ggplot(ef_plt,aes(x=DU_name,y=s,fill=species,alpha=period))+ #streams ordered by highest to lowest e2f survival
    theme_bw()+
    facet_grid(~species)+
    geom_histogram(stat="identity",position=position_dodge(width=0.3))+
    coord_flip()+
    theme(text = element_text(size=14),axis.text.x = element_text(angle=90,vjust = -.1))+
    labs(x="",y="Egg-to-fry survival",alpha="Scenario")+
    scale_x_discrete(name=NULL,
                     limits = rev(levels(reorder(DU_names$DU_name,DU_names$DU_num))))+ #oder by Basin_num/cartodbid
    scale_alpha_manual(values=c(.8,.5),labels=c("Current","Historic"))+
    scale_y_continuous(limits = c(0,1),breaks = c(0,.5,1))+
    guides(fill=F)
  
  # Save the figure
  save.path.fig <- file.path('Outputs', format(Sys.time(), '%Y%m%d'), 'figures')
  if (dir.exists(save.path.fig) == F){dir.create(save.path.fig, recursive = T)}
  
  ggsave(file.path(save.path.fig,'EF_survival.jpg'),height=7,width=10,dpi=250)
  
}



# Testing recreation of ef surv numbers using dplyr rather than pandas
# 
# sz = 8.529595020674
# rdacc_cor = 0.7665126
# s_bf = 0.05
# sed_under_thresh = 27.64866
# 
# #All species
# # Bo.all <- 1.9890
# # B1.all <- -0.1846
# fun.salmon <- function (sed){
#   1/(1+exp(-(1.9890 + -0.1846*sed)))
#   }
# 
# ef_test <- ef%>%
#   mutate(rdacc_cor_m2 = (rdacc*(sz^2))*rdacc_cor,
#          area_m2 = Area_km2*1e6,
#          rd_density = (rdacc_cor_m2/area_m2)*100,
#          slope_bf = slope*BF_width,
#          sed_current = ifelse(slope*BF_width<s_bf, sed_under_thresh, 5.74+(2.05*rd_density)), #use mean of field data for below
#          sed_hist = ifelse(slope*BF_width<s_bf, sed_under_thresh, 5.74),
#          ef_surv_current = fun.salmon(sed_current),
#          ef_surv_hist = fun.salmon(sed_hist))%>%
#   left_join(flowline)%>%
#   select(-noaaid)%>%
#   rename(du = geographic,
#          edt_schnk = SChinook_S,
#          edt_fchnk = FChinook_S,
#          edt_coho = Coho_Spawn,
#          edt_sthd = Steelhead_,
#          edt_chum = Chum_Spawn)%>%
#   gather(species,presence,edt_schnk:edt_chum)%>%
#   filter(presence == 'Yes',
#          !is.na(du),
#          sed_current>=0)%>%
#   select(-presence)%>%
#   group_by(du,species)%>%
#   summarize_all(mean,na.rm=T)


rm(ef)

