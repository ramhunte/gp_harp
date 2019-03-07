# Code to recalculate the pass percentage for all reaches, after removing a pair of error culverts
# Error culverts are on Hanaford Cr

library(tidyverse)
library(sf)

# Only need the attribute table from the new culverts layer
# This reaches into the Inputs.gdb, but a csv export of the culverts would work as well
culvs <- st_read("Inputs/shapefiles/dfw_obstructs_within50feet_NAD83.shp")%>%
  as.data.frame()%>%
  select(SiteRec_Id,FishPass)%>%
  mutate(FishPass = ifelse(FishPass == 'Unknown',50,as.character(FishPass))%>%as.numeric())

rm_clv <- flowline%>%
  select(noaaid,reach,culv_list)%>%
  filter(culv_list != "")%>%
  mutate(SiteRec_Id = str_split(culv_list,','))%>%
  select(-culv_list)%>%
  unnest(SiteRec_Id)%>%
  filter(SiteRec_Id != 'None')%>%
  mutate(SiteRec_Id = as.numeric(SiteRec_Id))%>%
  left_join(culvs , by = 'SiteRec_Id')%>%
  mutate(FishPass = ifelse(SiteRec_Id %in% c(5160,5162),1,FishPass/100))%>% # Change FishPass for two culverts to 100%
  group_by(noaaid)%>%
  summarize(pass_tot = prod(FishPass))%>%
  mutate(pass_tot = ifelse(is.na(pass_tot),1,pass_tot)) # Change NAs to 100% passage