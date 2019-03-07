library(sf)
library(tidyverse)
library(ggplot2)
library(scales)

spatial.diag <- file.path('Outputs',format(Sys.Date(), "%Y%m%d"),'diagnostics',fishtype)
if (dir.exists(spatial.diag) == F) {dir.create(spatial.diag,recursive = T)}


filename <- dir(file.path("Outputs",format(Sys.Date(), "%Y%m%d"),'hab.scenarios',fishtype),full.names = T) #Folder where all the hab scenarios are sitting
shp.path <- "Inputs/shapefiles/NOAA_subbasins_w_fp.shp" # Shapefile of the subbasins

hab.ver <- basename(filename[1])%>% # What version of habitat files?
  sub('\\.csv','', .)%>%
  gsub(".*_","",.)

if (fishtype == "coho") {params.to.lifestages <- data.frame(stage_nm = stage_nm,
                                                            lifestage = c('eggs','spawners','eggs','fry', rep('parr',2),rep('smolts',2)),
                                                            type = c('prod','cap','cap','cap', 'cap','prod','cap','prod'))}
if (fishtype == "spring chinook") {params.to.lifestages <- data.frame(stage_nm = stage_nm,
                                                                      lifestage = c('eggs', 'spawners', 'eggs', 'fry', rep('parr', 2), rep('smolts', 2), 'prespawn'),
                                                                      type = c('prod', 'cap', 'cap', 'cap', 'cap', 'prod', 'cap', 'prod', 'prod'))}


all.scenarios<- filename%>%
  map_dfr(read.csv,h=T,.id = 'name')%>%
  mutate(scenario = filename[as.numeric(name)]%>%basename(.)%>%gsub('_','\\.',.)%>%sub('\\.20.*','',.))%>%
  left_join(params.to.lifestages)%>%
  select(-X,-name,-stage_nm)

colnames(all.scenarios) <- c(as.character(subbasin_names$Subbasin),'scenario','lifestage','type')

all.scenarios <- all.scenarios%>%
  gather(Subbasin,value,as.character(subbasin_names$Subbasin))%>%
  left_join(subbasin_names)%>%
  spread(type,value)%>%
  mutate(prod_diff_from_Current = prod - prod[scenario == 'Current'],
         cap_diff_from_Current = cap - cap[scenario == 'Current'],
         prod_prcnt_diff_from_Current = prod_diff_from_Current/prod[scenario == 'Current'],
         cap_prcnt_diff_from_Current = cap_diff_from_Current/cap[scenario == 'Current'])


write.csv(all.scenarios,file.path(spatial.diag,paste0('Scenario_Exploration_',format(Sys.time(), "%Y%m%d"),'.csv')))

# Places with productivies lower than current
# all.scenarios%>%
#   filter(prod_diff_from_Current < 0)

# Places with capacities lower than current
# all.scenarios%>%
#   filter(cap_diff_from_Current < 0)


ch <- st_read(shp.path)%>%
  left_join(all.scenarios)%>%
  #select(Subbasin, scenario,lifestage, prod_prcnt_diff_from_Current,cap_prcnt_diff_from_Current)%>%
  #gather(param_type,value, prod_prcnt_diff_from_Current:cap_prcnt_diff_from_Current)%>%
  select(Subbasin, scenario,lifestage, prod_diff_from_Current,cap_diff_from_Current)%>%
  gather(param_type,value, prod_diff_from_Current:cap_diff_from_Current)%>%
  mutate(param_type = ifelse(substr(param_type,1,1) == 'p' ,'productivity','capacity'))


for(l in as.character(unique(ch$lifestage))){
  for (p in unique(ch$param_type)){
    
    ggplot(ch%>%filter(lifestage == l, param_type == p))+
      theme_void()+
      geom_sf(aes(fill=value))+
      coord_sf(crs = st_crs(ch),datum = NA)+
      scale_fill_gradient2()+
      facet_wrap(~scenario)+
      labs(fill = paste(p,'\n(Difference from Current)'),
           title = paste(fishtype,l,p,sep=' '),
           caption = paste0('Habitat scenario version ',hab.ver))+
      theme(plot.title = element_text(size=25,vjust = 11,hjust = 0.5),
            strip.text.x = element_text(size = 15),
            legend.title=element_text(size=15),
            plot.caption =element_text(size=15))
    
    ggsave(file.path(spatial.diag,paste(fishtype,l,p,paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")), width = 20, height = 15, dpi = 300)
  }
}



# Create bar plots of productivity and capacity difference from current ----

plot.params <- read.csv("Excel Files/scenarios.csv")%>%
  select(scenario,scenario.label,color)%>%
  mutate_if(is.factor,as.character)

all.scenarios.edr <- all.scenarios%>%
  group_by(scenario,lifestage,EcoRegion)%>%
  summarize(prod_diff_from_Current = mean(prod_diff_from_Current,na.rm=T),
            cap_diff_from_Current = sum(cap_diff_from_Current, na.rm=T))%>%
  ungroup()

all.scenarios.edr$scenario <- factor(all.scenarios.edr$scenario, levels= plot.params$scenario)



for (l in as.character(unique(all.scenarios.edr$lifestage))){
  
  ggplot(all.scenarios.edr%>%
           filter(lifestage== l),aes(scenario,prod_diff_from_Current,fill=scenario))+
    theme_bw()+
    geom_bar(stat = 'identity',color='black',na.rm = T)+
    facet_wrap(~EcoRegion)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = plot.params$color, guide=F)+
    scale_y_continuous(label=comma)+
    labs(x=NULL,
         y="Change from Current Scenario",
         title = paste0('Productivty change for ',l),
         caption = paste0('Habitat scenario version ',hab.ver))
  
  ggsave(file.path(spatial.diag,
                   paste('EDR_Productivity_Diff',fishtype,l,
                         paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")), 
         width = 10, height = 8, dpi = 300)#pdfs 10x10
  
  
  ggplot(all.scenarios.edr%>%
           filter(lifestage== l),aes(scenario,cap_diff_from_Current,fill=scenario))+
    theme_bw()+
    geom_bar(stat = 'identity',color='black',na.rm = T)+
    facet_wrap(~EcoRegion)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = plot.params$color, guide=F)+
    scale_y_continuous(label=comma)+
    labs(x=NULL,
         y="Change from Current Scenario",
         title = paste0('Capacity change for ',l),
         caption = paste0('Habitat scenario version ',hab.ver))
  
  ggsave(file.path(spatial.diag,
                   paste('EDR_Capacity_Diff',fishtype,l,
                         paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")), 
         width = 10, height = 8, dpi = 300)#pdfs 10x10
}

