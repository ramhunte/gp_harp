
#Creates maps of the restoration scenarios by EDR

library(rgdal)
library(tidyverse)
library(scales)
library(gridExtra)
require(TeachingDemos)


# Read in data ----

#Link to GDB
fgdb <- "data/LCM_spatial.gdb"

# List all feature classes in a file geodatabase
gdb_list <- ogrListLayers(fgdb)
print(sort(gdb_list))

# Read the feature class
edr <- readOGR(dsn=fgdb,layer="NOAA_EDR_20180823")





# Head map with color bins for change ----

#Fortify (turn into dataframe) and rejoin with attribute table
edr_fort <- fortify(edr,region = 'EcoRegion')


if(pop == "coho"){
  #tr.breaks <- seq(0,5000,2500)
  #tr.breaks <- c(0,1000,5000)
  tr.breaks <- c(0,5000,10000)
  tr.breaks[1] <- 100
}


if(pop == "spring.chinook"){
  tr.breaks <- seq(0,1000,250)
  tr.breaks[1] <- 10
}


if(pop == "fall.chinook"){
  tr.breaks <- seq(0,2000,500)
  tr.breaks[1] <- 10
}


edr_plot <- edr@data%>%
  right_join(.,total.run.edr)%>%
  mutate(total.run.change = ifelse(total.run.change < tr.breaks[1],0,total.run.change),
         tr.change.bin = cut(total.run.change,c(-Inf,tr.breaks,Inf), na.rm=T,dig.lab =10))%>%
         #tr.change.bin = prettyNum(edr_plot$tr.change.bin,big.mark = ','))%>%
  rename(id = EcoRegion)%>%
  right_join(.,edr_fort)%>%
  mutate(scenario = as.factor(scenario))


# Change names of bins to clean display [e.g from (0,500] -> 0-500))
levels(edr_plot$tr.change.bin) <- c("No Change",levels(edr_plot$tr.change.bin)[-1]%>%gsub("]|\\(","",.)%>%sub(",","-",.))


# Create background layer
noplot.edr <- c('Upper Skookumchuck','Chehalis River Tidal')
edr.na <- geom_polygon(data = fortify(edr[edr@data$EcoRegion %in% noplot.edr,]),
                      aes(x=long,y=lat,group=group),fill = "grey80",color = "black",na.rm = T)


#Define function to plot map
plot.map <- function(plot.scenario,fill.param){
  
  df <- edr_plot%>%filter(scenario == plot.scenario)
  plot.col <- plot.params%>%filter(scenario %in% df$scenario)%>%select(color)%>%unlist(use.names=F)
  
  ggplot()+
    theme_void()+
    geom_polygon(data=df%>%filter(scenario == plot.scenario),
                 aes(x=long,y=lat,group=group, alpha = get(fill.param)),
                 fill = plot.col,
                 color="black")+
    scale_alpha_manual(values = seq(0,1,length.out = length(levels(edr_plot$tr.change.bin))),
                       drop = F)+
    labs(alpha = df$ylab.tr,
         title =unique(df$scenario.label))+
    theme(legend.position=c(.2,.2),
          text = element_text( size = 14),
          plot.title = element_text(margin = margin(t = 10, b = -10),hjust = .5))+
    coord_equal()+
    edr.na
    
} #close plot.map() function


# Choose which scenarios to plot

top.4.scenarios<- total.run.edr%>%
  filter(time.period == 'Current')%>%
  group_by(scenario)%>%
  summarize(total.run.change=sum(total.run.change))%>%
  top_n(4)%>%
  select(scenario)%>%
  unlist(use.names = FALSE)

bottom.4.scenarios <- total.run.edr%>%
  filter(time.period == 'Current',
         scenario != 'Current',
         !(scenario %in% top.4.scenarios))%>%
  select(scenario)%>%
  unlist(use.names = FALSE)%>%
  unique()


# fill.params <- c("prcnt.change","total.run.change")

# Create map using plot.map() function
jpeg(file.path(out.path,paste('Top4_scenarios.change',pop,paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")),width=2000,height=2000,res=150)
grid.arrange(plot.map(plot.scenario = top.4.scenarios[1], fill.param = "tr.change.bin"),
             plot.map(plot.scenario = top.4.scenarios[2], fill.param = "tr.change.bin"),
             plot.map(plot.scenario = top.4.scenarios[3], fill.param = "tr.change.bin"),
             plot.map(plot.scenario = top.4.scenarios[4], fill.param = "tr.change.bin"),
             ncol=2)
dev.off()


jpeg(file.path(out.path,paste('Bottom4_scenarios.change',pop,paste0(format(Sys.time(), "%Y%m%d"),'.jpg'),sep = "_")),width=2000,height=2000,res=150)
grid.arrange(plot.map(plot.scenario = bottom.4.scenarios[1], fill.param = "tr.change.bin"),
             plot.map(plot.scenario = bottom.4.scenarios[2], fill.param = "tr.change.bin"),
             plot.map(plot.scenario = bottom.4.scenarios[3], fill.param = "tr.change.bin"),
             plot.map(plot.scenario = bottom.4.scenarios[4], fill.param = "tr.change.bin"),
             ncol=2)
dev.off()





# Map with barplots imposed on top ----

edr.which <- c(1:5, 7:10,12)#edr@plotOrder[c(-10, -3)]#
myscenarios <- c(1,2,4,5,8:11)
col.myscenarios <- c(2,8,3,9,6,7,5,4)

# put coordinates in alphabetical by name:
mycoords <- coordinates(edr)[c(8:10,7,1,6,4,5,2,11),]#excludes 3 and 12; full list is c(8:10,7,1,3,6,4,5,2,12,11)

# Willapa Hills: 10; Black Hills: 1; mainstem lower: 6
mycoords[10,1] <- 483940.5
mycoords[10,2] <- 5145741
mycoords[1,1] <- 482780
mycoords[1,2] <- 5211972
mycoords[6,1] <- 474851.4
mycoords[6,2] <- 5198687


jpeg(paste0(out.path,'/Map_w_barplots_',pop,'_',format(Sys.time(), "%Y%m%d"),".jpg"),
     width = 10, height = 10, units = 'in', res = 300)

sp::plot(edr, border ="gray80")

for(i in 1:length(edr.which)) {
  subplot(#barplot(test, las=1, axes=FALSE, axisnames=FALSE),
    barplot(tapply(total.run.edr$total.run, total.run.edr[,2:1], sum)[edr.which[i], myscenarios] -
              tapply(total.run.edr$total.run, total.run.edr[,2:1], sum)[edr.which[i], 3],
            las=1, axes=FALSE, axisnames=FALSE,# horiz=TRUE,
            space=c(1,1), width=rep(0.8,length(edr.which)),
            col=plot.params$color[col.myscenarios], border=plot.params$color[col.myscenarios]),
    
    x=mycoords[i , 1],
    y=mycoords[i , 2],
    size=c(0.7, 0.7))
}

legend("topright", legend=unique(total.run.edr$scenario)[myscenarios],# remove current,
       fill=plot.params$color[col.myscenarios], 
       border=plot.params$color[col.myscenarios], bty="n", cex=1.2)

dev.off()