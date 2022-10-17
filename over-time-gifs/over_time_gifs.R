library(lubridate)
library(tidyverse)
library(cowplot)
require(rgdal)
require(magick)
require(gridExtra)
require(grid)
require(ggpubr)
require(extrafont)
require(ggformula)
require(zoo)
require(gifski)
require(mltools)
require(rgeos)
require(sp)
require(sf)
require(ggthemes)
require(stringr)
theme_set(theme_tufte())

getMonth = function (week)
{
  Months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  return (Months [as.integer(1 + ((week-1)/4))])
}

load("EBD/ebd_IN_relAug-2022.RData")
load("maps.RData")
load("clips.RData")

data = data %>%
  mutate(YEARs = YEAR,
         LAST.EDITED.DATE= as.Date(LAST.EDITED.DATE), 
         YEARe = year(LAST.EDITED.DATE))

sub = data %>% distinct(SAMPLING.EVENT.IDENTIFIER,YEARe) 
a = str_split_fixed(sub$SAMPLING.EVENT.IDENTIFIER,"S",2)
sub$subID = a[,2]
sub$subID = as.numeric(sub$subID)
sub = sub %>% arrange(desc(subID))

base = sub$YEARe[1]-1
flag0 = 0
for (i in 1:length(sub$subID))
{
  if (sub$YEARe[i] == base & flag0 == 0)
  {
    flag = i
    flag0 = 1
  }
  if (sub$YEARe[i] < base & flag0 != 0)
  {
    sub$YEAR[flag:(i-1)] = base
    flag = i
    base = sub$YEARe[i]
    print(flag)
    print(base)
  }
  if (i == length(sub$subID))
  {
    print(flag)
    print(i)
    sub$YEAR[flag:i] = base
  }
}
sub = sub %>% select(SAMPLING.EVENT.IDENTIFIER,YEAR)
data = left_join(data,sub)

sub = data %>% distinct(SAMPLING.EVENT.IDENTIFIER,YEARs) 
a = str_split_fixed(sub$SAMPLING.EVENT.IDENTIFIER,"S",2)
sub$subID = a[,2]
sub$subID = as.numeric(sub$subID)
sub = sub %>% arrange(subID)

sub$YEARf = 0
flag = sub$subID[1]

for (i in 2012:2022)
{

  x0 = sub %>% filter(YEARs == i) %>% arrange(subID)
  x1 = x0$subID[5]
  sub$YEARf[sub$subID >= flag & sub$subID < x1] = i-1
  flag = x1
  print(flag)

  if (i == 2022)
  {
    sub$YEARf[sub$subID >= flag] = i
  }
}
sub = sub %>% select(SAMPLING.EVENT.IDENTIFIER,YEARf)
data = left_join(data,sub)


statemap = gBuffer(statemap, byid=TRUE, width=0)
filterstate = fortify(statemap)

temp0 = data %>% group_by(GROUP.ID) %>% slice(1)
temp = temp0

rownames(temp) = temp$GROUP.ID
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg1)
temp = data.frame(temp)
temp$GROUP.ID = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg1"

temp = temp0

rownames(temp) = temp$GROUP.ID
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g2clip)
temp = data.frame(temp)
temp$GROUP.ID = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g2clip"

temp = temp0

rownames(temp) = temp$GROUP.ID
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g3clip)
temp = data.frame(temp)
temp$GROUP.ID = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g3clip"

## point locations over time

res = 300
fps = 1.5
windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
nums = 2011:2022

img = image_graph(width = 966, height = 1074, res = res, bg = "black")

for (i in nums)
{
  locs = data %>% filter(YEARf<=i) %>% distinct(LONGITUDE,LATITUDE)

  x = ggplot() +
    theme(text=element_text(family="Gill Sans"))+
    theme(panel.background = element_rect(fill = "black",
                                          colour = "black"),
          plot.margin = margin(1, 1, 1, 1, "mm"),
          plot.background = element_rect(
            fill = "black",
            colour = "black",
            size = 0.3),
          axis.line=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  qx = ggdraw(x) +  
    draw_label(i, 0.5, 0.59, size = 6, fontfamily="Gill Sans", fontface = 'bold', colour = "white")
  
  vs = viewport(width = 0.1, height = 0.03, x = 0.93,
                y = 0.98, just = c("center","top"))

  ggp = ggplot() +
    geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = NA, fill = "black")+  
    geom_point(data = locs, aes(x=LONGITUDE,y=LATITUDE), colour = "#fcfa53", 
               size = 0.1, alpha = 0.5, stroke = 0) +
    scale_x_continuous(limits = c(65,97.5)) +
    scale_y_continuous(limits = c(4,37)) +
    theme_bw()+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          #panel.border = element_blank(),
          plot.background = element_rect(fill = "black",colour = NA),
          panel.background = element_rect(fill = "black", colour = NA),
          plot.title = element_text(hjust = 0.9))+
    coord_map()
  
  full = function() {
    print(ggp)
    print(qx, vp = vs)
  }
  full()
}

dev.off(which = 2)
image_write_gif(img, "over-time-gifs/birding_point_growth.gif", delay = 1/fps)


## species maps over time

res = 300
fps = 1.5
windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
nums = 2011:2022
proj4string(statemap) = "+proj=longlat +datum=WGS84"
filterstate = fortify(statemap)

species = "Black-headed Ibis"

img = image_graph(width = 966, height = 1074, res = res, bg = "white")

for (i in nums)
{
  temp = data %>% filter(YEARf<=i, ALL.SPECIES.REPORTED == 1) %>%
    group_by(g2clip) %>%
    mutate(lists = n_distinct(GROUP.ID)) %>% ungroup() %>%
    filter(COMMON.NAME == species, lists >= 10) %>%
    group_by(g2clip) %>%
    summarize(freq = n_distinct(GROUP.ID)/max(lists))
  
  fortified = fortify(g2clip, region = c("id"))
  fortified$id = as.factor(fortified$id)
  temp$g2clip = as.factor(temp$g2clip)
  plotdf = na.omit(left_join(fortified,temp, by = c('id' = "g2clip"))) # SPDF to plot
  
  plotdf$freq1 = mltools::bin_data(plotdf$freq, bins=4, binType = "quantile")
  sm = plotdf %>%
    group_by(freq1) %>% summarize(min = round(min(freq),2),max = round(max(freq),2))
  l = length(sm$freq1)
  vals = c("#99CCFF","#6699CC","#336699","#003399")
  
  x = ggplot() +
    theme(text=element_text(family="Gill Sans"))+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.margin = margin(1, 1, 1, 1, "mm"),
          plot.background = element_rect(
            fill = "white",
            colour = "white",
            size = 0.3),
          axis.line=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  qx = ggdraw(x) +  
    draw_label(i, 0.5, 0.59, size = 6, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
  
  vs = viewport(width = 0.1, height = 0.03, x = 0.93,
                y = 0.98, just = c("center","top"))
  
  ggp = ggplot() +
    geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq1)) +
    geom_path(data = filterstate, aes(x = long, y = lat, group = group), col = 'black', size = 0.5) +
    scale_x_continuous(limits = c(65,97.5)) +
    scale_y_continuous(limits = c(4,38)) +
    theme_bw()+
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                                   breaks = sm$freq1, labels = sm$freq1)} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                                   breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                                                 paste(">",sm$min[3])))} +
    {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                                                              paste(sm$min[3]," - ",sm$max[3]),paste(">",sm$min[4])))} +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank())+
    theme(legend.position = "none")+
    coord_map()
  
  full = function() {
    print(ggp)
    print(qx, vp = vs)
  }
  full()
}

dev.off(which = 2)
image_write_gif(img, "over-time-gifs/BhIb_growth.gif", delay = 1/fps)


species = "Singing Bushlark"

img = image_graph(width = 966, height = 1074, res = res, bg = "white")

for (i in nums)
{
  temp = data %>% filter(YEARf<=i, ALL.SPECIES.REPORTED == 1) %>%
    group_by(g2clip) %>%
    mutate(lists = n_distinct(GROUP.ID)) %>% ungroup() %>%
    filter(COMMON.NAME == species, lists >= 10) %>%
    group_by(g2clip) %>%
    summarize(freq = n_distinct(GROUP.ID)/max(lists))
  
  fortified = fortify(g2clip, region = c("id"))
  fortified$id = as.factor(fortified$id)
  temp$g2clip = as.factor(temp$g2clip)
  plotdf = na.omit(left_join(fortified,temp, by = c('id' = "g2clip"))) # SPDF to plot
  
  plotdf$freq1 = mltools::bin_data(plotdf$freq, bins=4, binType = "quantile")
  sm = plotdf %>%
    group_by(freq1) %>% summarize(min = round(min(freq),2),max = round(max(freq),2))
  l = length(sm$freq1)
  vals = c("#99CCFF","#6699CC","#336699","#003399")
  
  x = ggplot() +
    theme(text=element_text(family="Gill Sans"))+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.margin = margin(1, 1, 1, 1, "mm"),
          plot.background = element_rect(
            fill = "white",
            colour = "white",
            size = 0.3),
          axis.line=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  qx = ggdraw(x) +  
    draw_label(i, 0.5, 0.59, size = 6, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
  
  vs = viewport(width = 0.1, height = 0.03, x = 0.93,
                y = 0.98, just = c("center","top"))
  
  ggp = ggplot() +
    geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq1)) +
    geom_path(data = filterstate, aes(x = long, y = lat, group = group), col = 'black', size = 0.5) +
    scale_x_continuous(limits = c(65,97.5)) +
    scale_y_continuous(limits = c(4,38)) +
    theme_bw()+
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = sm$freq1)} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                               paste(">",sm$min[3])))} +
    {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                                            paste(sm$min[3]," - ",sm$max[3]),paste(">",sm$min[4])))} +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank())+
    theme(legend.position = "none")+
    coord_map()
  
  full = function() {
    print(ggp)
    print(qx, vp = vs)
  }
  full()
}

dev.off(which = 2)
image_write_gif(img, "over-time-gifs/SiBu_growth.gif", delay = 1/fps)


species = "Yellow-browed Warbler"

img = image_graph(width = 966, height = 1074, res = res, bg = "white")

for (i in nums)
{
  temp = data %>% filter(YEARf<=i, ALL.SPECIES.REPORTED == 1) %>%
    group_by(g2clip) %>%
    mutate(lists = n_distinct(GROUP.ID)) %>% ungroup() %>%
    filter(COMMON.NAME == species, lists >= 10) %>%
    group_by(g2clip) %>%
    summarize(freq = n_distinct(GROUP.ID)/max(lists))
  
  fortified = fortify(g2clip, region = c("id"))
  fortified$id = as.factor(fortified$id)
  temp$g2clip = as.factor(temp$g2clip)
  plotdf = na.omit(left_join(fortified,temp, by = c('id' = "g2clip"))) # SPDF to plot
  
  plotdf$freq1 = mltools::bin_data(plotdf$freq, bins=4, binType = "quantile")
  sm = plotdf %>%
    group_by(freq1) %>% summarize(min = round(min(freq),2),max = round(max(freq),2))
  l = length(sm$freq1)
  vals = c("#99CCFF","#6699CC","#336699","#003399")
  
  x = ggplot() +
    theme(text=element_text(family="Gill Sans"))+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.margin = margin(1, 1, 1, 1, "mm"),
          plot.background = element_rect(
            fill = "white",
            colour = "white",
            size = 0.3),
          axis.line=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  qx = ggdraw(x) +  
    draw_label(i, 0.5, 0.59, size = 6, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
  
  vs = viewport(width = 0.1, height = 0.03, x = 0.93,
                y = 0.98, just = c("center","top"))
  
  ggp = ggplot() +
    geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq1)) +
    geom_path(data = filterstate, aes(x = long, y = lat, group = group), col = 'black', size = 0.5) +
    scale_x_continuous(limits = c(65,97.5)) +
    scale_y_continuous(limits = c(4,38)) +
    theme_bw()+
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = sm$freq1)} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                               paste(">",sm$min[3])))} +
    {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                                            paste(sm$min[3]," - ",sm$max[3]),paste(">",sm$min[4])))} +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank())+
    theme(legend.position = "none")+
    coord_map()
  
  full = function() {
    print(ggp)
    print(qx, vp = vs)
  }
  full()
}

dev.off(which = 2)
image_write_gif(img, "over-time-gifs/YbWa_growth.gif", delay = 1/fps)



species = "Black-breasted Weaver"

img = image_graph(width = 966, height = 1074, res = res, bg = "white")

for (i in nums)
{
  temp = data %>% filter(YEARf<=i, ALL.SPECIES.REPORTED == 1) %>%
    group_by(g2clip) %>%
    mutate(lists = n_distinct(GROUP.ID)) %>% ungroup() %>%
    filter(COMMON.NAME == species, lists >= 10) %>%
    group_by(g2clip) %>%
    summarize(freq = n_distinct(GROUP.ID)/max(lists))
  
  fortified = fortify(g2clip, region = c("id"))
  fortified$id = as.factor(fortified$id)
  temp$g2clip = as.factor(temp$g2clip)
  plotdf = na.omit(left_join(fortified,temp, by = c('id' = "g2clip"))) # SPDF to plot
  
  plotdf$freq1 = mltools::bin_data(plotdf$freq, bins=4, binType = "quantile")
  sm = plotdf %>%
    group_by(freq1) %>% summarize(min = round(min(freq),2),max = round(max(freq),2))
  l = length(sm$freq1)
  vals = c("#99CCFF","#6699CC","#336699","#003399")
  
  x = ggplot() +
    theme(text=element_text(family="Gill Sans"))+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.margin = margin(1, 1, 1, 1, "mm"),
          plot.background = element_rect(
            fill = "white",
            colour = "white",
            size = 0.3),
          axis.line=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  qx = ggdraw(x) +  
    draw_label(i, 0.5, 0.59, size = 6, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
  
  vs = viewport(width = 0.1, height = 0.03, x = 0.93,
                y = 0.98, just = c("center","top"))
  
  ggp = ggplot() +
    geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq1)) +
    geom_path(data = filterstate, aes(x = long, y = lat, group = group), col = 'black', size = 0.5) +
    scale_x_continuous(limits = c(65,97.5)) +
    scale_y_continuous(limits = c(4,38)) +
    theme_bw()+
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = sm$freq1)} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                               paste(">",sm$min[3])))} +
    {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                                                            paste(sm$min[3]," - ",sm$max[3]),paste(">",sm$min[4])))} +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank())+
    theme(legend.position = "none")+
    coord_map()
  
  full = function() {
    print(ggp)
    print(qx, vp = vs)
  }
  full()
}

dev.off(which = 2)
image_write_gif(img, "over-time-gifs/BbWe_growth.gif", delay = 1/fps)




## seasonality

res = 300
fps = 1.5
windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
nums = 2011:2022
dataf = data %>% filter(COUNTY %in% c("Bengaluru Urban","Bengaluru Rural"))
dataf$WEEK = 4
dataf$WEEK[dataf$DAY.M <= 23] = 3
dataf$WEEK[dataf$DAY.M <= 15] = 2
dataf$WEEK[dataf$DAY.M <= 8] = 1
dataf$WEEK = dataf$WEEK + 4*(dataf$MONTH-1)
dataf = dataf %>% filter(COMMON.NAME != "Blyth's Reed Warbler" | !MONTH %in% c(7,8))
dataf = dataf %>% filter(COMMON.NAME != "Gray Wagtail" | !MONTH %in% c(6,7))


species = c("Common Cuckoo","Blyth's Reed Warbler","Gray Wagtail","Northern Shoveler","Green Sandpiper")
s1 = rep(species, each = 48)
s2 = rep(1:48, length(species))
skel = data.frame(COMMON.NAME = s1, WEEK = s2)

img = image_graph(width = 1500, height = 1200, res = res, bg = "white")

for (i in nums)
{
  plotdata = dataf %>% filter(YEARf<=i, ALL.SPECIES.REPORTED == 1) %>%
    group_by(MONTH,WEEK) %>%
    mutate(lists = n_distinct(GROUP.ID)) %>% ungroup() %>%
    filter(COMMON.NAME %in% species) %>%
    group_by(COMMON.NAME,WEEK) %>%
    summarize(freq = n_distinct(GROUP.ID)/max(lists))
  
  plotdata = left_join(skel,plotdata)
  plotdata$freq[is.na(plotdata$freq)] = 0
  plotdata$MONTH = getMonth(plotdata$WEEK)
  
  #eBird uses the same breaks
  my_breaks = c(0.0, 0.0000000001, 0.0025, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.6, 1)
  my_labels = c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  plotdata$ABUNDANCE = mapply(cut, plotdata$freq, MoreArgs = list (breaks = my_breaks,
                                            labels = my_labels,
                                            include.lowest = TRUE))
  plotdata$ABUNDANCE = as.character(plotdata$ABUNDANCE)
  plotdata$ABUNDANCE = as.numeric(plotdata$ABUNDANCE)
  
  x = ggplot() +
    theme(text=element_text(family="Gill Sans"))+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.margin = margin(1, 1, 1, 1, "mm"),
          plot.background = element_rect(
            fill = "white",
            colour = "white",
            size = 0.3),
          axis.line=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  qx = ggdraw(x) +  
    draw_label(i, 0.5, 0.59, size = 6, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
  
  vs = viewport(width = 0.1, height = 0.03, x = 0.03,
                y = 0.98, just = c("center","top"))
  
  new = plotdata
  new1 = new
  new1$ABUNDANCE = -1*new1$ABUNDANCE
  toplot = rbind(new,new1)
  toplot = toplot[toplot$COMMON.NAME %in% species,]
  toplot$MONTH = factor(toplot$MONTH, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  l = length(species)
  if (l %% 2 == 0)
  {
    alt = data.frame(COMMON.NAME = species,col = rep(c("grey","white"),l/2))
  }
  if (l %% 2 == 1)
  {
    alt = data.frame(COMMON.NAME = species,col = c(rep(c("grey","white"),(l-1)/2),"grey"))
  }
  
  toplot = left_join(toplot,alt)
  toplot$COMMON.NAME = factor(toplot$COMMON.NAME, levels = species)
  
  
  ggp = ggplot(toplot, aes(x=WEEK, y=ABUNDANCE)) + 
    geom_rect(aes(fill = col),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.1) +
    facet_grid(COMMON.NAME~MONTH, 
               scale = "free_x", switch = "y", 
               labeller = label_wrap_gen(width = 2, multi_line = TRUE)) +
    geom_bar(stat = "identity", position = "identity", fill = "dark green") +
    xlab("MONTH") +
    ylab("COMMON.NAME")
  
  ggp1 = ggp +
    ## use element_text to retain axis labels/titles/ticks with appropriate sizes specified
    theme(axis.title.x = element_blank(),
          axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank(),
          strip.background.x = element_rect(color = "black", size = 1),
          #strip.text.x = element_blank(),
          #strip.text.y = element_blank()) +
          strip.text.x = element_text(size = 6, face = "bold"),
          strip.text.y = element_text(size = 6, face = "bold", angle = 180)) +
    theme(legend.position="none") +
    scale_y_continuous(limits = c(-10,10)) +
    scale_fill_manual(values = c("grey","white"), breaks = c("grey","white"))
  
  full = function() {
    print(ggp1)
    print(qx, vp = vs)
  }
  full()
}

dev.off(which = 2)
image_write_gif(img, "over-time-gifs/seasonality_growth_Bengaluru.gif", delay = 1/fps)


#ignore
start = Sys.time()
sub = data %>% distinct(SAMPLING.EVENT.IDENTIFIER,YEARs) 
all.list = sub[with(sub, match(unique(SAMPLING.EVENT.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER)), ]
all.list$subID = as.numeric(do.call("rbind", strsplit(all.list$SAMPLING.EVENT.IDENTIFIER, "S", fixed = TRUE))[,2])

year.first.id = with(all.list, tapply(subID, YEARs, function(x) sort(x)[5])) ## takes second-lowest list.id
year.first.id = data.frame(YEARs=names(year.first.id), first.id=year.first.id, stringsAsFactors=FALSE)

## find year of uploading
all.list$upload.year = sapply(all.list$subID, function(x) rev(year.first.id$YEARs[x >= year.first.id$first.id])[1])
sub$YEARf = all.list$upload.year[match(sub$SAMPLING.EVENT.IDENTIFIER, all.list$SAMPLING.EVENT.IDENTIFIER)]
end = Sys.time()
print(end-start)
#ignore
