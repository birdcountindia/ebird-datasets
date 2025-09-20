require(lubridate)
require(tidyverse)
require(magick)
require(scales) # for comma format of numbers
require(grid)
require(glue)
require(sf)

plot_lims <- function(){
  
  xextent1 <- cur_bbox[3] - cur_bbox[1]
  yextent1 <- cur_bbox[4] - cur_bbox[2]
  
  if (xextent1 > yextent1) {
    ext_diff <- xextent1 - yextent1
    
    xlimit <- c(cur_bbox[1], cur_bbox[3])
    ylimit <- c((cur_bbox[2] - ext_diff/2), (cur_bbox[4] + ext_diff/2))
  } 
  if (yextent1 > xextent1) {
    ext_diff <- yextent1 - xextent1
    
    xlimit <- c((cur_bbox[1] - ext_diff/2), (cur_bbox[3] + ext_diff/2))
    ylimit <- c(cur_bbox[2], cur_bbox[4])
  }
  
  # true extent of map area given these limits
  xextent2 <- xlimit[2] - xlimit[1]
  yextent2 <- ylimit[2] - ylimit[1]
  
  # output
  assign("xextent1", xextent1, envir = .GlobalEnv)
  assign("yextent1", yextent1, envir = .GlobalEnv)
  assign("xextent2", xextent2, envir = .GlobalEnv)
  assign("yextent2", yextent2, envir = .GlobalEnv)
  assign("xlimit", xlimit, envir = .GlobalEnv)
  assign("ylimit", ylimit, envir = .GlobalEnv)
  
}

path = "EBD/ebd_IN_relMay-2025.RData"
load(path)

data_loc <- data %>% distinct(LONGITUDE, LATITUDE)
cur_bbox <- data_loc %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% st_bbox()
plot_lims()

map_cov_plain <- ggplot() +
  #geom_sf(data = india_sf, colour = NA, fill = "black") +
  geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE), 
             #colour = "#fcfa53", 
             #colour = "#FFA726",
             colour = "#FB8C00",
             size = 0.3, stroke = 0) +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "lines"),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(clip = "off", xlim = xlimit, ylim = ylimit)

n1 = "pointindiamap05.jpg"

print(map_cov_plain)
ggsave(file=n1, units="in", width=8, height=11, bg = "white")
