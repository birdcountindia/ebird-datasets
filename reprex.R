
library(lubridate)
library(tidyverse)
library(glue)

library(magick)
library(scales) # for comma format of numbers
library(grid)


# loading objects
load("reprex.RData")


map_cov_logo <- image_convert(image_read("bcilogo-framed.png"), matte = T)

map_cov_text <- glue::glue("{label_comma()(data_cov$LOCATIONS)} locations
                      {label_comma()(data_cov$LISTS)} lists
                      {label_comma()(data_cov$HOURS)} hours
                      {label_comma()(data_cov$PEOPLE)} people
                      
                      {label_comma()(data_cov$STATES)} states/UTs
                      {label_comma()(data_cov$DISTRICTS)} districts
                      
                      {label_comma()(data_cov$SPECIES)} species
                      {round(data_cov$OBSERVATIONS, 1)} million observations")

map_cov_footer <- glue::glue("Data until September 2022")


### map with annotations of stats and BCI logo ###
map_cov_annot <- ggplot() +
  geom_polygon(data = indiamap, aes(x = long, y = lat, group = group), 
               colour = NA, fill = "black")+
  geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE), 
             colour = "#fcfa53", size = 0.05, stroke = 0) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
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
        # panel.border = element_blank(),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(2,2,0,23), "lines")) +
  annotation_raster(map_cov_logo, 
                    ymin = 4.5, ymax = 6.5,
                    xmin = 46.5, xmax = 53.1) +
  annotation_custom(textGrob(label = map_cov_text,
                             hjust = 0,
                             gp = gpar(col = "#FCFA53", cex = 1.5)),
                    ymin = 19, ymax = 31,
                    xmin = 40, xmax = 53)  +
  annotation_custom(textGrob(label = map_cov_footer,
                             hjust = 0,
                             gp = gpar(col = "#D2D5DA", cex = 1.0)),
                    ymin = 15, ymax = 16,
                    xmin = 40, xmax = 53) 

ggsave(map_cov_annot, file = "map_cov_annot.png", device = png,
       units = "in", width = 13, height = 9, bg = "transparent", dpi = 300)



### plain map without annotations ###
map_cov_plain <- ggplot() +
  geom_polygon(data = indiamap, aes(x = long, y = lat, group = group), 
               colour = NA, fill = "black")+
  geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE), 
             colour = "#fcfa53", size = 0.05, stroke = 0.1) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
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
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        # panel.border = element_blank(),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5)) +
  coord_map()

ggsave(map_cov_plain, file = "map_cov_plain.png", device = "png",
       units = "in", width = 8, height = 11, bg = "transparent", dpi = 300)
