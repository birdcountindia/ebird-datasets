require(tidyverse)

# Required for maps:
load("maps.RData")
require(magick)
require(scales) # for comma format of numbers
require(grid)
require(glue)
require(sf)

# converting spdf to sf
indiamap_sf <- indiamap %>% 
  st_as_sf() %>% 
  dplyr::select(-DISTRICT) 

statemap_sf <- statemap %>% 
  st_as_sf() %>% 
  dplyr::select(ST_NM, geometry) %>% 
  magrittr::set_colnames(c("STATE", "geometry")) %>% 
  mutate(STATE = str_to_title(STATE)) %>% 
  # replacing ampersand with "and"
  mutate(STATE = str_replace(STATE, "&", "and")) %>% 
  # corrections for Madhu's SPDF values
  mutate(STATE = case_when(STATE == "Dadra and Nagar Have" ~ "Dadra and Nagar Haveli",
                           STATE == "Andaman and Nicobar" ~ "Andaman and Nicobar Islands",
                           TRUE ~ STATE))

source("ebirding-coverage/ebirding-coverage-functions.R")

#### creating monthly coverage stats for India ####

cov_stats(data, scale = "country")

### coverage data csv ###
data_cov <- cbind(data1, data2, data3, data4, data5, data6, data7)

# to write data with better columns and longer format
data_cov2 <- data_cov %>% 
  magrittr::set_colnames(c("Unique locations", "Total lists", "Complete lists", "Lists with media",
                           "eBirding hours", "eBirders", "States", "Districts", "Species",
                           "Total observations (in millions)")) %>% 
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")

write_csv(data_cov2, coveragedatapath)

print("Monthly coverage stats for India generated and written as CSV.")


#### creating monthly coverage point map for India  ####

map_cov_logo <- image_convert(image_read("bcilogo-framed.png"), matte = T)

map_cov_text <- glue::glue("{label_comma()(data_cov$LOCATIONS)} locations
                      {label_comma()(data_cov$LISTS)} lists
                      {label_comma()(data_cov$HOURS)} hours
                      {label_comma()(data_cov$PEOPLE)} people
                      
                      {label_comma()(data_cov$STATES)} states/UTs
                      {label_comma()(data_cov$DISTRICTS)} districts
                      
                      {label_comma()(data_cov$SPECIES)} species
                      {round(data_cov$OBSERVATIONS, 1)} million observations")

map_cov_footer <- glue::glue("Data until {rel_month_lab} {rel_year}")


data_loc <- data %>% distinct(LONGITUDE, LATITUDE)

cur_bbox <- data_loc %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% st_bbox()
plot_lims()


### map with annotations of stats and BCI logo ###
map_cov_annot <- ggplot() +
  geom_sf(data = indiamap_sf, colour = NA, fill = "black") +
  geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE),
             colour = "#fcfa53", size = 0.05, stroke = 0) +
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
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5))+
  coord_sf(clip = "off", xlim = xlimit, ylim = ylimit) +
  theme(plot.margin = unit(c(0,0,0,24), "lines")) +
  annotation_raster(map_cov_logo, 
                    ymin = -0.85, ymax = 1.8,
                    xmin = 33.75, xmax = 41.0) +
  annotation_custom(textGrob(label = map_cov_text,
                             hjust = 0,
                             gp = gpar(col = "#FCFA53", cex = 1.0)),
                    ymin = 14, ymax = 29,
                    xmin = 33.3, xmax = 34)  +
  annotation_custom(textGrob(label = map_cov_footer,
                             hjust = 0,
                             gp = gpar(col = "#D2D5DA", cex = 0.7)),
                    ymin = 11.29, ymax = 11.3,
                    xmin = 33.3, xmax = 34) 

ggsave(coveragemappath1, map_cov_annot,  
       # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
       device = png,
       units = "in", width = 10, height = 7, bg = "black", dpi = 300)


### plain map without annotations ###
map_cov_plain <- ggplot() +
  geom_sf(data = indiamap_sf, colour = NA, fill = "black") +
  geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE), 
             colour = "#fcfa53", size = 0.05, stroke = 0) +
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
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(clip = "off", xlim = xlimit, ylim = ylimit) 

ggsave(coveragemappath2, map_cov_plain, 
       # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
       device = png,
       units = "in", width = 8, height = 8, bg = "black", dpi = 300)

print("Monthly coverage maps for India created.")


#### uploading to GDrive ###

# "put" overwrites/updates existing file whereas "upload" creates new files each time
drive_put(coveragemappath1, 
          path = as_id("16qhxEVi7POAHaeQs_z4u6-C9IoHdrd8T"),
          name = glue("{str_pad(0, width=2, pad='0')} India.png"))

#### creating monthly coverage stats for states ####

cov_stats(data, scale = "state")

### coverage data csv ###
data_cov <- data1 %>% left_join(data2) %>% left_join(data3) %>% left_join(data4) %>% 
  left_join(data5) %>% left_join(data6) %>% left_join(data7)


### looping across states ###

for (i in unique(state_info$STATE)) {
  
  cur_state <- i
  
  coveragedatapath <- filter(coveragepaths_st, STATE == cur_state)$DATA
  
  data_cov_state <- filter(data_cov, STATE == cur_state)
  
  # to write data with better columns and longer format
  data_cov2 <- data_cov_state %>% 
    magrittr::set_colnames(c("State", "Unique locations", "Total lists", "Complete lists", "Lists with media",
                             "eBirding hours", "eBirders", "States", "Districts", "Species",
                             "Total observations")) %>% 
    pivot_longer(cols = c(everything(), -State), names_to = "Statistic", values_to = "Value")
  
  write_csv(data_cov2, coveragedatapath)
  
  print(glue("Monthly coverage stats for {cur_state} generated and written as CSV."))
  
}
  
#### creating monthly coverage point map for states ####

tictoc::tic("Monthly coverage maps for states COMPLETE") 
count <- 0
for (i in unique(state_info$STATE)) {
  
  cur_state <- i
  count <- count + 1
  
  coveragemappath1 <- filter(coveragepaths_st, STATE == cur_state)$MAP1
  coveragemappath2 <- filter(coveragepaths_st, STATE == cur_state)$MAP2
  
  data_cov_state <- filter(data_cov, STATE == cur_state)
  
  
  map_cov_logo <- image_convert(image_read("bcilogo-framed.png"), matte = T)
  
  map_cov_text <- glue::glue("{str_to_upper(cur_state)}
                             
                             {label_comma()(data_cov_state$LOCATIONS)} locations
                             {label_comma()(data_cov_state$LISTS)} lists
                             {label_comma()(data_cov_state$HOURS)} hours
                             {label_comma()(data_cov_state$PEOPLE)} people
                             
                             {label_comma()(data_cov_state$DISTRICTS)} districts
                             
                             {label_comma()(data_cov_state$SPECIES)} species
                             {label_comma()(data_cov_state$OBSERVATIONS)} observations")
  
  map_cov_footer <- glue::glue("Data until {rel_month_lab} {rel_year}")
  
  
  data_loc <- data %>% filter(STATE == cur_state) %>% distinct(LONGITUDE, LATITUDE)
  
  cur_bbox <- data_loc %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% st_bbox()
  plot_lims()
  
  annot_lims()
  
  ### map with annotations of stats and BCI logo ###
  map_cov_annot <- ggplot() +
    geom_sf(data = filter(statemap_sf, STATE == cur_state),
            colour = NA, fill = "black") +
    geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE),
               colour = "#fcfa53", size = 0.05, stroke = 0) +
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
          plot.background = element_rect(fill = "black", colour = NA),
          panel.background = element_rect(fill = "black", colour = NA),
          plot.title = element_text(hjust = 0.5)) +
    coord_sf(clip = "off", xlim = xlimit, ylim = ylimit) +
    theme(plot.margin = unit(c(0,0,0,24), "lines")) +
    annotation_raster(map_cov_logo,
                      ymin = a1$ymin, ymax = a1$ymax,
                      xmin = a1$xmin, xmax = a1$xmax) +
    annotation_custom(textGrob(label = map_cov_text,
                               hjust = 0,
                               gp = gpar(col = "#FCFA53", cex = 1.0)),
                      ymin = a2$ymin, ymax = a2$ymax,
                      xmin = a2$xmin, xmax = a2$xmax) +
    annotation_custom(textGrob(label = map_cov_footer,
                               hjust = 0,
                               gp = gpar(col = "#D2D5DA", cex = 0.7)),
                      ymin = a3$ymin, ymax = a3$ymax,
                      xmin = a3$xmin, xmax = a3$xmax) 
  
  ggsave(coveragemappath1, map_cov_annot,
         # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
         device = png,
         units = "in", width = 10, height = 7, bg = "black", dpi = 300)
  
  
  ### plain map without annotations ###
  map_cov_plain <- ggplot() +
    geom_sf(data = filter(statemap_sf, STATE == cur_state),
            colour = NA, fill = "black") +
    geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE), 
               colour = "#fcfa53", size = 0.05, stroke = 0) +
    labs(title = str_to_upper(cur_state)) +
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
          plot.margin = unit(c(0.75, 1.5, 1.5, 1.5), "lines"),
          plot.background = element_rect(fill = "black", colour = NA),
          panel.background = element_rect(fill = "black", colour = NA),
          plot.title = element_text(hjust = 0.5, colour = "#FCFA53")) +
    coord_sf(clip = "off", xlim = xlimit, ylim = ylimit) 

    
  ggsave(coveragemappath2, map_cov_plain, 
         # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
         device = png,
         units = "in", width = 8, height = 8, bg = "black", dpi = 300)
  
  print(glue("Monthly coverage maps for {cur_state} ({count}/37) created."))
  
  #### uploading to GDrive ###

  # "put" overwrites/updates existing file whereas "upload" creates new files each time
  drive_put(coveragemappath1, 
            path = as_id("16qhxEVi7POAHaeQs_z4u6-C9IoHdrd8T"),
            name = glue("{str_pad(count, width=2, pad='0')} {cur_state}.png"))
  
  print(glue("Monthly coverage maps for {cur_state} ({count}/37) uploaded to GDrive."))
  
}
tictoc::toc()
