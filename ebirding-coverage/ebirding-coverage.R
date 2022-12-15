require(tidyverse)

# Required for maps:
load("maps.RData")
require(magick)
require(scales) # for comma format of numbers
require(grid)
require(glue)


#### creating monthly coverage stats ####

data0 <- data %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup()


### locations ##
data1 <- data0 %>% 
  distinct(LATITUDE, LONGITUDE) %>% 
  summarise(LOCATIONS = n())

### lists ###
temp1 <- data0 %>%
  summarise(LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))
### complete lists ###
temp2 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  summarise(C.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))
### unique lists with media ###
temp3 <- data0 %>% 
  group_by(GROUP.ID) %>% 
  filter(any(HAS.MEDIA == 1)) %>% 
  ungroup()  %>%
  summarise(M.LISTS = n_distinct(GROUP.ID))

data2 <- cbind(temp1, temp2, temp3)

### cumulative birding hours ###
data3 <- data0 %>% 
  filter(!is.na(DURATION.MINUTES)) %>% 
  summarise(HOURS = round(sum(DURATION.MINUTES)/60, 2))

### people ###
groupaccs <- read_csv(groupaccspath) %>% 
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", 
                              GA.2 == 1 ~ "GA.2", 
                              TRUE ~ "NG"))
filtGA <- groupaccs %>% filter(CATEGORY %in% c("GA.1", "GA.2")) %>% select(OBSERVER.ID)

data4 <- data0 %>% 
  anti_join(filtGA) %>% 
  summarise(PEOPLE = n_distinct(OBSERVER.ID))


### states/UTs & districts ###
data5 <- data0 %>% 
  summarise(STATES = n_distinct(STATE), DISTRICTS = n_distinct(COUNTY))


### species ###
data6 <- data %>% 
  mutate(CATEGORY = case_when(CATEGORY == "domestic" &
                                COMMON.NAME == "Rock Pigeon" ~ "species",
                              TRUE ~ CATEGORY)) %>%
  filter(APPROVED == 1, CATEGORY %in% c("species", "issf")) %>% 
  summarise(SPECIES = n_distinct(COMMON.NAME))


### observations ###
data7 <- data %>% summarise(OBSERVATIONS = round(n()/1000000, 2)) # in millions


### coverage data csv ###
data_cov <- cbind(data1, data2, data3, data4, data5, data6, data7)

# to write data with better columns and longer format
data_cov2 <- data_cov
names(data_cov2) <- c("Unique locations", "Total lists", "Complete lists", "Lists with media",
                      "eBirding hours", "eBirders", "States", "Districts", "Species",
                      "Total observations (in millions)")
data_cov2 <- data_cov2 %>% 
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")

write_csv(data_cov2, coveragedatapath)

print("Monthly coverage stats generated and written as CSV.")


#### creating monthly coverage point map  ####

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
        plot.title = element_text(hjust = 0.5))+
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

ggsave(map_cov_annot, file = coveragemappath1, 
       # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
       device = png,
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

ggsave(map_cov_plain, file = coveragemappath2, 
       # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
       device = png,
       units = "in", width = 8, height = 11, bg = "transparent", dpi = 300)

print("Monthly coverage maps created.")
