library(tidyverse)
library(magrittr)


load("EBD/ebd_IN_relMar-2022.RData")


data0 <- data %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup()


# locations
data1 <- data0 %>% 
  distinct(LATITUDE, LONGITUDE) %>% 
  summarise(LOCATIONS = n())

# lists
temp1 <- data0 %>%
  summarise(LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))
# complete lists
temp2 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  summarise(C.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))
# unique lists with media
temp3 <- data0 %>% 
  group_by(GROUP.ID) %>% 
  filter(any(HAS.MEDIA == 1)) %>% 
  ungroup()  %>%
  summarise(M.LISTS = n_distinct(GROUP.ID))
data2 <- cbind(temp1, temp2, temp3)

# cumulative birding hours
data3 <- data0 %>% 
  filter(!is.na(DURATION.MINUTES)) %>% 
  summarise(HOURS = sum(DURATION.MINUTES)/60)

# people
groupaccs <- read_csv("group-accounts/ebd_users_GA_relDec-2021.csv") %>% 
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", 
                              GA.2 == 1 ~ "GA.2", 
                              TRUE ~ "NG"))
filtGA <- groupaccs %>% filter(CATEGORY %in% c("GA.1", "GA.2")) %>% select(OBSERVER.ID)
data4 <- data0 %>% 
  anti_join(filtGA) %>% 
  summarise(PEOPLE = n_distinct(OBSERVER.ID))


# states/UTs
data5 <- data0 %>% 
  summarise(STATES = n_distinct(STATE), DISTRICTS = n_distinct(COUNTY))


# species
data6 <- data %>% 
  mutate(CATEGORY = case_when(CATEGORY == "domestic" & 
                                COMMON.NAME == "Rock Pigeon" ~ "species",
                              TRUE ~ CATEGORY)) %>% 
  filter(APPROVED == 1, CATEGORY %in% c("species", "issf")) %>% 
  summarise(SPECIES = n_distinct(COMMON.NAME))


# observations
data7 <- data %>% summarise(OBSERVATIONS = n()/1000000) # in millions


data_cov <- cbind(data1, data2, data3, data4, data5, data6, data7)
write_csv(data_cov, "Mittal/IN_coverage_relMar-2022.csv")



# coverage map with yellow points

load("maps.RData")

data_loc <- data %>% distinct(LONGITUDE, LATITUDE)

map_cov <- ggplot() +
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
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        # panel.border = element_blank(),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5))+
  coord_map()

ggsave(map_cov, file = "Mittal/IN_coverage_relMar-2022.png", 
       units = "in", width = 8, height = 11, bg = "transparent", dpi = 300)
