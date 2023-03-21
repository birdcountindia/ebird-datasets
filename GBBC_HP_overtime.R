library(tidyverse)
library(writexl)
library(sf)


senspath <- "EBD/ebd_sensitive_relFeb-2023_IN.txt" 
groupaccspath <- "group-accounts/ebd_users_GA_relFeb-2023.csv"

dataset_str <- "ebd_IN_prv_rel" # or "ebd_IN_rel" if no unvetted data
source("monthly-param-auto.R")

load(maindatapath)
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/summaries.R")



data0 <- data %>% 
  filter(STATE.CODE == "IN-HP", 
         MONTH == 2) %>% 
  # GBBCs
  filter((YEAR == 2014 & DAY.M %in% 14:17) |
           (YEAR == 2015 & DAY.M %in% 13:16) |
           (YEAR == 2016 & DAY.M %in% 12:15) |
           (YEAR == 2017 & DAY.M %in% 17:20) |
           (YEAR == 2018 & DAY.M %in% 16:19) |
           (YEAR == 2019 & DAY.M %in% 15:18) |
           (YEAR == 2020 & DAY.M %in% 14:17) |
           (YEAR == 2021 & DAY.M %in% 12:15) |
           (YEAR == 2022 & DAY.M %in% 18:21) |
            (YEAR == 2023 & DAY.M %in% 17:20))

summary <- data0 %>% 
  group_by(YEAR) %>% 
  basic_stats(pipeline = T, prettify = F)


# top 5 common species, over time -----------------------------------------

comm_spec <- data0 %>% 
  group_by(YEAR) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(YEAR, COMMON.NAME) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            REP.FREQ = 100*NO.LISTS/min(TOT.LISTS)) %>% 
  arrange(desc(REP.FREQ)) %>% 
  slice(1:5) %>% 
  ungroup()



write_xlsx(x = list("Stats" = summary, 
                    "Common species" = comm_spec),
           path = "GBBC_HP_overtime.xlsx")
  

# districtwise stats on interactive map -----------------------------------

load("../india-maps/outputs/maps_sf.RData")

cur_dists_sf <- dists_sf %>% filter(STATE.NAME == "Himachal Pradesh")

dist_stats <- data0 %>% 
  # joining map vars to EBD
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(dists_sf)) %>% 
  st_join(dists_sf %>% dplyr::select(-STATE.NAME, -AREA)) %>% 
  st_drop_geometry() %>% 
  group_by(YEAR, DISTRICT.NAME) %>% 
  basic_stats(pipeline = T, prettify = F) %>% 
  ungroup() %>% 
  # keeping only necessary
  dplyr::select(YEAR, DISTRICT.NAME, SPECIES, LISTS.ALL, PARTICIPANTS, LOCATIONS) %>% 
  complete(YEAR = 2014:2023, 
           DISTRICT.NAME = cur_dists_sf$DISTRICT.NAME, 
           fill = list(SPECIES = 0,
                       LISTS.ALL = 0,
                       PARTICIPANTS = 0,
                       LOCATIONS = 0)) %>% 
  ungroup() %>% 
  right_join(cur_dists_sf %>% dplyr::select(-AREA)) %>% 
  st_as_sf()

map_dist_stats_lists <- ggplot(dist_stats) + 
  geom_sf(aes(fill = LISTS.ALL)) +
  facet_wrap(~ YEAR, ncol = 5) +
  scale_fill_viridis_b(breaks = c(5, 10, 25, 50, 100), 
                       values = scales::rescale(c(0, 5, 10, 25, 50, 100, 150)),
                       limits = c(0, 150),
                       begin = 0, end = 1,
                       name = "Total\nlists") + 
  theme_classic() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

ggsave(map_dist_stats_lists, filename = "GBBC_HP_overtime_disteffortmap_lists.png",
       dpi = 300, width = 12, height = 6, units = "in")

map_dist_stats_obs <- ggplot(dist_stats) + 
  geom_sf(aes(fill = PARTICIPANTS)) +
  facet_wrap(~ YEAR, ncol = 5) +
  scale_fill_viridis_b(breaks = c(2, 5, 10, 20), 
                       values = scales::rescale(c(0, 2, 5, 10, 20, 50)),
                       limits = c(0, 50),
                       begin = 0, end = 1,
                       name = "Total\nparticipants") + 
  theme_classic() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

ggsave(map_dist_stats_obs, filename = "GBBC_HP_overtime_disteffortmap_obs.png",
       dpi = 300, width = 12, height = 6, units = "in")

map_dist_stats_spec <- ggplot(dist_stats) + 
  geom_sf(aes(fill = SPECIES)) +
  facet_wrap(~ YEAR, ncol = 5) +
  scale_fill_viridis_b(breaks = c(10, 50, 100, 150), 
                       values = scales::rescale(c(0, 10, 50, 100, 150, 250)),
                       limits = c(0, 250),
                       begin = 0, end = 1,
                       name = "Total\nspecies") + 
  theme_classic() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

ggsave(map_dist_stats_spec, filename = "GBBC_HP_overtime_disteffortmap_spec.png",
       dpi = 300, width = 12, height = 6, units = "in")
