library(tidyverse)
library(sf)
sf_use_s2(FALSE)

load("EBD/ebd_IN_relApr-2023.RData")
load(url("https://github.com/birdcountindia/india-maps/raw/main/outputs/maps_sf.RData"))
load(url("https://github.com/birdcountindia/india-maps/raw/main/outputs/grids_g0_sf.RData"))

data0 <- data %>% filter(STATE == "Andhra Pradesh")
ap_sf <- states_sf %>% filter(STATE.NAME == "Andhra Pradesh")
ap_dists_sf <- dists_sf %>% 
  filter(DISTRICT.NAME %in% c("Chittoor", "Y.S.R.",
                              "Anantapur", "Sri Potti Sriramulu Nellore",
                              "Kurnool"))

# creating 1km2 grid for AP
cs <- 1*1000/111111 # deg value of 1 km x 1 km
n <- (c(diff(st_bbox(ap_sf)[c(1, 3)]), diff(st_bbox(ap_sf)[c(2, 4)]))/cs) %>% ceiling()

g00_sf <- ap_sf %>% 
  st_make_grid(cellsize = cs, n = n) %>% 
  st_as_sf() %>% 
  rename(GEOM.G00 = x) %>% 
  # cell IDs
  rownames_to_column("GRID.G00")



temp <- data0 %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, COUNTY, STATE, LONGITUDE, LATITUDE) %>% 
  # sensitive species haven't got district/admin updates so showing up with different values
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # joining map vars to EBD
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(dists_sf)) %>% 
  st_join(dists_sf %>% dplyr::select(-STATE.NAME, -AREA)) %>% 
  st_join(states_sf %>% dplyr::select(-AREA)) %>% 
  # grid cells
  st_join(g0_in_sf %>% dplyr::select(GRID.G0)) %>% 
  st_join(g00_sf %>% dplyr::select(GRID.G00)) %>% 
  st_drop_geometry()


# first get list of checklists that have NA then join geometry by name of district/state
# cannot do this for grid cells because no existing column in eBird to join by
temp1 <- temp %>% 
  filter(is.na(DISTRICT.NAME)) %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, COUNTY) %>% 
  # sensitive species haven't got district/admin updates so showing up with different values
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(DISTRICT.NAME = COUNTY)

temp2 <- temp %>% 
  filter(is.na(STATE.NAME)) %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, STATE) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(STATE.NAME = STATE)


# then get list of checklists that DO NOT have NA separately
temp1a <- temp %>% 
  filter(!is.na(DISTRICT.NAME)) %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, DISTRICT.NAME) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup() 

temp2a <- temp %>% 
  filter(!is.na(STATE.NAME)) %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, STATE.NAME) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup() 


tempa <- full_join(temp1a, temp2a, by = "SAMPLING.EVENT.IDENTIFIER")


# then get list of checklists with existing grid cell info joined
temp0 <- temp %>% 
  distinct(GRID.G0, GRID.G00, SAMPLING.EVENT.IDENTIFIER) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% 
  ungroup()


# join everything back
temp_all <- full_join(temp1, temp2, by = "SAMPLING.EVENT.IDENTIFIER") %>% 
  full_join(tempa, by = "SAMPLING.EVENT.IDENTIFIER") %>% 
  # this results in .x and .y columns for district and state names, so coalesce
  mutate(DISTRICT.NAME = coalesce(!!!dplyr::select(., contains("DISTRICT.NAME"))),
         STATE.NAME = coalesce(!!!dplyr::select(., contains("STATE.NAME")))) %>% 
  dplyr::select(SAMPLING.EVENT.IDENTIFIER, DISTRICT.NAME, STATE.NAME) %>% 
  left_join(temp0)


# joining GROUP.ID-mapvars info to full data
data0 <- data0 %>% left_join(temp_all)


# calculating number of checklists
data1 <- data0 %>% 
  group_by(GRID.G0) %>% 
  dplyr::summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))


totgrids0 <- g0_in_sf %>% st_intersection(ap_sf) 

totgrids1 <- totgrids0 %>% 
  left_join(data1) %>% 
  mutate(NO.LISTS = replace_na(NO.LISTS, 0)) %>% 
  dplyr::select(-c(AREA, AREA.1, AREA.G0, TOT.G0)) %>% 
  magrittr::set_colnames(c("GRID.G5", "STATE.NAME", "NO.LISTS", "GEOM.G5"))


data2 <- data0 %>% 
  group_by(GRID.G00) %>% 
  dplyr::summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))
  
totgrids2 <- g00_sf %>% 
  left_join(data2) %>% 
  mutate(NO.LISTS = replace_na(NO.LISTS, 0)) %>% 
  st_intersection(ap_dists_sf) %>% 
  dplyr::select(-c(AREA)) %>% 
  magrittr::set_colnames(c("GRID.G1", "NO.LISTS", "STATE.NAME", "DISTRICT.NAME", "GEOM.G1"))


# writing shapefiles
st_geometry(totgrids1) <- "GEOM.G5"
st_write(totgrids1, dsn = "AP_5x5.shp", append = FALSE)

st_geometry(totgrids2) <- "GEOM.G1"
st_write(totgrids2, dsn = "AP_1x1.shp", append = FALSE)
