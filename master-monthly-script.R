library(lubridate)
library(tidyverse)
library(glue)
library(data.table)

### required files in directory: ###
# - latest EBD release .txt file
# - latest sensitive species data .txt file 
# - latest group accounts data .csv file 
# - spatial data (pre-processed) as "maps.RData" file
# - BCI logo with circular frame as .png file

###   ###


#### variable parameters ####

# parameters of interest that might change over time depending on requirements
# but will stay same for a number of months at a time

# update when latest available
senspath <- "EBD/ebd_sensitive_relMay-2022_IN.txt" 
groupaccspath <- "group-accounts/ebd_users_GA_relDec-2021.csv"

dataset_str <- "ebd_IN_prv_rel" # or "ebd_IN_rel" if no unvetted data

preimp <- c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
            "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","BREEDING.CODE",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
            "TRIP.COMMENTS","HAS.MEDIA")

preimp_metrics <- c("COMMON.NAME", "STATE.CODE", "COUNTY.CODE", "OBSERVATION.DATE",
                    # "OBSERVATION.COUNT",
                    "OBSERVER.ID", "SAMPLING.EVENT.IDENTIFIER", "ALL.SPECIES.REPORTED",
                    "GROUP.IDENTIFIER", "HAS.MEDIA", "APPROVED")

#### automation parameters ####

# (only works when using new rel. data in the month it comes out, e.g., working with relJun in July)

cur_year <- today() %>% year()
cur_month_num <- today() %>% month()
cur_month_lab <- today() %>% month(label = T, abbr = T)

rel_year <- (today() - months(1)) %>% year()
rel_month_num <- (today() - months(1)) %>% month()
rel_month_lab <- (today() - months(1)) %>% month(label = T, abbr = T) 

# for PJ's metrics
CurMonth <- rel_month_num
CurYear <- rel_year
PrevYear <- rel_year - 1
Months <- seq((today() - months(6)), (today() - months(1)), by = "month") %>% month()


zippath <- glue("EBD/{dataset_str}{rel_month_lab}-{rel_year}.zip")
rawfile <- glue("{dataset_str}{rel_month_lab}-{rel_year}.txt")
rawpath <- glue("EBD/{rawfile}")

maindatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")
pmpdatapath <- glue("EBD/pmp_rel{rel_month_lab}-{rel_year}.RData")
mcdatapath <-  glue("EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_{toupper(rel_month_lab)}.RData")

coveragedatapath <- glue("ebirding-coverage/data/coverage_rel{rel_month_lab}-{rel_year}.csv")
coveragemappath <- glue("ebirding-coverage/maps/coverage_rel{rel_month_lab}-{rel_year}.png")

#### unzipping EBD download ####

unzip(zipfile = zippath, files = rawfile, exdir = "EBD") # don't add trailing slash in path


#### main data processing steps ####

### main EBD ###

# this method using base R import takes only 373 sec with May 2022 release
nms <- names(read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", 
                        stringsAsFactors = F, na.strings = c(""," ", NA)))
nms[!(nms %in% preimp)] <- "NULL"
nms[nms %in% preimp] <- NA
data <- read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "",
                   stringsAsFactors = F, na.strings = c(""," ",NA)) 

# # tidy import takes way longer, a total of 877 sec, but could be useful for smaller data
# data <- read_delim(rawpath, col_select = preimp,
#                    name_repair = make.names, # base R nomencl. with periods for spaces
#                    quote = "", na = c(""," ", NA), show_col_types = F)


### sensitive species ###
nms1 <- names(read.delim(senspath, nrows = 1, sep = "\t", header = T, quote = "", 
                         stringsAsFactors = F, na.strings = c(""," ", NA)))
nms1[!(nms1 %in% preimp)] <- "NULL"
nms1[nms1 %in% preimp] <- NA
senssp <- read.delim(senspath, colClasses = nms1, sep = "\t", header = T, quote = "",
                     stringsAsFactors = F, na.strings = c(""," ",NA))

### combing the two ###
data <- bind_rows(data, senssp) %>% 
  # filtering unvetted as well as exotic species
  filter(APPROVED == 1)


### adding useful columns ###
met_week <- function(dates) {
  require(lubridate)
  
  normalyear <- c((0:363 %/% 7 + 1), 52)
  leapyear   <- c(normalyear[1:59], 9, normalyear[60:365])
  yearday    <- yday(dates)
  
  return(ifelse(leap_year(dates), leapyear[yearday], normalyear[yearday])) 
}

data <- data %>% 
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, 
                           GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE)) 
# DAY.Y = yday(OBSERVATION.DATE), 
# WEEK.Y = met_week(OBSERVATION.DATE), 
# M.YEAR = if_else(DAY.Y <= 151, YEAR-1, YEAR), # from 1st June to 31st May
# WEEK.MY = if_else(WEEK.Y > 21, WEEK.Y-21, 52-(21-WEEK.Y)))

rm(.Random.seed)
save(data, file = maindatapath)


#### filtering for PMP (only for alternate months) ####

pmp_months <- seq(1, 12, by = 2) # odd months

if (cur_month_num %in% pmp_months) {

data_pmp <- data %>% group_by(GROUP.ID) %>%
  filter(any(OBSERVER.ID == "obsr2607928")) %>% # PMP's eBird account ID
  ungroup()

save(data_pmp, file = pmpdatapath)

}



#### filtering for monthly challenge ####

data_mc <- data %>% filter(YEAR == rel_year, MONTH == rel_month_num)

rm(.Random.seed)
save(data_mc, file = mcdatapath)


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
  summarise(HOURS = sum(DURATION.MINUTES)/60)

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
data7 <- data %>% summarise(OBSERVATIONS = n()/1000000) # in millions


### coverage data csv ###
data_cov <- cbind(data1, data2, data3, data4, data5, data6, data7)
write_csv(data_cov, coveragedatapath)



#### creating monthly coverage point map  ####

load("maps.RData")

require(magick)
require(scales) # for comma format of numbers
require(grid)

map_cov_logo <- image_convert(image_read("bcilogo-framed.png"), matte = T)

map_cov_text <- glue::glue("{label_comma()(data_cov$LOCATIONS)} locations
                      {label_comma()(data_cov$LISTS)} lists
                      {label_comma()(round(data_cov$HOURS))} hours
                      {label_comma()(data_cov$PEOPLE)} people
                      
                      {label_comma()(data_cov$STATES)} states/UTs
                      {label_comma()(data_cov$DISTRICTS)} districts
                      
                      {label_comma()(data_cov$SPECIES)} species
                      {label_comma()(round(data_cov$OBSERVATIONS, 1))} million observations")

map_cov_footer <- glue::glue("Data until {rel_month_lab} {rel_year}")


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
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1,1,0,23), "lines")) +
  annotation_raster(map_cov_logo, 
                    ymin = 31, ymax = 37,
                    xmin = 49, xmax = 55) +
  annotation_custom(textGrob(label = map_cov_text,
                             hjust = 0,
                             gp = gpar(col = "#FCFA53", cex = 1.5)),
                    ymin = 12, ymax = 24,
                    xmin = 42, xmax = 55)  +
  annotation_custom(textGrob(label = map_cov_footer,
                             hjust = 0,
                             gp = gpar(col = "#D2D5DA", cex = 1.0)),
                    ymin = 8, ymax = 9,
                    xmin = 42, xmax = 55) 

ggsave(map_cov, file = coveragemappath, 
       units = "in", width = 13, height = 9, bg = "transparent", dpi = 300)

#### generating PJ's monthly metrics out of EBD ####

print(glue::glue("Generating metrics for {rel_month_lab} {rel_year} from {zippath}"))


nms2 <- names(read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", 
                        stringsAsFactors = F, na.strings = c(""," ", NA)))
nms2[!(nms2 %in% preimp_metrics)] <- "NULL"
nms2[nms2 %in% preimp_metrics] <- NA
ebd <- read.delim(rawpath, colClasses = nms2, sep = "\t", header = T, quote = "",
                  # nrows = 100000, # For testing, this is useful
                  stringsAsFactors = F, na.strings = c(""," ",NA)) 


### new user stats before preparing data for other analyses/metrics ###
india_new_users_stats <- setDT(ebd)[, 
                                    .(OBSERVATION.DATE = min(OBSERVATION.DATE)), 
                                    by = OBSERVER.ID] %>% # dt takes 1.29 sec while tidy takes 8.65 sec
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
         YEAR = year(OBSERVATION.DATE),
         MONTH = month(OBSERVATION.DATE)) %>%
  group_by(YEAR, MONTH) %>%
  summarise(count = n_distinct(OBSERVER.ID)) %>%
  ungroup() %>%
  filter(YEAR > CurYear - 2)


### preparing data (filtering + adding useful columns) ###
ebd <- ebd %>%
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, 
                           GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE)) %>% 
  filter(APPROVED == 1, YEAR > CurYear - 2)


# ### Reassign states ###
# ebd <- ebd %>% mutate (STATE.CODE =
#                         ifelse (STATE.CODE %in% states_assign$state_in,
#                                 states_assign [states_assign$state_in == STATE.CODE,] %>%
#                                   select (state_out) %>% pull(),
#                                 STATE.CODE))


states <- read_csv("BCI-metrics/states.csv")


india_obsv_stats <- ebd %>% 
  group_by(YEAR, MONTH) %>%
  summarise(count = n_distinct(COMMON.NAME, GROUP.ID)) %>%
  ungroup()

india_list_stats <- ebd %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(YEAR, MONTH) %>%
  summarise(count = n_distinct(GROUP.ID)) %>%
  ungroup()

india_media_list_stats <- ebd %>% 
  filter(HAS.MEDIA == 1) %>%
  group_by(YEAR, MONTH) %>%
  summarise(count = n_distinct(GROUP.ID)) %>%
  ungroup()

india_user_stats <- ebd %>%
  group_by(YEAR, MONTH) %>%
  summarise(count = n_distinct(OBSERVER.ID)) %>%
  ungroup()

india_media_user_stats <- ebd %>%
  filter(HAS.MEDIA == 1) %>%
  group_by(YEAR, MONTH) %>%
  summarise(count = n_distinct(OBSERVER.ID)) %>%
  ungroup()

state_obsv_stats <- ebd %>% 
  group_by(STATE.CODE, YEAR, MONTH) %>%
  summarise(count = n_distinct(COMMON.NAME, GROUP.ID)) %>%
  ungroup()

state_list_stats <- ebd %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(STATE.CODE, YEAR, MONTH) %>%
  summarise(count = n_distinct(GROUP.ID)) %>%
  ungroup()

state_user_stats <- ebd %>%
  group_by(STATE.CODE, YEAR, MONTH) %>%
  summarise(count = n_distinct(OBSERVER.ID)) %>%
  ungroup()

min_no_of_lists <- 15

county_coverage_stats <- ebd %>% 
  group_by(STATE.CODE, YEAR, MONTH, COUNTY.CODE) %>% 
  summarise(count = n_distinct(GROUP.ID)) %>%
  ungroup() %>% 
  mutate(covered = ifelse(count >= min_no_of_lists, 1, 0)) %>%
  filter(covered == 1) %>%
  left_join(states, by = "STATE.CODE") %>% 
  group_by(STATE.CODE, YEAR, MONTH) %>%
  summarise(coverage = round(100 * sum(covered) / min(Districts), 0))


districts <- ebd %>% distinct(COUNTY.CODE)

district_obsv_stats <- ebd %>% 
  group_by(COUNTY.CODE, YEAR, MONTH) %>%
  summarise(count = n_distinct(COMMON.NAME, GROUP.ID)) %>%
  ungroup() 

district_list_stats <- ebd %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(COUNTY.CODE, YEAR, MONTH) %>%
  summarise(count = n_distinct(GROUP.ID)) %>%
  ungroup() 

district_user_stats <- ebd %>%
  group_by(COUNTY.CODE, YEAR, MONTH) %>%
  summarise(count = n_distinct(OBSERVER.ID)) %>%
  ungroup() 


###   ###


accounting <- function (number)
{
  #  return(number)
  return (ifelse (is.finite(number),paste0(number),"_"))
  #  return (ifelse (is.finite(number), ifelse (number >=0, paste0(number), paste0("(", -number, ")")),"_"))
}


photo_stats <<- 0
sound_stats <<- 0
video_stats <<- 0
source ("BCI-metrics/mediaMlMetrics.R")

pullMediaStats()

# Move this to mediaMLMetrics later
colnames(photo_stats) <- c("YEAR", "MONTH", "count")
colnames(sound_stats) <- c("YEAR", "MONTH", "count")
colnames(video_stats) <- c("YEAR", "MONTH", "count")

source("BCI-metrics/indiaMetrics.R")
source("BCI-metrics/stateMetrics.R")
source("BCI-metrics/districtMetrics.R")

india_metrics <- genIndiaMetrics()
state_metrics <- genStateMetrics()
district_metrics <- genDistrictMetrics()

write.csv2(india_metrics, "BCI-metrics/india_metrics.csv")

write.csv2(state_metrics, "BCI-metrics/state_metrics.csv", row.names = FALSE)

write.csv2(district_metrics, "BCI-metrics/district_metrics.csv", row.names = FALSE)
