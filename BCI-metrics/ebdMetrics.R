#################################################################
############# Script to create monthly metrics out of ebd########
#################################################################

nms2 <- names(read.delim(path_ebd_main, nrows = 1, sep = "\t", header = T, quote = "", 
                         stringsAsFactors = F, na.strings = c(""," ", NA)))
nms2[!(nms2 %in% preimp_metrics)] <- "NULL"
nms2[nms2 %in% preimp_metrics] <- NA
ebd <- read.delim(path_ebd_main, colClasses = nms2, sep = "\t", header = T, quote = "",
                  # nrows = 100000, # For testing, this is useful
                  stringsAsFactors = F, na.strings = c(""," ",NA)) 


### new user stats before preparing data for other analyses/metrics ###
india_new_users_stats <- data.table::setDT(ebd)[, 
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

india_user_stats <- ebd %>%
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


districts <- ebd %>% distinct(COUNTY.CODE, COUNTY)

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
source ("BCI-metrics/mediaMlMetrics.R")

pullMediaStats()

# Move this to mediaMLMetrics later
colnames(photo_stats) <- c("YEAR", "MONTH", "count")
colnames(sound_stats) <- c("YEAR", "MONTH", "count")

source("BCI-metrics/indiaMetrics.R")
source("BCI-metrics/stateMetrics.R")
source("BCI-metrics/districtMetrics.R")

india_metrics <- genIndiaMetrics()
state_metrics <- genStateMetrics()
district_metrics <- genDistrictMetrics()

write_csv(india_metrics, "BCI-metrics/india_metrics.csv")
write_csv(state_metrics, "BCI-metrics/state_metrics.csv")
write_csv(district_metrics, "BCI-metrics/district_metrics.csv")


# uploading to GSheet
source("BCI-metrics/gsheet_functions.R")

write_metrics_sheet(district_metrics, "DT")
write_metrics_sheet(state_metrics, "ST")
write_metrics_sheet(india_metrics, "IN")
