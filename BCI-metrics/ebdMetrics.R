#################################################################
############# Script to create monthly metrics out of ebd########
#################################################################

#Rscript ebdMetrics.R 4 2022 0 ..\data\\

library (tidyverse)
library (lubridate)
library (data.table)

args <- commandArgs(trailingOnly = TRUE)

CurMonth <- as.integer (args [1])
CurYear  <- as.integer (args [2])
unzip <- as.integer(args[3])
dir <- args[4]

print(CurMonth)
print(CurYear)
print (dir)
print(unzip)
print (paste("Generating metrics for", month.abb[CurMonth], CurYear, "from", dir, "with unzip =", unzip))

##
CurMonth <- 5
CurYear  <- 2022
# Keep the zip file in a direct one level up as data
dir <- "..\\ebird-datasets\\EBD\\"
unzip <- 1

PrevYear <- CurYear - 1 

# Last six months
Months <- (CurMonth-5):CurMonth

# Last six months rolling over at Jan
Months <- (Months - 1) %% 12 + 1


ebdfile <- paste0("ebd_IN_prv_rel",month.abb[CurMonth],"-",CurYear)

# List the interested columns
preimp <-  c( "COMMON.NAME",
              "STATE.CODE",
              "COUNTY.CODE",
              "OBSERVATION.DATE",
#              "OBSERVATION.COUNT",
              "OBSERVER.ID",
              "SAMPLING.EVENT.IDENTIFIER",
              "ALL.SPECIES.REPORTED",
              "GROUP.IDENTIFIER",
              "HAS.MEDIA",
              "APPROVED"
)


#Incase the unzip is not done, uncomment this line
if (unzip)
{
  print(paste(dir, ebdfile,'.zip',sep=''))
  unzip(paste(dir, ebdfile,'.zip',sep=''))
}

print(ebdfile)
# Read the header plus first row
nms <- read.delim( paste0 (ebdfile,".txt"),
                  nrows = 1, 
                  sep = '\t', 
                  header = T, 
                  quote = "", 
                  stringsAsFactors = F, 
                  na.strings = c ("", " ",NA)) 
nms <- names(nms)
nms [!(nms %in% preimp)] <- "NULL"
nms [nms %in% preimp] <- NA

ebd <- read.delim(paste0(ebdfile,".txt"),
                  colClasses = nms,
#                  nrows = 100000, # For testing, this is useful
                  sep = '\t', 
                  header = T, 
                  quote = "", 
                  stringsAsFactors = F, 
                  na.strings = c ("", " ",NA)) 
states <- read.csv2("states.csv", sep =",")

###################################################################
india_new_users_stats <- setDT(ebd)[, .(OBSERVATION.DATE = min(OBSERVATION.DATE)), by = OBSERVER.ID] %>%
                          mutate (OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
                                  YEAR  = year(OBSERVATION.DATE),
                                  MONTH = month(OBSERVATION.DATE)) %>%
                          group_by(YEAR, MONTH) %>%
                          summarize (count = n_distinct(OBSERVER.ID)) %>%
                          ungroup %>%
                          filter (YEAR > CurYear - 2)


# Prepare Data for analysis

ebd <- ebd %>% 
          filter (APPROVED == 1, format(as.Date(OBSERVATION.DATE), "%Y") > CurYear - 2) %>%
          mutate (GROUP.ID = ifelse (is.na(GROUP.IDENTIFIER), 
                                     SAMPLING.EVENT.IDENTIFIER,
                                     GROUP.IDENTIFIER),
                  OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
                  YEAR  = year(OBSERVATION.DATE),
                  MONTH = month(OBSERVATION.DATE)
          )


# Reassign states 
#ebd <- ebd %>% mutate (STATE.CODE = 
#                         ifelse (STATE.CODE %in% states_assign$state_in, 
#                                 states_assign [states_assign$state_in == STATE.CODE,] %>% 
#                                   select (state_out) %>% pull(),
#                                 STATE.CODE))



india_obsv_stats <- ebd %>% 
                group_by(YEAR, MONTH) %>%
                summarize (count = n_distinct(COMMON.NAME, GROUP.ID)) %>%
                ungroup

india_list_stats <- ebd %>% 
                filter (ALL.SPECIES.REPORTED == 1) %>%
                group_by(YEAR, MONTH) %>%
                summarize (count = n_distinct(GROUP.ID)) %>%
                ungroup 

india_media_list_stats <- ebd %>% 
                filter (HAS.MEDIA == 1) %>%
                group_by(YEAR, MONTH) %>%
                summarize (count = n_distinct(GROUP.ID)) %>%
                ungroup

india_user_stats <- ebd %>%
                group_by(YEAR, MONTH) %>%
                summarize (count = n_distinct(OBSERVER.ID)) %>%
                ungroup 

india_media_user_stats <- ebd %>%
                filter (HAS.MEDIA == 1) %>%
                group_by(YEAR, MONTH) %>%
                summarize (count = n_distinct(OBSERVER.ID)) %>%
                ungroup 

state_obsv_stats <- ebd %>% 
                group_by(STATE.CODE, YEAR, MONTH) %>%
                summarize (count = n_distinct(COMMON.NAME, GROUP.ID)) %>%
                ungroup 

state_list_stats <- ebd %>% 
                filter (ALL.SPECIES.REPORTED == 1) %>%
                group_by(STATE.CODE, YEAR, MONTH) %>%
                summarize (count = n_distinct(GROUP.ID)) %>%
                ungroup 

state_user_stats <- ebd %>%
                group_by(STATE.CODE, YEAR, MONTH) %>%
                summarize (count = n_distinct(OBSERVER.ID)) %>%
                ungroup 

min_no_of_lists <- 15

county_coverage_stats <- ebd %>% 
                group_by (COUNTY.CODE, YEAR, MONTH, STATE.CODE) %>% 
                summarize ( count = n_distinct (GROUP.ID)) %>%
                ungroup %>% 
                mutate (covered = ifelse(count >= min_no_of_lists, 1, 0)) %>%
                filter (covered == 1) %>%
                group_by (STATE.CODE, YEAR, MONTH) %>%
                summarize ( coverage = round (100 * sum (covered) / states[states$STATE.CODE == STATE.CODE,]$Districts,0))


districts <- ebd$COUNTY.CODE %>% unique() %>% as.data.frame()
colnames(districts) <- c("COUNTY.CODE")

district_obsv_stats <- ebd %>% 
  group_by(COUNTY.CODE, YEAR, MONTH) %>%
  summarize (count = n_distinct(COMMON.NAME, GROUP.ID)) %>%
  ungroup 

district_list_stats <- ebd %>% 
  filter (ALL.SPECIES.REPORTED == 1) %>%
  group_by(COUNTY.CODE, YEAR, MONTH) %>%
  summarize (count = n_distinct(GROUP.ID)) %>%
  ungroup 

district_user_stats <- ebd %>%
  group_by(COUNTY.CODE, YEAR, MONTH) %>%
  summarize (count = n_distinct(OBSERVER.ID)) %>%
  ungroup 


##################################################################
accounting <- function (number)
{
#  return(number)
  return (ifelse (is.finite(number),paste0(number),"_"))
#  return (ifelse (is.finite(number), ifelse (number >=0, paste0(number), paste0("(", -number, ")")),"_"))
}


photo_stats <<- 0
sound_stats <<- 0
video_stats <<- 0
source ("mediaMlMetrics.R")

pullMediaStats()
 
# Move this to mediaMLMetrics later
colnames (photo_stats) <- c ("YEAR", "MONTH", "count")
colnames (sound_stats) <- c ("YEAR", "MONTH", "count")
colnames (video_stats) <- c ("YEAR", "MONTH", "count")

source ("indiaMetrics.R")
source ("stateMetrics.R")
source ("districtMetrics.R")

india_metrics <- genIndiaMetrics()
state_metrics <- genStateMetrics()
district_metrics <- genDistrictMetrics()

write.csv2(india_metrics, "india_metrics.csv")

write.csv2(state_metrics, "state_metrics.csv", row.names = FALSE)

write.csv2(district_metrics, "district_metrics.csv", row.names = FALSE)
