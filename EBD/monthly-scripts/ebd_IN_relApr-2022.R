library(lubridate)
library(tidyverse)

preimp <- c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
            "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","BREEDING.CODE",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
            "TRIP.COMMENTS","HAS.MEDIA")


path_ebd_main <- "EBD/ebd_IN_relApr-2022.txt"
nms <- names(read.delim(path_ebd_main, nrows = 1, sep = "\t", header = T, quote = "", 
                        stringsAsFactors = F, na.strings = c(""," ", NA)))
nms[!(nms %in% preimp)] <- "NULL"
nms[nms %in% preimp] <- NA
data <- read.delim(path_ebd_main, colClasses = nms, sep = "\t", header = T, quote = "",
                   stringsAsFactors = F, na.strings = c(""," ",NA)) 


### sensitive species
senspath <- "EBD/ebd_sensitive_relDec-2021_IN.txt"
nms1 <- names(read.delim(senspath, nrows = 1, sep = "\t", header = T, quote = "", 
                         stringsAsFactors = F, na.strings = c(""," ", NA)))
nms1[!(nms1 %in% preimp)] <- "NULL"
nms1[nms1 %in% preimp] <- NA
senssp <- read.delim(senspath, colClasses = nms1, sep = "\t", header = T, quote = "",
                     stringsAsFactors = F, na.strings = c(""," ",NA))


data <- bind_rows(data, senssp) 


met_week <- function(dates) {
  require(lubridate)
  normal_year <- c((0:363 %/% 7 + 1), 52)
  leap_year   <- c(normal_year[1:59], 9, normal_year[60:365])
  year_day    <- yday(dates)
  return(ifelse(leap_year(dates), leap_year[year_day], normal_year[year_day])) 
}


### adding useful columns 
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
save(data, file = "EBD/ebd_IN_relApr-2022.RData")



### filtering for PMP
data_pmp <- data %>% group_by(GROUP.ID) %>%
  filter(any(OBSERVER.ID == "obsr2607928")) # PMP account ID

save(data_pmp, file = "EBD/pmp_relApr-2022.RData")

rm(list = setdiff(ls(envir = .GlobalEnv), c("data")), pos = ".GlobalEnv")



### Month of April for monthly challenge
data_mc <- data %>% filter(YEAR == 2022 & MONTH == 4)

rm(.Random.seed)
save(data_mc, file = "EBD/ebd_IN_relApr-2022_APR.RData")

