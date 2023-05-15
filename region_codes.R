library(tidyverse)

load("EBD/ebd_IN_relMar-2023.RData")


bhu_rawpath <-  "EBD/ebd_BT_relMar-2023.txt"
nep_rawpath <-  "EBD/ebd_NP_relMar-2023.txt"

preimp <- c("STATE.CODE","COUNTY.CODE")

nms <- names(read.delim(bhu_rawpath, nrows = 1, sep = "\t", header = T, quote = "", 
                        stringsAsFactors = F, na.strings = c(""," ", NA)))
nms[!(nms %in% preimp)] <- "NULL"
nms[nms %in% preimp] <- NA

data_BT <- read.delim(bhu_rawpath, colClasses = nms, sep = "\t", header = T, quote = "",
                      stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  mutate(COUNTRY.CODE = "BT") %>% 
  distinct(COUNTRY.CODE, STATE.CODE, COUNTY.CODE)

data_NP <- read.delim(nep_rawpath, colClasses = nms, sep = "\t", header = T, quote = "",
                      stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  mutate(COUNTRY.CODE = "NP") %>% 
  distinct(COUNTRY.CODE, STATE.CODE, COUNTY.CODE)





region_codes <- data %>% 
  mutate(COUNTRY.CODE = "IN") %>% 
  distinct(COUNTRY.CODE, STATE.CODE, COUNTY.CODE) %>% 
  bind_rows(data_BT, data_NP) %>% 
  arrange(COUNTRY.CODE, STATE.CODE, COUNTY.CODE)

save(region_codes, file = "region_codes.RData")
