library(tidyverse)
library(glue)

temp <- list.files("EBD/", full.names = T) %>% 
  file.info() %>% 
  rownames_to_column("FILE") %>% 
  filter(str_detect(FILE, "ebd_users") & str_ends(FILE, ".txt")) %>% 
  arrange(desc(mtime)) %>% 
  dplyr::select(FILE) %>% 
  slice(1)

latestusersrel <- temp$FILE %>% str_extract("(?<=rel)[^.]*(?=.|$)")

temp <- list.files("EBD/", full.names = T) %>% 
  file.info() %>% 
  rownames_to_column("FILE") %>% 
  filter(str_detect(FILE, "ebd_sensitive") & str_ends(FILE, ".txt")) %>% 
  arrange(desc(mtime)) %>% 
  dplyr::select(FILE) %>% 
  slice(1)

latestsensrel <- temp$FILE %>% str_extract("(?<=rel)[^.]*(?=.|$)")



userspath <- glue("EBD/ebd_users_rel{latestusersrel}.txt")
groupaccspath <- glue("group-accounts/ebd_users_GA_rel{latestusersrel}.csv")

senspath <- glue("EBD/ebd_sensitive_rel{latestsensrel}.txt")

save(userspath, senspath, groupaccspath,
     file = "EBD/latest_non-EBD_paths.RData")
