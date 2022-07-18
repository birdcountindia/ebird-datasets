library(tidyverse)
library(lubridate)


# ensure .RData of EBD is in the directory
load("EBD/ebd_IN_relMar-2022.RData") 
data <- data %>% distinct(OBSERVER.ID)

eBird_users <- read.delim("EBD/ebd_users_relMar-2022.txt", sep = "\t", header = T, 
                          quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
eBird_users <- eBird_users %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))
data0 <- left_join(data, eBird_users)


# textual filters for group accounts
keywords <- c("Group","group","Survey","survey","Atlas","atlas","rganization",
              "rganisation","oundation","AWC","awc","Census","census","Bird","bird",
              "Count","count","niversity","ollege","Centre","Center","centre","center",
              "School","school","Club","club","City","city","State","state",
              "202","201","200","rnitholog","arathon","ociety","istoric","nstitut",
              "etwork","Team","team","estival","Fest","fest","esearch","ecord",
              "roject","iodivers","ational","eserve","rotect","Lodge","lodge","esort",
              "anctuary","ildlife","epartment","Dept","dept","onservation","Trust",
              "trust","ollect","Monitor","monitor")

# other group accounts without keywords in name
accounts <- c("GBCN Goa") 

data1 <- data0 %>% 
  filter(str_detect(FULL.NAME, paste(keywords, collapse = "|")) |
           str_detect(FULL.NAME, paste(accounts, collapse = "|"))) %>% 
  select(OBSERVER.ID)


prev_GAlist <- read_csv("group-accounts/ebd_users_GA_relDec-2021.csv") %>%
  select(-FULL.NAME) 


data2 <- data1 %>% 
  # joining GA.1, GA.2 and justification columns for old users
  left_join(prev_GAlist) %>% 
  left_join(eBird_users) %>% 
  arrange(GA.1)


write_csv(data2, "group-accounts/ebd_users_GA_relMar-2022_0.csv", na = "")


# will modify the .csv by assigning the values for GA.1 and GA.2 with 
# justifications (for new accounts)