load(maindatapath)

hindi_states <- data %>% 
  filter(STATE %in% c("Bihar", "Delhi", "Haryana", "Chhattisgarh", "Jharkhand", 
                      "Madhya Pradesh", "Rajasthan", "Uttarakhand", 
                      "Himachal Pradesh", "Uttar Pradesh")) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(COMMON.NAME) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            TOT.LISTS = min(TOT.LISTS),
            REP.FREQ = 100*NO.LISTS/TOT.LISTS) %>% 
  dplyr::select(COMMON.NAME, REP.FREQ) %>% 
  arrange(desc(REP.FREQ)) %>% 
  slice(1:400)

write_csv(hindi_states, file = "hindi_states_top400.csv")