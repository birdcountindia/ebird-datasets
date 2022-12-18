#### function for creating monthly coverage stats ####

cov_stats <- function(data, scale, state_info)
{
  if (scale == "country") {
    
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
      summarise(HOURS = floor(sum(DURATION.MINUTES)/60))
    
    ### people ###
    groupaccs <- read_csv(groupaccspath, show_col_types = F) %>% 
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
    data7 <- data %>% summarise(OBSERVATIONS = round(n()/1000000, 2)) # in millions
    
  } else if (scale == "state") {
    
    data0 <- data %>% 
      group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
      slice(1) %>% 
      group_by(STATE)
    
    
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
      group_by(STATE) %>% 
      summarise(M.LISTS = n_distinct(GROUP.ID))
    
    data2 <- temp1 %>% left_join(temp2) %>% left_join(temp3)
    
    ### cumulative birding hours ###
    data3 <- data0 %>% 
      filter(!is.na(DURATION.MINUTES)) %>% 
      summarise(HOURS = floor(sum(DURATION.MINUTES)/60))
    
    ### people ###
    groupaccs <- read_csv(groupaccspath, show_col_types = F) %>% 
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
      group_by(STATE) %>% 
      summarise(SPECIES = n_distinct(COMMON.NAME))
    
    ### observations ###
    data7 <- data %>% 
      group_by(STATE) %>% 
      summarise(OBSERVATIONS = n()) 
    
  }
  
  assign("data1", data1, envir = .GlobalEnv)
  assign("data2", data2, envir = .GlobalEnv)
  assign("data3", data3, envir = .GlobalEnv)
  assign("data4", data4, envir = .GlobalEnv)
  assign("data5", data5, envir = .GlobalEnv)
  assign("data6", data6, envir = .GlobalEnv)
  assign("data7", data7, envir = .GlobalEnv)
  
}

#### decide plot limits ####

plot_lims <- function(){
  
  xextent1 <- cur_bbox[3] - cur_bbox[1]
  yextent1 <- cur_bbox[4] - cur_bbox[2]
  
  if (xextent1 > yextent1) {
    ext_diff <- xextent1 - yextent1
    
    xlimit <- c(cur_bbox[1], cur_bbox[3])
    ylimit <- c((cur_bbox[2] - ext_diff/2), (cur_bbox[4] + ext_diff/2))
  } 
  if (yextent1 > xextent1) {
    ext_diff <- yextent1 - xextent1
    
    xlimit <- c((cur_bbox[1] - ext_diff/2), (cur_bbox[3] + ext_diff/2))
    ylimit <- c(cur_bbox[2], cur_bbox[4])
  }
  
  # true extent of map area given these limits
  xextent2 <- xlimit[2] - xlimit[1]
  yextent2 <- ylimit[2] - ylimit[1]
  
  # output
  assign("xextent1", xextent1, envir = .GlobalEnv)
  assign("yextent1", yextent1, envir = .GlobalEnv)
  assign("xextent2", xextent2, envir = .GlobalEnv)
  assign("yextent2", yextent2, envir = .GlobalEnv)
  assign("xlimit", xlimit, envir = .GlobalEnv)
  assign("ylimit", ylimit, envir = .GlobalEnv)
  
}

#### decide annotation limits ####

annot_lims <- function(){
  
  annot_lims_y <- read_csv("ebirding-coverage/annot_lims_config.csv", show_col_types = F) %>% 
    filter(STATE == cur_state)
  
  annot_lims_x <- data.frame(NO = c("a1", "a2", "a3"),
                             xmin = c(xlimit[1] - (28.5/29.5)*(xextent2),
                                      (xlimit[1] - (28.5/29.5)*(xextent2)) - (13/29.5)*xextent2,
                                      (xlimit[1] - (28.5/29.5)*(xextent2)) - (13/29.5)*xextent2),
                             xmax = c((xlimit[1] - (28.5/29.5)*(xextent2)) + (6.6/29.5)*xextent2,
                                      (xlimit[1] - (28.5/29.5)*(xextent2)) + (13/29.5)*xextent2,
                                      (xlimit[1] - (28.5/29.5)*(xextent2)) + (13/29.5)*xextent2))
  
  a1 <- annot_lims_y %>% left_join(annot_lims_x) %>% filter(NO == "a1")
  a2 <- annot_lims_y %>% left_join(annot_lims_x) %>% filter(NO == "a2")
  a3 <- annot_lims_y %>% left_join(annot_lims_x) %>% filter(NO == "a3")
  
  
  assign("a1", a1, envir = .GlobalEnv)
  assign("a2", a2, envir = .GlobalEnv)
  assign("a3", a3, envir = .GlobalEnv)
  
}

