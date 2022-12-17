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
  
  if (xextent1 > yextent1) {
    
    ext_ratio <- yextent1/xextent1
    
    ymid <- (ylimit[1] + ylimit[2])/2
    
    a1 <- data.frame(xmin = xlimit[1] - (28.5/29.5)*(xextent2),
                     xmax = (xlimit[1] - (28.5/29.5)*(xextent2)) + (6.6/29.5)*xextent2,
                     ymin = 25.5,
                     ymax = 23.8) 
    
    # the text in the box, due to hjust = 0, will start from middle of x axis limits
    # hence need to adjust xmin accordingly, so that the logo and this text line up
    a2 <- data.frame(xmin = a1$xmin - (13/29.5)*xextent2,
                     xmax = a1$xmin + (13/29.5)*xextent2,
                     ymin = a1$ymin + (12.64/31.5)*yextent2, # diff. in ymins of logo and text is 14 originally
                     ymax = (a1$ymin + (12.64/31.5)*yextent2) + ((13/29.5)*xextent2)/1.08) # 1.08 is aspect ratio 
    
    a3 <- data.frame(xmin = a1$xmin - (13/29.5)*xextent2,
                     xmax = a1$xmin + (13/29.5)*xextent2,
                     ymin = a1$ymin + (9.24/31.25)*yextent2, # diff. in ymins of logo and text is 10 originally
                     ymax = (a1$ymin + (9.24/31.25)*yextent2) + ((13/29.5)*xextent2)/13) # 13 is aspect ratio 
    
  } else if (yextent1 > xextent1) {
    
    a1 <- data.frame(xmin = xlimit[1] - (28.5/29.5)*(xextent2),
                     xmax = (xlimit[1] - (28.5/29.5)*(xextent2)) + (6.6/29.5)*xextent2,
                     ymin = ylimit[1] - 0.15*yextent1,
                     ymax = (ylimit[1] - 0.15*yextent1) + ((8.5/29.5)*xextent2)/3.52) # 3.52 is aspect ratio for logo
    
    # the text in the box, due to hjust = 0, will start from middle of x axis limits
    # hence need to adjust xmin accordingly, so that the logo and this text line up
    a2 <- data.frame(xmin = a1$xmin - (13/29.5)*xextent2,
                     xmax = a1$xmin + (13/29.5)*xextent2,
                     ymin = a1$ymin + (14/31.25)*yextent1, # diff. in ymins of logo and text is 14 originally
                     ymax = (a1$ymin + (14/31.25)*yextent1) + ((13/29.5)*xextent2)/1.08) # 1.08 is aspect ratio 
    
    a3 <- data.frame(xmin = a1$xmin - (13/29.5)*xextent2,
                     xmax = a1$xmin + (13/29.5)*xextent2,
                     ymin = a1$ymin + (10/31.25)*yextent1, # diff. in ymins of logo and text is 10 originally
                     ymax = (a1$ymin + (10/31.25)*yextent1) + ((13/29.5)*xextent2)/13) # 13 is aspect ratio 
    
  }
  
  assign("a1", a1, envir = .GlobalEnv)
  assign("a2", a2, envir = .GlobalEnv)
  assign("a3", a3, envir = .GlobalEnv)
  
}

