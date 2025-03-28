require(lubridate)
require(tidyverse)
require(magick)
require(scales) # for comma format of numbers
require(grid)
require(glue)
require(sf)

get_percentile_indices <- function(data, value, perc) {
  low = (1 - perc/100)/2
  high = 1 - (1 - perc/100)/2
  # Step 1: Find the indices where the value appears in the data
  indices <- which(data == value)
  
  # Step 2: Calculate the number of values (length) and the 95% percentile range
  n <- length(indices)
  lower_index <- floor(low * n) + 1  # 2.5% from the start (lower bound)
  upper_index <- ceiling(high * n)    # 97.5% from the start (upper bound)
  
  # Step 3: Return the indices that cover the 95% range
  lower_bound <- indices[lower_index]
  upper_bound <- indices[upper_index]
  
  return(list(lower_bound = lower_bound, upper_bound = upper_bound))
}

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

path = "EBD/ebd_IN_relSep-2024.RData"
load(path)
data0 = data

# order sampling event identifiers in ascending order

data = data0 %>%
  dplyr::select(YEAR,OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER,LONGITUDE,LATITUDE) %>%
  mutate(
    first_letter = sub("^(.)", "\\1", SAMPLING.EVENT.IDENTIFIER),  # Extract the first letter
    sampling_number = sub("^.(.*)", "\\1", SAMPLING.EVENT.IDENTIFIER)  # Extract the rest 
  ) %>%
  mutate(sampling_number = as.numeric(sampling_number)) %>%
  arrange(sampling_number) %>%
  mutate(submit_year = 2013)

upper_limit = length(data$submit_year)
for (i in 2024:2014)
{
  lower_limit = get_percentile_indices(data$YEAR, i, 99)$lower_bound
  data$submit_year[lower_limit:upper_limit] = i
  upper_limit = lower_limit - 1
}


data_loc_main <- data %>% distinct(submit_year,LONGITUDE, LATITUDE)
cur_bbox <- data_loc_main %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% st_bbox()
plot_lims()




for (i in 2013:2024)
{
  data_loc <- data_loc_main %>%
    filter(submit_year <= i) %>%
    distinct(LONGITUDE, LATITUDE)
  
  map_cov_plain <- ggplot() +
    #geom_sf(data = india_sf, colour = NA, fill = "black") +
    geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE), 
               colour = "#fcfa53", size = 0.05, stroke = 0) +
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
          plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "lines"),
          plot.background = element_rect(fill = "black", colour = NA),
          panel.background = element_rect(fill = "black", colour = NA),
          plot.title = element_text(hjust = 0.5)) +
    coord_sf(clip = "off", xlim = xlimit, ylim = ylimit) +
    annotate("text", 
             x = Inf, y = Inf, 
             label = i, 
             color = "#fcfa53", 
             size = 5, 
             hjust = 1, vjust = 1)
  
  name = paste("indiamap",i,".png",sep="")
  
  ggsave(name, map_cov_plain, 
         # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
         device = png,
         units = "in", width = 8, height = 8, bg = "black", dpi = 300)
}

