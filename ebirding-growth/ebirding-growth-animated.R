require(tidyverse)
require(glue)
require(magick)
library(gganimate)

source("ebirding-growth/ebirding-growth-functions.R")

gdrive_folder_id <- "15qe4oLGpOGIrhRvqPcZs2QqoFeNzxrBI"
gdrive_folder_id_st.ut <- "128OFmHUltlrtyLZyy8SqlUv0HhtVb3ya"

#I think we might need to change the year from 2024 to 2025 inside this 
#function once we enter next year
growth_gif <- function(data, scale = "country", cur_state = NULL) {
  
  # plot settings
  
  x_breaks <- 2000:currel_year
  
  # breaks for y axis (different for states)
  y_breaks <- seq(0, max(data$TOT.LISTS), length.out = 5) %>% round(digits = -2)
  y_breaks_lab <- glue("{round(y_breaks/1000, 1)} K")
  y_breaks_gap <- y_breaks[5] - y_breaks[4]
  
  y_t1 <- data %>% 
    filter(YEAR == 2014) %>% 
    pull(TOT.LISTS)
  
  text_data <- tibble(
    x_pos = 2014,
    y_pos = y_t1,
    text_label = glue("({round(y_t1/1000, 1)} K)")
  )
  
  # colours
  col_text_main <- "#CCCCCC"
  col_text_subtle <- "#AAAAAA50"
  col_guide_pale <- "#CCCCCC80"
  col_light_bright <- "#fcfa53"
  col_light_subtle <- "#fcfa3099"
  
  # text
  cur_unit <- if (scale == "country") "India" else if (scale == "state") cur_state
  plot_title <- glue("eBirding in {cur_unit} over time")
  plot_subtitle <- glue("Data until {currel_month_lab} {currel_year}")
  
  # logo
  bci_logo <- image_convert(image_read("bcilogo-framed.png"), matte = T)
  logo_ymin <- max(y_breaks) + round(y_breaks_gap/2, -2)
  logo_ymax <- logo_ymin + round(y_breaks_gap*0.6, -2)
  logo_xmin <- currel_year-4
  logo_xmax <- currel_year
  
  # plotting the line graph
  
  # separating current year 
  data <- data %>% 
    mutate(CUR.YEAR.LISTS = case_when(YEAR == currel_year ~ TOT.LISTS,
                                      TRUE ~ NA),
           TOT.LISTS = case_when(YEAR != currel_year ~ TOT.LISTS,
                                 TRUE ~ NA))
  
  our_plot <- ggplot(data, aes(x = YEAR, y = TOT.LISTS)) +
    geom_point(data = . %>% filter(YEAR <= 2024), 
               size = 3, colour = col_light_bright) +
    
    geom_point(mapping = aes(x = YEAR, y = CUR.YEAR.LISTS), 
               size = 3, shape = 24, colour = col_light_bright, fill = col_light_subtle) +
    
    geom_line(data = . %>% filter(YEAR <= 2024), 
              linewidth = 1, colour = col_light_bright) +
    
    geom_vline(xintercept = 2014, linetype = "dotted", colour = col_guide_pale) +
    geom_text(data = text_data,
              mapping = aes(x = x_pos, y = y_pos, label = text_label),
              size = 3, 
              colour = col_text_main, 
              nudge_x = 0.7,
              lineheight = 0.9,
              inherit.aes = FALSE) +
    
    scale_x_continuous(breaks = x_breaks,
                       # interested in after eBird established in India
                       limits = c(min(x_breaks), max(x_breaks))) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_breaks_lab) +
    coord_cartesian(clip = "off") +
    labs(x = "Year", y = "Number of complete lists per year",
         title = plot_title, subtitle = plot_subtitle) +
    theme_void() +
    theme(axis.text.x = element_text(colour = col_text_main, size = 8),
          axis.text.y = element_text(colour = col_text_main, size = 9, hjust = 1,
                                     margin = margin(0, 10, 0, 0)),
          axis.title.y = element_text(angle = 90, vjust = 6, colour = col_text_main),
          axis.title.x = element_text(colour = col_text_main, vjust = -2),
          
          axis.ticks = element_blank(),
          panel.grid.major.y = element_line(linetype = "longdash", colour = col_text_subtle),
          
          plot.title = element_text(size = 20, face = "bold", colour = col_text_main,
                                    hjust = -0.15, vjust = 5),
          plot.subtitle = element_text(size = 14, face = "bold", colour = col_text_subtle,
                                       hjust = -0.11, vjust = 5),
          plot.margin = unit(c(2, 1, 1, 2), "lines"),
          
          plot.background = element_rect(fill = "black", colour = NA),
          panel.background = element_rect(fill = "black", colour = NA)) 
  # add BCI logo
  # annotation_raster(bci_logo, 
  #                   ymin = logo_ymin, ymax = logo_ymax,
  #                   xmin = logo_xmin, xmax = logo_xmax) +
  # geom_image(
  #   data = tibble(YEAR = 2022, TOT.LISTS = logo_ymin),
  #   aes(image = "bcilogo-framed.png"),
  #   size = 0.25
  # )
  
  return(our_plot)
  
} #required function
# growth graph for India --------------------------------------------------

path_csv <- glue("{path_growth}Anim/growth_rel{currel_month_lab}-{currel_year}_IN_anim.csv")
path_gif <- glue("{path_growth}Anim/growth_rel{currel_month_lab}-{currel_year}_IN_anim.gif")

# summarising number of complete checklists per year
data_growth <- growth_stats(data, "country")
write_csv(data_growth, file = path_csv)
rm(data, data_sed)

plot_growth <- data_growth %>% growth_gif("country")
plot_growth$data <- plot_growth$data %>% 
  filter(YEAR >= 2000)

plot_growth_Anim <- plot_growth + 
  transition_reveal(YEAR) +
  view_follow(fixed_y = TRUE)

#for animating and overlaying logo
anim_frames <- animate(plot_growth_Anim, width = 11, height = 7, units = "in", fps = 25, 
                       duration = 15, end_pause = 100, res = 100,
                       renderer = magick_renderer())
logo_img <- magick::image_read("bcilogo-framed.png")
logo_img <- magick:: image_scale(logo_img, "150x")
anim_with_logo <- magick::image_composite(anim_frames, logo_img, 
                                          gravity = "northeast",
                                          offset = "+10+10")
magick::image_write(anim_with_logo, path = path_gif)

# upload to GDrive

# "put" overwrites/updates existing file whereas "upload" creates new files each time
drive_put(path_gif, 
          path = as_id(gdrive_folder_id),
          name = glue("{str_pad(0, width=2, pad='0')} India_anim.gif"))


# growth gifs for States and UTs------------------------------------

path_csv <- glue("{path_growth}/Anim/growth_rel{currel_month_lab}-{currel_year}_ST_Anim.csv")

# summarising number of complete checklists per year
data_growth <- growth_stats(data, "state")
write_csv(data_growth, file = path_csv)
data_growth <- read.csv(path_csv)

iwalk(unique(state_info$STATE.CODE), ~ {
  
  cur_state_info <- state_info %>% filter(STATE.CODE == .x)  
  cur_state <- cur_state_info$STATE
  cur_data <- data_growth %>% filter(STATE == cur_state)  
  cur_gdrive_path <- gdrive_paths(state_info) %>% filter(STATE == cur_state) %>% pull(PATH)
  

  path_gif <- glue("{path_growth}/Anim/growth_rel{currel_month_lab}-{currel_year}_{.x}_Anim.gif")

  plot_growth <- cur_data %>% growth_gif("state", cur_state = cur_state)
  plot_growth$data <- plot_growth$data %>% 
    filter(YEAR >= 2000)
  
  plot_growth_Anim <- plot_growth + 
    transition_reveal(YEAR) +
    view_follow(fixed_y = TRUE)
  
  #for animating and overlaying the logo
  anim_frames <- animate(plot_growth_Anim, width = 11, height = 7, units = "in", fps = 25, 
                         duration = 15, end_pause = 100, res = 100,
                         renderer = magick_renderer())
  logo_img <- magick::image_read("bcilogo-framed.png")
  logo_img <- magick:: image_scale(logo_img, "150x")
  anim_with_logo <- magick::image_composite(anim_frames, logo_img, 
                                            gravity = "northeast",
                                            offset = "+10+10")
  magick::image_write(anim_with_logo, path = path_gif)
  
  
  # upload to GDrive
  # "put" overwrites/updates existing file whereas "upload" creates new files each time
  drive_put(path_gif, 
            path = as_id(gdrive_folder_id_st.ut),
            name = glue("{str_pad(.y, width=2, pad='0')} {cur_state}_Anim.gif"))

})



