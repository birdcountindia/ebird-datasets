require(tidyverse)
require(glue)
require(magick)

source("ebirding-growth/ebirding-growth-functions.R")


# growth graph for India --------------------------------------------------

path_csv <- glue("{path_growth}growth_rel{currel_month_lab}-{currel_year}_IN.csv")
path_png <- glue("{path_growth}growth_rel{currel_month_lab}-{currel_year}_IN.png")

# summarising number of complete checklists per year
data_growth <- growth_stats(data, "country")
write_csv(data_growth, file = path_csv)

plot_growth <- data_growth %>% growth_plot("country")
ggsave(plot_growth, file = path_png, 
       width = 11, height = 7, units = "in", dpi = 300)

# add logo to plot
add_logo(path_png, "bcilogo-framed.png", "top right") %>% 
  magick::image_write(path = path_png)


# upload to GDrive

# "put" overwrites/updates existing file whereas "upload" creates new files each time
drive_put(path_png, 
          path = as_id("1f4-DVEzsHlvcU5-glg9NxidJf8-iUq1M"),
          name = glue("{str_pad(0, width=2, pad='0')} India.png"))


# growth graph for states -------------------------------------------------

path_csv <- glue("{path_growth}growth_rel{currel_month_lab}-{currel_year}_ST.csv")

# summarising number of complete checklists per year
data_growth <- growth_stats(data, "state")
write_csv(data_growth, file = path_csv)


iwalk(unique(state_info$STATE.CODE), ~ {
  
  cur_state_info <- state_info %>% filter(STATE.CODE == .x)  
  cur_state <- cur_state_info$STATE
  cur_data <- data_growth %>% filter(STATE == cur_state)  
  cur_gdrive_path <- gdrive_paths(state_info) %>% filter(STATE == cur_state) %>% pull(PATH)
  

  path_png <- glue("{path_growth}growth_rel{currel_month_lab}-{currel_year}_{.x}.png")

  plot_growth <- cur_data %>% growth_plot("state", cur_state = cur_state)
  ggsave(plot_growth, file = path_png, 
         width = 11, height = 7, units = "in", dpi = 300)
  
  # add logo to plot
  add_logo(path_png, "bcilogo-framed.png", "top right") %>% 
    magick::image_write(path = path_png)
  
  
  # upload to GDrive
  # "put" overwrites/updates existing file whereas "upload" creates new files each time
  drive_put(path_png, 
            path = as_id(cur_gdrive_path),
            name = glue("{str_pad(.y, width=2, pad='0')} {cur_state}.png"))

})



