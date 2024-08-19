# calculate growth stats --------------------------------------------------

growth_stats <- function(data, scale = "country") {
  
  stats <- data %>% 
    # complete lists
    filter(ALL.SPECIES.REPORTED == 1) %>% 
    {if (scale == "country") {
      group_by(., YEAR) %>% 
        reframe(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))
    } else if (scale == "state") {
      group_by(., STATE, YEAR) %>% 
        reframe(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
        group_by(STATE)
    }} %>% 
    complete(YEAR = 2000:currel_year, 
             fill = list(TOT.LISTS = 0)) %>% 
    {if (scale == "country") {
      arrange(., YEAR)
    } else if (scale == "state") {
      arrange(., STATE, YEAR)
    }} %>% 
    mutate(CUM.LISTS = cumsum(TOT.LISTS)) %>% 
    ungroup()
  
  return(stats)
  
}

# plot growth graph -------------------------------------------------------

growth_plot <- function(data, scale = "country", cur_state = NULL) {

  # plot settings
  
  x_breaks <- 2000:currel_year
  
  # breaks for y axis (different for states)
  y_breaks <- seq(0, max(data$TOT.LISTS), length.out = 5) %>% round(digits = -2)
  y_breaks_lab <- glue("{round(y_breaks/1000, 1)} K")
  y_breaks_gap <- y_breaks[5] - y_breaks[4]
  
  y_t1 <- data %>% 
    filter(YEAR == 2014) %>% 
    pull(TOT.LISTS)
  
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
    geom_point(size = 3, colour = col_light_bright) +
    # current year point
    geom_point(mapping = aes(x = YEAR, y = CUR.YEAR.LISTS), 
               size = 3, shape = 24, colour = col_light_bright, fill = col_light_subtle) +
    geom_line(linewidth = 1, colour = col_light_bright) +
    geom_vline(xintercept = 2014, linetype = "dotted", colour = col_guide_pale) +
    geom_text(aes(x = 2014, y = y_t1, 
                  label = glue("{round(y_t1/1000, 1)} K")),
              size = 3, colour = col_text_main, nudge_x = 0.7) +
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
  
}

# add logo to plot --------------------------------------------------------

# from https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

# get GDrive paths --------------------------------------------------------

# structuring all states into GBBC regions (different directories in the drive)
gdrive_paths <- function(data_state_info) {
  
  data_state_info %>% 
    distinct(STATE, STATE.CODE) %>% 
    mutate(PATH = case_when(
      # islands
      STATE.CODE %in% c("LD", "AN") ~ "1cHJw5Tvh-M55OSLHt6dFySd8ZJSpgzxS",
      # himalaya
      STATE.CODE %in% c("LA", "UL", "JK", "HP") ~ "1YdbZgQRQIoPj4TujishwV2-2ZZWMCwKq",
      # central
      STATE.CODE %in% c("OR", "MH", "MP", "JH", "CT") ~ "1xXMJAZsdg4rBY9ASY-PnLfePz7yKkuIq",
      # east
      STATE.CODE %in% c("WB", "TR", "SK", "ML",
                        "NL", "MZ", "MN", "AS", "AR") ~ "1lY62297twLyi4HmKDmZMl4kXglY1sHPU",
      # west
      STATE.CODE %in% c("RJ", "GJ", "DD", "DN") ~ "1lpBzWFzRGuipFlT9pfFGrveIlp0cN_yk",
      # north
      STATE.CODE %in% c("UP", "PB", "HR", "DL",
                        "CH", "BI") ~ "1TNDSb5YggTDy8Hw4zqtKKaQfzeV2SHg6",
      #south
      STATE.CODE %in% c("GA", "TS", "TN", "PY",
                        "KL", "KA", "AP") ~ "10UGjU01i4o29GExsLlRts9DZY2K0BBjv"
    ))
  
}
