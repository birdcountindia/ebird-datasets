load("EBD/ebd_IN_relJun-2022.RData")
data <- data %>% slice_sample(n = 10000000)

cur_state <- unique(state_info$STATE)[18]

coveragemappath1 <- filter(coveragepaths_st, STATE == cur_state)$MAP1

data_cov_state <- filter(data_cov, STATE == cur_state)


map_cov_logo <- image_convert(image_read("bcilogo-framed.png"), matte = T)

map_cov_text <- glue::glue("{str_to_upper(cur_state)}
                             
                             {label_comma()(data_cov_state$LOCATIONS)} locations
                             {label_comma()(data_cov_state$LISTS)} lists
                             {label_comma()(data_cov_state$HOURS)} hours
                             {label_comma()(data_cov_state$PEOPLE)} people
                             
                             {label_comma()(data_cov_state$DISTRICTS)} districts
                             
                             {label_comma()(data_cov_state$SPECIES)} species
                             {label_comma()(data_cov_state$OBSERVATIONS)} observations")

map_cov_footer <- glue::glue("Data until {rel_month_lab} {rel_year}")


data_loc <- data %>% filter(STATE == cur_state) %>% distinct(LONGITUDE, LATITUDE)

cur_bbox <- data_loc %>% st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% st_bbox()
plot_lims()

annot_lims()

### map with annotations of stats and BCI logo ###
map_cov_annot <- ggplot() +
  geom_sf(data = filter(statemap_sf, STATE == cur_state),
          colour = NA, fill = "black") +
  geom_point(data = data_loc, aes(x = LONGITUDE, y = LATITUDE),
             colour = "#fcfa53", size = 0.05, stroke = 0) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(clip = "off", xlim = xlimit, ylim = ylimit) +
  theme(plot.margin = unit(c(0,0,0,24), "lines")) +
  annotation_raster(map_cov_logo,
                    ymin = a1$ymin, ymax = a1$ymax,
                    xmin = a1$xmin, xmax = a1$xmax) +
  annotation_custom(textGrob(label = map_cov_text,
                             hjust = 0,
                             gp = gpar(col = "#FCFA53", cex = 1.0)),
                    ymin = a2$ymin, ymax = a2$ymax,
                    xmin = a2$xmin, xmax = a2$xmax) +
  annotation_custom(textGrob(label = map_cov_footer,
                             hjust = 0,
                             gp = gpar(col = "#D2D5DA", cex = 0.7)),
                    ymin = a3$ymin, ymax = a3$ymax,
                    xmin = a3$xmin, xmax = a3$xmax) 
  
ggsave(coveragemappath1, map_cov_annot,
       # use png(), not ragg::agg_png() which does anti-aliasing, removing crispness of points
       device = png,
       units = "in", width = 10, height = 7, bg = "black", dpi = 300)

