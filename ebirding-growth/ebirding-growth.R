require(tidyverse)
require(glue)

cur_unit <- "India"


# summarising number of lists per year
data_growth <- data %>% 
  # complete lists
  filter(ALL.SPECIES.REPORTED == 1) %>% 
  # if state, filter for state
  group_by(YEAR) %>% 
  reframe(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))

write_csv(data_growth,
          glue("{path_growth}growth_rel{currel_month_lab}-{currel_year}_IN.csv"))


# breaks for y axis (different for states)
y_breaks <- seq(0, max(data_growth$TOT.LISTS), length.out = 5) %>% 
  round(digits = -2)

y_breaks_lab <- glue("{round(y_breaks/1000, 1)} K")

y_t1 <- data_growth %>% 
  filter(YEAR == 2014) %>% 
  pull(TOT.LISTS)

x_breaks <- 2000:(currel_year - 1)


# plotting the line graph

dataprog_lists <- ggplot(data_growth, aes(x = YEAR, y = TOT.LISTS)) +
  geom_point(size = 3, colour = "#fcfa53") +
  geom_line(linewidth = 1, colour = "#fcfa53") +
  geom_hline(yintercept = y_t1, linetype = "dotted", colour = "#CCCCCC80") +
  geom_text(aes(x = 2014, y = y_t1, 
                label = glue("{round(y_t1/1000, 1)} K")),
            size = 3, colour = "#CCCCCC", nudge_y = -15000) +
  scale_x_continuous(breaks = x_breaks,
                     # interested in after eBird established in India
                     limits = c(min(x_breaks), max(x_breaks))) +
  scale_y_continuous(breaks = y_breaks,
                     labels = y_breaks_lab) +
  coord_cartesian(clip = "off") +
  labs(x = "Year", y = "Number of lists (in thousands)",
       title = glue("Growth of eBird in {cur_unit}"),
       subtitle = glue("Data until {currel_month_lab} {currel_year}")) +
  theme_void() +
  theme(axis.text.x = element_text(colour = "#CCCCCC", size = 8),
        axis.text.y = element_text(colour = "#CCCCCC", size = 9, hjust = 1,
                                   margin = margin(0, 10, 0, 0)),
        axis.title.y = element_text(angle = 90, vjust = 6, colour = "#CCCCCC"),
        axis.title.x = element_text(colour = "#CCCCCC", vjust = -2),
        
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linetype = "longdash", colour = "#AAAAAA50"),
        
        plot.title = element_text(size = 20, face = "bold", colour = "#CCCCCC",
                                  hjust = -0.15, vjust = 5),
        plot.subtitle = element_text(size = 14, face = "bold", colour = "#AAAAAA50",
                                     hjust = -0.11, vjust = 5),
        plot.margin = unit(c(2, 1, 1, 2), "lines"),
        
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA))
 

ggsave(glue("{path_growth}growth_rel{currel_month_lab}-{currel_year}_IN.png"), 
       dataprog_lists,
       width = 11, height = 7, units = "in", dpi = 300)
