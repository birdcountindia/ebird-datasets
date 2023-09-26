require(tidyverse)
require(glue)


cur_unit <- "India"

# summarising number of lists per year

data_growth <- data %>% 
  # if state, filter for state
  group_by(YEAR) %>% 
  summarise(TOT.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER))

write.csv("ebirding-growth/tables/.csv") ###

# breaks for y axis (different for states)
y_breaks <- seq(0, max(data_growth$TOT.LISTS), length.out = 5) %>% 
  round(digits = -2)

y_breaks_lab <- glue("{y_breaks} K")

# plotting the line graph

dataprog_lists <- ggplot(data_growth, aes(x = YEAR, y = TOT.LISTS/1000)) +
  geom_point(size = 3, colour = "#fcfa53") +
  geom_line(size = 1.5, colour = "#fcfa53") +
  geom_hline(yintercept = 2, linetype = "dotted") +
  scale_x_continuous(breaks = 2014:rel_year,
                     # interested in after eBird established in India
                     limits = c(2014, rel_year)) +
  scale_y_continuous(breaks = y_breaks,
                     labels = y_breaks_lab) +
  labs(x = "Year", y = "Number of lists (in thousands)",
       title = glue("Growth of eBird in {cur_unit}"),
       subtitle = glue("Data until {rel_month_lab} {rel_year}")) +
  theme(axis.text.x = element_text(size = 8),
        axis.text = element_text(colour = "#E0E0E0"),
        axis.title = element_text(colour = "#CCCCCC"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20, face = "bold", colour = "#CCCCCC", hjust = -0.08),
        plot.margin = unit(c(1.5, 1, 0.5, 1), "lines"),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA))
 
###
ggsave("ebirding-growth/graphs/.png", dataprog_lists,
       width = 11, height = 7, units = "in", dpi = 300)
