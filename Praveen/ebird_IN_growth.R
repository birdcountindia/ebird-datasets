# from hist_spread.R

grid_cov_timeline <- grid_cov_timeline %>% 
  filter(RESOLUTION == 2)


dataprog_lists <- ggplot(grid_cov_timeline, 
                         aes(x = MEDIAN.YEAR, y = TOT.LISTS/1000)) +
  geom_point(size = 2.5, colour = "#fcfa53") +
  geom_line(size = 1, colour = "#fcfa53") +
  geom_hline(yintercept = 2, linetype = "dotted") +
  scale_x_continuous(breaks = grid_cov_timeline$MEDIAN.YEAR,
                     labels = grid_cov_timeline$TIME.SOIB2,
                     limits = c(1990, 2021)) +
  scale_y_continuous(breaks = seq(0, 500, 50),
                     labels = paste(seq(0, 500, 50), "K")) +
  labs(x = "Time period", y = "Number of lists (in thousands)",
       title = "Growth of eBird in India",
       subtitle = " ") +
  theme(axis.text.x = element_text(size = 8),
        axis.text = element_text(colour = "#E0E0E0"),
        axis.title = element_text(colour = "#CCCCCC"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20, face = "bold", colour = "#CCCCCC", hjust = -0.08),
        plot.margin = unit(c(1.5, 1, 0.5, 1), "lines"),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.background = element_rect(fill = "black", colour = NA))

ggsave("hist_spread/figs/dataprog_lists_PJ.png", dataprog_lists,
       width = 11, height = 6, units = "in", dpi = 300)
