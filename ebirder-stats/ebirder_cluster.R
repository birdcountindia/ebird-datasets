library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(purrr)

library(factoextra)

# column names
col_names <- c("OBSERVER.ID", "EBIRD.FAMILIARITY", "EBIRD.DATA.QUAL", "BIRD.KNOW", 
               "IP.COMM.SKILL", "COACH.WILL", "COMMENTS", "DURATION")

# ebird user info
eBird_users <- read.delim("EBD/ebd_users_relMay-2022.txt", sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))


# importing interview data
for_cluster <- list(A = read_excel(path = "ebirder-stats/for_ebirder_cluster.xlsx", sheet = 1),
                    B = read_excel(path = "ebirder-stats/for_ebirder_cluster.xlsx", sheet = 2),
                    C = read_excel(path = "ebirder-stats/for_ebirder_cluster.xlsx", sheet = 3),
                    D = read_excel(path = "ebirder-stats/for_ebirder_cluster.xlsx", sheet = 4),
                    E = read_excel(path = "ebirder-stats/for_ebirder_cluster.xlsx", sheet = 5)) %>% 
  # renaming columns
  purrr::map(~ magrittr::set_colnames(.x, col_names)) %>% 
  # removing columns not required for clustering
  purrr::map(~ select(.x, -COMMENTS, -DURATION)) %>% 
  # combining data from different sheets
  bind_rows() %>% 
  # adding names
  left_join(eBird_users) %>% 
  # removing NAs (ALL columns in row need to be non-NA)
  filter(if_all(everything(), ~ !is.na(.x)))

cluster_data <- for_cluster %>% 
  select(-OBSERVER.ID) %>% 
  column_to_rownames("FULL.NAME")


# no need to scale the data because all variables on same scale (ordinal 1-5)

# checking for correlation between variables
var_cor <- cor(cluster_data, method = "spearman")


# distance object (default: Euclidean)
dist <- cluster_data %>% get_dist()


# calculating k
fviz_nbclust(cluster_data, FUNcluster = kmeans, method = "wss") +
  labs(subtitle = "Elbow method, within sum of squares")
# either 4 or 6


# clustering
cluster_out <- kmeans(cluster_data, centers = 6, nstart = 9999)
print(cluster_out)


# visualising clusters
fviz_cluster(list(data = cluster_data, cluster = cluster_out$cluster))



cluster_result <- cluster_out$cluster %>% 
  as.data.frame() %>% 
  magrittr::set_colnames("CLUSTER") %>% 
  rownames_to_column("FULL.NAME") %>% 
  right_join(for_cluster) %>% 
  arrange(CLUSTER, desc(EBIRD.FAMILIARITY), desc(EBIRD.DATA.QUAL), desc(BIRD.KNOW), 
          desc(IP.COMM.SKILL), desc(COACH.WILL))

cluster_type <- cluster_out$centers %>% 
  as.data.frame() %>% 
  rownames_to_column("CLUSTER") %>% 
  arrange(desc(EBIRD.FAMILIARITY), desc(EBIRD.DATA.QUAL), desc(BIRD.KNOW), 
          desc(IP.COMM.SKILL), desc(COACH.WILL))

cluster_type_long <- cluster_type %>% 
  pivot_longer(2:6, names_to = "VARIABLE", values_to = "SCORE") %>% 
  mutate(VARIABLE = factor(VARIABLE, 
                           levels = c("EBIRD.FAMILIARITY", "EBIRD.DATA.QUAL", "BIRD.KNOW", 
                                      "IP.COMM.SKILL", "COACH.WILL")),
         VARIABLE.NUM = as.numeric(VARIABLE))

# saving into excel sheet
write_xlsx(x = list("Results" = cluster_result, "Cluster means" = cluster_type),
           path = "ebirder-stats/ebirder_cluster_results.xlsx")


# Parallel Coordinates Plot 
# (ref https://openclassrooms.com/en/courses/5869986-perform-an-exploratory-data-analysis/6177861-analyze-the-results-of-a-k-means-clustering)
par_coord_plot <- ggplot(cluster_type_long, aes(VARIABLE.NUM, SCORE, colour = CLUSTER)) +
  geom_line(size = 1.1) +
  scale_x_continuous(name = "Variable", labels = unique(cluster_type_long$VARIABLE)) +
  scale_y_continuous(name = "Score") +
  scale_color_discrete(name = "Cluster") +
  theme_classic()

ggsave("ebirder-stats/ebirder_cluster_plot.png", par_coord_plot,
       width = 11, height = 6, units = "in", dpi = 300)
