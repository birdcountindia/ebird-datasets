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


# saving into excel sheet
write_xlsx(x = list("Results" = cluster_result, "Cluster means" = cluster_type),
           path = "ebirder-stats/ebirder_cluster_results.xlsx")
