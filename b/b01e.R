setwd("~/GitHub/arm_barn")
source("header.R")

# import data ----
pitcher_data <- read_rds("export/b/b00.rds") %>%
  column_to_rownames("pit_id") %>%
  filter(events >= 8)

# set clustering options ----
k_clusters <- 5

# manipulate data ----
pitcher_data_scaled <- pitcher_data %>%
  scale()

pitcher_data_weighted <- pitcher_data_scaled * splitstackshape::expandRows(cluster_weights,
                                                                           count = nrow(pitcher_data_scaled),
                                                                           count.is.col = FALSE)
rownames(pitcher_data_weighted) <- rownames(pitcher_data_scaled)

# run clustering function ----
fit <- kmeans(pitcher_data_weighted, k_clusters)

# get cluster means
aggregate(pitcher_data_weighted,by=list(fit$cluster),FUN=mean)
# append cluster assignment
pitcher_data_weighted <- data.frame(pitcher_data_weighted, fit$cluster)

pitcher_data <- pitcher_data %>%
  rownames_to_column(var = "pit_id") %>%
  left_join(pitcher_data_weighted %>%
              rownames_to_column(var = "pit_id") %>%
              select(pit_id, fit.cluster)) %>%
  select(fit.cluster, everything())

write_rds(pitcher_data, "export/b/b01e.rds")
