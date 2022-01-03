setwd("~/arm_barn")
source("header.R")

# import data ----
pitcher_data <- read_rds("b00.rds") %>%
  column_to_rownames("pit_id") %>%
  filter(events >= 8, mean_starter_flag < 1)

# set clustering options ----
k_clusters <- 9

# manipulate data ----
pitcher_data_scaled <- pitcher_data %>%
  scale()

pitcher_data_weighted <- pitcher_data_scaled * splitstackshape::expandRows(cluster_weights,
                                                                           count = nrow(pitcher_data_scaled),
                                                                           count.is.col = FALSE)
rownames(pitcher_data_weighted) <- rownames(pitcher_data_scaled)

# From https://www.statmethods.net/advstats/cluster.html ----

# Ward Hierarchical Clustering
d <- dist(pitcher_data_weighted, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=k_clusters) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=k_clusters, border="red")


# note:  transpose before using
t_data <- pitcher_data_weighted %>%
  t() %>% 
  as.data.frame()
# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(t_data, method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)