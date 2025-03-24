library('factoextra')
library(ggplot2)
df <- scale(socialMedia)

selected_features <- c("InDegree", "OutDegree", "TotalPosts", "MeanPostsPerSubForum", "PercBiNeighbours")
df <- scale(socialMedia[, selected_features])
# Visualize the optimal number of clusters
fviz_nbclust(df, FUNcluster = kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  geom_vline(xintercept = 7, linetype = 2)
