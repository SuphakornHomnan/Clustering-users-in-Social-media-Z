library('factoextra')
library(ggplot2)
library(dplyr)

selected_features <- c("InDegree", "OutDegree", "TotalPosts", "MeanPostsPerSubForum") # "MeanPostsPerSubForum", "PercBiNeighbours"

social_media_selected_features <- socialMedia[, selected_features]
df <- scale(social_media_selected_features)

optimal_k <- 4
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(social_media_selected_features, centers = optimal_k, nstart = 20)

# Evaluate the clustering model
# lets run a quick K-means algo.
km.res <- eclust(df, "kmeans", k = optimal_k,
                 nstart = 25, graph = FALSE)

library('cluster')
#running the silhouette analysis 
sil <- silhouette(km.res$cluster, dist(df))
summary(sil)

fviz_silhouette(sil)

# plot cluster of soical media data
#you should read more about the different functions and ways to plot -- use ?fviz_cluster() in your console
fviz_cluster(km.res, df, 
             ellipse.type = "convex",
             geom=c("point"), 
             palette = "jco", 
             ggtheme = theme_classic()) #you can change the color palette and theme to your preferences

# mutation cluster attribute to socialMedia data
socialMedia_with_cluster <- cbind(socialMedia, cluster = km.res$cluster)
#socialMedia <- cbind(socialMedia, cluster = km.res$cluster)

library('dplyr')
library(writexl)
summary_data <- socialMedia %>%
  group_by(cluster) %>%
  summarise_all(funs(mean = mean(., na.rm = TRUE), min = min(., na.rm = TRUE), max = max(., na.rm = TRUE))) %>%
  arrange(cluster)


# Export the summarized data to an Excel file
write_xlsx(summary_data, "summary_data.xlsx")
