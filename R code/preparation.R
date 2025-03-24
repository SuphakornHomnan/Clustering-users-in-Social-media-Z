library(dplyr)
library(ggplot2)
library('corrplot')

socialMedia <- read.csv("Data_DMML.csv")

library('Hmisc')

# Create the describe object
desc <- describe(socialMedia)




# write correlation matrix of socialMedia dataframe and write the results to table.
correlationMatrix <- cor(socialMedia)
# 5 Features with highest correlation, are selected to do feature selection as K-mean algorithm.
desc$InDegree
desc$OutDegree
desc$TotalPosts
desc$MeanPostsPerSubForum
desc$PercBiNeighbours

corrplot(correlationMatrix, method = 'color', type = 'lower', 
         tl.col = "black", tl.srt = 90, tl.cex = 0.25, 
         addCoef.col = "black", number.cex = 0.3)
write.csv(correlationMatrix, file = "correlationMatrix.csv")

# one-way anova test
library(ggplot2)
xlist = c("InDegree", "InDegree", "InDegree", "InDegree", "OutDegree", "OutDegree", "OutDegree", "TotalPosts")
ylist = c("OutDegree", "TotalPosts", "MeanPostsPerSubForum", "PercBiNeighbours", "TotalPosts", "MeanPostsPerSubForum", "PercBiNeighbours", "MeanPostsPerSubForum")

ggplot(socialMedia, aes(x= InDegree, y= OutDegree, color= OutDegree)) +
  geom_point()

ggplot(socialMedia, aes(x= InDegree, y= TotalPosts, color= TotalPosts)) +
  geom_point()

ggplot(socialMedia, aes(x= InDegree, y= MeanPostsPerSubForum, color= MeanPostsPerSubForum)) +
  geom_point()

ggplot(socialMedia, aes(x= InDegree, y= PercBiNeighbours, color= PercBiNeighbours)) +
  geom_point()

ggplot(socialMedia, aes(x= OutDegree, y= TotalPosts, color= TotalPosts)) +
  geom_point()

ggplot(socialMedia, aes(x= OutDegree, y= MeanPostsPerSubForum, color= MeanPostsPerSubForum)) +
  geom_point()

ggplot(socialMedia, aes(x= OutDegree, y= PercBiNeighbours, color= PercBiNeighbours)) +
  geom_point()

ggplot(socialMedia, aes(x= TotalPosts, y= MeanPostsPerSubForum, color= MeanPostsPerSubForum)) +
  geom_point()

