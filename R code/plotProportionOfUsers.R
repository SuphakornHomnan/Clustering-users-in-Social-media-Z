library(dplyr)
library(ggplot2)

xaxis <- c("Engager", "Blogger", "Influencer", "Novice")
yaxis <- c(318,3,79,1907)
yaxis_pecentage <- c(0.138, 0.001, 0.034, 0.827)

# plot bar chart based on these above data: x= xaxis, y=yaxis_percentage, fill= yaxis, and show the amount of user (% amount of user) of each bar
# yaxis change scale, such as from 0.8 to 80%.
data <- data.frame(xaxis, yaxis, yaxis_pecentage)
data <- data[order(data$yaxis_pecentage), ]

# Plotting with ggplot2
ggplot(data = data, aes(x = factor(xaxis, levels = data$xaxis), y = yaxis_pecentage, fill = yaxis)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(yaxis, "(", yaxis_pecentage*100,"%)")), vjust = -0.3, size = 3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Role", y = "% Role Composition", fill = "Amount") +
  scale_fill_gradient(low = "#3ADAFE", high = "#18018B") +
  scale_y_continuous(labels = scales::percent_format())

# Plotting Rtfl role composition
xaxis <- c("Contributor", "Collaborator", "Leader")
yaxis <- c(1907,321, 79)
yaxis_pecentage <- c(0.827, 0.139, 0.034)

data2 <- data.frame(xaxis, yaxis, yaxis_pecentage)
data2 <- data2[order(data2$yaxis_pecentage), ]

# Plotting with ggplot2
ggplot(data = data2, aes(x = factor(xaxis, levels = data2$xaxis), y = yaxis_pecentage, fill = yaxis)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(yaxis, "(", yaxis_pecentage*100,"%)")), vjust = -0.3, size = 3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Role", y = "% Mapped Role Composition", fill = "Amount") +
  scale_fill_gradient(low = "#3ADAFE", high = "#18018B") +
  scale_y_continuous(labels = scales::percent_format())

