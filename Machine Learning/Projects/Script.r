setwd('/home/jose/Desktop/Mis cosas/Machine Learning Project')
library(tidyverse) ; library(reshape2) ; library(ggthemes)


reviews <- read.csv('amazon_reviews.csv', header = T, sep = ",", dec = ".")
attach(reviews) ; names(reviews)


graph <- reviews %>% 
            group_by(asin) %>% 
            summarize(occurences = n()) %>% 
            arrange(desc(occurences)) 
            

graph <- head(graph, 10)



labels <- c(graph$asin)

count <- c(graph$occurences)

data <- data.frame(labels, count)

new_labels <- c("SanDisk 64GB Memory", "HDMI Amazon Cable", "Chrome -cast HDMI", "Media -bridge HDMI", "Trascend SDHC Card",
    "ErgoFit Head -phones", "DVI HDMI Cable", "USB Apple Cable", "Roku3 Player", "eneloop AA Batteries")

data <- data.frame(labels, new_labels, count)



ggplot(data, aes(x = reorder(new_labels, -count), y = count)) + 
    geom_bar(stat = "identity", fill = "#1663BE") +
    geom_text(aes(label = count), vjust = -1) +
    theme_economist() +
    labs(title = "Top 10 Reviewed Products", x = "", y = "# Reviews") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2)) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 1))



