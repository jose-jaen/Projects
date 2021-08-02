## Setting up the working directory where the data is stored

setwd('/home/jose/Desktop/Mis cosas/Machine Learning Project')

## Firing up the relevant packages for data exploration and visualization

library(tidyverse) ; library(reshape2) ; library(ggthemes)

## Reading the data from the CSV file created in Python

reviews <- read.csv('amazon_reviews.csv', header = T, sep = ",", dec = ".")
attach(reviews) ; names(reviews)

## Grouping the data to show reviewed products in descending order with dplyr

reviewed_products <- reviews %>% 
            group_by(asin) %>% 
            summarize(occurences = n()) %>% 
            arrange(desc(occurences)) 

## Filtering top 10 products

top_reviewed_products <- head(reviewed_products, 10)

## Reduced data frame with targeted features

labels <- c(top_reviewed_products$asin)

count <- c(top_reviewed_products$occurences)

## Getting the products' name

new_labels <- c("SanDisk 64GB Memory", "HDMI Amazon Cable", "Chrome -cast HDMI", "Media -bridge HDMI", "Trascend SDHC Card",
    "ErgoFit Head -phones", "DVI HDMI Cable", "USB Apple Cable", "Roku3 Player", "eneloop AA Batteries")

top_reviews_df <- data.frame(labels, new_labels, count)

## Barplot of top 10 reviewed products

top_reviews <- ggplot(top_reviews_df, aes(x = reorder(new_labels, -count), y = count)) + 
    geom_bar(stat = "identity", fill = "#1663BE") +
    geom_text(aes(label = count), vjust = -1, size = 4.2) +
    theme_economist() +
    labs(title = "Top 10 Reviewed Products", x = "", y = "# Reviews") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2)) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 1))

top_reviews

## Let's check the average rating of these products to see if they are popular or notorious

ratings <- reviews %>% 
            select(asin, overall) %>% 
            group_by(asin) %>% 
            summarize(occurences = n(), avg_rating = mean(overall)) %>% 
            arrange(desc(occurences)) 

top_ratings <- head(ratings, 10)

top_ratings_df <- data.frame(top_ratings)


## Plotting the average rating of the top 10 reviewed products

top_ratings <- ggplot(top_ratings_df, aes(x = reorder(new_labels, -occurences), y = avg_rating)) + 
    geom_bar(stat = "identity", fill = "#1663BE") +
    geom_text(aes(label = round(avg_rating, 2)), vjust = -1, size = 4.2) +
    theme_economist() +
    geom_hline(yintercept = 3, color = "red") +
    labs(title = "Top 10 Products Average Rating", x = "", y = "Rating") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2)) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 1))

top_ratings


new_reviews <- reviews %>% 
                select(asin, overall) %>% 
                group_by(asin) %>% 
                summarize(occurences = n(), avg_rating = mean(overall)) %>% 
                mutate (
                    sentiment = case_when(
                    avg_rating < 4 ~ '-1',
                    avg_rating >= 4 ~ '1'
                    )
                )

head(new_reviews, 10)


new_reviews2 <- reviews %>% 
                select(asin, overall) %>% 
                group_by(asin) %>% 
                summarize(occurences = n(), avg_rating = mean(overall)) %>% 
                mutate (
                    sentiment = case_when(
                    avg_rating <= 2 ~ 'Very Negative',
                    avg_rating > 2 & avg_rating < 4 ~ 'Negative',
                    avg_rating >= 4 ~ 'Positive'
                    )
                ) 





ggplot(data = new_reviews, aes(x = avg_rating, color = sentiment, fill = sentiment)) +
geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) + geom_density(alpha = 0.6) +
scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
labs(title = "Rating distribution", x = "Average Rating", y = "Density") + theme_economist() +
geom_vline(data = new_reviews, aes(xintercept = occurences, color = sentiment), linetype = "dashed") 


ggplot(data = new_reviews2, aes(x = avg_rating, color = sentiment, fill = sentiment)) +
geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) + geom_density(alpha = 0.6) +
scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
labs(title = "Rating distribution", x = "Average Rating", y = "Density") + theme_economist() 



ggplot(data = new_reviews, aes(x = avg_rating, y = occurences, fill = sentiment, color = sentiment)) + geom_point()











ggplot(data = new_reviews2, aes(x = avg_rating)) +
geom_histogram(aes(..scaled..), position = "identity", alpha = 0.5) + geom_density(alpha = 0.6) +
##scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
##scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
labs(title = "Rating distribution", x = "Average Rating", y = "Density") + theme_economist()

ggplot(new_reviews2, aes(avg_rating, ..scaled.., fill = sentiment)) +
    geom_density() +
    ggtitle("Scaled")
