## Setting up the working directory where the data is stored

setwd('/home/jose/Desktop/Mis cosas/Machine Learning Project')

## Firing up the relevant packages for data exploration and visualization

library(tidyverse) ; library(reshape2) ; library(ggthemes) ; library(ggridges)
library(fBasics) ; library(ggExtra) ; library(e1071) ; library(BSDA) ; library(nortest)

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
                    labs(title = "TOP 10 REVIEWED PRODUCTS", x = "", y = "# REVIEWS") +
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
                    theme_economist() + ylim(c(0, 6)) +
                    geom_hline(yintercept = 4, color = "red", size = 1) +
                    labs(title = "TOP 10 PRODUCTS AVERAGE RATING", x = "", y = "RATING") +
                    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2)) +
                    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 1))

top_ratings


## Let us introduce the threshold rule 

reviews_query <- reviews %>% 
                  mutate (
                    sentiment = case_when(
                    overall <= 2 ~ 'Negative',
                    overall > 2 & overall < 4 ~ 'Neutral',
                    overall >= 4 ~ 'Positive'
                  )
                 ) %>% 
                  select(asin, overall, sentiment) %>% 
                  group_by(asin) %>% 
                  summarize(occurences = n(), overall, avg_rating = mean(overall), sentiment, asin)

reviews_query <- as.data.frame(reviews_query)


## Total number of positive reviews

length(reviews_query$asin[reviews_query$sentiment == 'Positive'])

## Total number of neutral reviews

length(reviews_query$asin[reviews_query$sentiment == 'Neutral'])

## Total number of negative reviews

length(reviews_query$asin[reviews_query$sentiment == 'Negative'])


## Let us now focus upon average rating

segmented_reviews <- reviews %>% 
                 select(asin, overall) %>% 
                 group_by(asin) %>% 
                 summarize(occurences = n(), avg_rating = mean(overall)) %>% 
                 mutate (
                      sentiment = case_when(
                      avg_rating <= 2 ~ 'Negative',
                      avg_rating > 2 & avg_rating < 4 ~ 'Neutral',
                      avg_rating >= 4 ~ 'Positive'
                      )
                  ) 

## Distribution of products by average rating 

general_distribution <- ggplot(data = segmented_reviews, aes(x = avg_rating)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, color = '#BA55D3', fill = '#BA55D3') + geom_density(alpha = 0.6, color = 'red') +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  labs(title = "RATING DISTRIBUTION", x = "AVERAGE RATING", y = "DENSITY") + theme_economist() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2)) 

general_distribution

## As seen in the graph, the distribution is left-skewed

mean(segmented_reviews$avg_rating)
median(segmented_reviews$avg_rating)
skewness(segmented_reviews$avg_rating)

## Some relevant statistical inference techniques would be of no use owing to the lack of normality, which can be easily tested

lillie.test(segmented_reviews$avg_rating)
lillie.test(segmented_reviews$occurences)

## Positive reviews is the dominant group, but let's see if there is some correlation between ratings and number of comments

## Scatterplot

scatter_plot <- ggplot(data = segmented_reviews, aes(x = avg_rating, y = occurences, fill = sentiment, color = sentiment)) + 
    geom_point() + theme_economist() +
    scale_fill_manual(values = c('#E3242B', "#E69F00", "#56B4E9")) +
    scale_color_manual(values = c('#E3242B', "#E69F00", "#56B4E9")) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2)) +
    labs(title = "OCCURENCES BY SENTIMENT", x = "AVERAGE RATING", y = "OCCURENCES") +
    coord_cartesian(ylim = c(0, 5000))

scatter_plot



cor(segmented_reviews$avg_rating, segmented_reviews$occurences)

## A quick glance at the graph and the low correlation value are indicative of a low relationship between both variables

## Let's test if the correlation is statistically significant

correlationTest(segmented_reviews$avg_rating, segmented_reviews$occurences, "pearson")

## We reject the null hypothesis that there is none, however, there does not seem to be a linear relationship

## Let us use rank correlation coefficients:

correlationTest(segmented_reviews$avg_rating, segmented_reviews$occurences, "spearman")
correlationTest(segmented_reviews$avg_rating, segmented_reviews$occurences, "kendall")

# Variables' distributions are independent

## This may be caused by outliers, which can be clearly reflected in a boxplot


boxplot <- ggplot(data = segmented_reviews, aes(x = avg_rating, y = occurences, fill= sentiment)) + 
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(values = c('#E3242B', "#E69F00", "#56B4E9")) +
    scale_color_manual(values = c('#E3242B', "#E69F00", "#56B4E9")) +
    theme(legend.position = "none") + theme_economist() +
    labs(title = "OCCURENCES DISTRIBUTION", x = "AVERAGE RATING", y = "OCCURENCES") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2)) +
    coord_cartesian(ylim = c(0, 50)) + xlim(c(0, 6.5))

boxplot


## Statistical inference to study if groups have different means

negative <- segmented_reviews$occurences[segmented_reviews$sentiment == 'Negative']
neutral <- segmented_reviews$occurences[segmented_reviews$sentiment == 'Neutral']
positive <- segmented_reviews$occurences[segmented_reviews$sentiment == 'Positive']

length(negative) ; length(neutral) ; length(positive)

occurence <- segmented_reviews$occurences

Block <- c(rep(1, length(negative)), rep(2, length(neutral)), rep(3, length(positive)))

Block <- factor(Block)

anova.fit <- aov(occurence ~ Block)

summary(anova.fit)

tukey <- TukeyHSD(anova.fit)

tky = as.data.frame(TukeyHSD(anova.fit)$Block)
tky$pair = rownames(tky)

# Plot pairwise TukeyHSD comparisons and color by significance level

ggplot(tky, aes(color = cut(`p adj`, c(0, 0.01, 0.05, 1), 
                           label= c("p < 0.01", "p < 0.05", "Non-Significant")))) +
  geom_hline(yintercept = 0, lty = "11", color = "grey30") +
  geom_errorbar(aes(pair, ymin = lwr, ymax = upr), width = 0.2, size = 1.3) +
  geom_point(aes(pair, diff), size = 3) +
  labs(color = "") + theme_economist() +
  labs(title = "TUKEY-HSD TEST", x = "PAIRS", y = expression(H[0])) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, vjust = -2))

## Although difference among means for positive and neutral is not statistically significant, let's see if medians are (Q2)

positive_data <- segmented_reviews$occurences[segmented_reviews$sentiment == 'Positive']

SIGN.test(positive_data, md = median(segmented_reviews$occurences[segmented_reviews$sentiment == 'Neutral']), 
          alternative = "greater")