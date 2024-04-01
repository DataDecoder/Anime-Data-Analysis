
# Install and load necessary packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
library(readr)
library(ggplot2)
library(dplyr)

# Task 1: Load the "Anime Dataset" into R and inspect its structure
anime_data <- read_csv("C:/Users/Vinod/Downloads/anime.csv")

# Task 2: Identify key variables and check for missing values
str(anime_data)
summary(anime_data)

# Task 3: Handle missing values
anime_data <- na.omit(anime_data)

# Task 4: Compute basic descriptive statistics for ratings and popularity scores
ratings_stats <- summary(anime_data$rating)
popularity_stats <- summary(anime_data$members)

# Task 5: Generate visualizations for the distribution of ratings and popularity
ggplot(anime_data, aes(x = rating)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of Anime Ratings", x = "Rating", y = "Frequency")

ggplot(anime_data, aes(x = members)) +
  geom_histogram(binwidth = 10000, fill = "green", color = "black") +
  labs(title = "Distribution of Anime Popularity", x = "Members", y = "Frequency")

# Task 6: Analyze the relationship between ratings and popularity
ggplot(anime_data, aes(x = rating, y = members)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Ratings and Popularity", x = "Rating", y = "Members")

# Task 7: Explore the distribution of anime across different genres
# Split the 'genre' column into individual genres
genres <- strsplit(anime_data$genre, ", ")
all_genres <- unlist(genres)

# Create a table of genre frequencies
genre_table <- table(all_genres)

# Display the top 5 most popular genres based on viewership
top_genres_viewership <- head(sort(genre_table, decreasing = TRUE), 5)
print(top_genres_viewership)

# Task 8: Histogram for the distribution of ratings
ggplot(anime_data, aes(x = rating)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of Anime Ratings", x = "Rating", y = "Frequency")


# Display the top 5 genres with the highest average ratings
top_genres_ratings <- genre_avg_ratings %>%
  arrange(desc(avg_rating)) %>%
  head(5)
print(top_genres_ratings)

## Bar plot for the top 5 genres with highest average ratings
ggplot(top_genres_ratings, aes(x = reorder(genre, -avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 5 Genres with Highest Average Ratings", x = "Genre", y = "Average Rating")


#Analysis 1: Number of Episodes vs. Ratings
# Scatter plot for the relationship between the number of episodes and ratings
ggplot(anime_data, aes(x = as.numeric(episodes), y = rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Number of Episodes and Ratings",
       x = "Number of Episodes", y = "Rating")

# Top 10 most popular anime based on members
top_popular_anime <- anime_data %>%
  arrange(desc(members)) %>%
  head(10)

# Bar plot for the most popular anime
ggplot(top_popular_anime, aes(x = reorder(name, -members), y = members)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 10 Most Popular Anime", x = "Anime Name", y = "Members")

# Select top genres (modify as needed)
selected_genres <- c("Josei", "Thriller", "Mystery", "Police", "Shounen")

# Filter data for selected genres
selected_genre_data <- anime_data %>%
  filter(genre %in% selected_genres)

# Box plot for the distribution of ratings for top genres
ggplot(selected_genre_data, aes(x = reorder(genre, -rating), y = rating)) +
  geom_boxplot(fill = "green") +
  labs(title = "Rating Distribution for Top Genres", x = "Genre", y = "Rating")
