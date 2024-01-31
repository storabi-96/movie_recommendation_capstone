# importing necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(glue)) install.packages("glue", repos = "http://cran.us.r-project.org")
if(!require(ggExtra)) install.packages("ggExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(FRAPO)) install.packages("FRAPO", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(glue)
library(ggExtra)
library(stringr)
library(FRAPO)
library(zoo)

# loading the dataset
load("edx.Rdata")

# quickly see how the dataset looks like
head(edx)

#edx_downsized <- edx[sample(nrow(edx), size = 50000, replace = FALSE), ]

# see how many users and movies we have
n_users <- n_distinct(edx$userId)
n_movies <- n_distinct(edx$movieId)

cat("number users in dataset: ", n_users)
cat("number movie in dataset: ", n_movies)

# see how many missing ratings we have
n_missing_rating <- (n_movies*n_users) - nrow(edx)

cat("number of missing movie ratings missing in dataset: ", n_missing_rating)
cat("number of movie ratings present in dataset: ", nrow(edx))
cat("sparsity: ", 1 - nrow(edx)/(n_movies*n_users))

# Let's do some EDA and see how does the data behave
# calculate average of rating range (not average of ratings)
range_average <- (max(edx$rating) - min(edx$rating))/2

# proportion of ratings above the range average
ratio_above_range_average <- mean(
  edx %>% 
  mutate(above_range_average = rating > range_average) %>%
  pull(above_range_average))

cat(glue("The average of the raiting range is {range_average}"))
cat(glue("About {round(ratio_above_range_average*100)}% of the ratings are above the range average"))

# Visualize relationship between rating and frequency of rating for a movie
rating_vs_number <- edx %>% 
  group_by(movieId) %>% 
  summarize(number_of_ratings = n(),
            average_rating = mean(rating))

joint_plot <- rating_vs_ratingfreq %>% 
  group_by(movieId) %>% 
  summarize(number_of_ratings = n(),
            average_rating = mean(rating)) %>%
  ggplot(aes(x=number_of_ratings, y=average_rating)) + 
  geom_point(alpha=0.15) + scale_x_continuous(trans="log10")
ggExtra::ggMarginal(p = joint_plot, type = "histogram")

rating_vs_number_corr <- cor(rating_vs_number$average_rating, rating_vs_number$number_of_ratings)
cat(glue("correlation coefficient between average rating and number of ratings is {rating_vs_number_corr}"))

# So it makes sense to add number of ratings as a feature to our data set
edx <- merge(edx, rating_vs_number, by=c("movieId"))

rm(rating_vs_number)
# now let's look at the effect of genres on the rating. 

cat(glue("There are {n_distinct(edx$genres)} distinct genres in the dataset"))

my_genres <- c("Sci-Fi", "Drama")
edx %>% mutate(genre_1 = ifelse(str_detect(genres,my_genres[1]), my_genres[1], glue("No {my_genres[1]}")),
                         genre_2 = ifelse(str_detect(genres,my_genres[2]), my_genres[2], glue("No {my_genres[2]}"))) %>%
  select(average_rating, genre_1, genre_2) %>%
  distinct() %>% # keep unique rows only
  ggplot(aes(average_rating)) + geom_density() + facet_grid(genre_2~genre_1)

# there seems to be a genre dependency on rating. So we keep genre as a feature
# now let's look at the effect of user on the rating
my_plot <- edx %>% 
  group_by(userId) %>% 
  summarize(rating_num = n(),
            avg_rating = mean(rating)) %>%
  ggplot(aes(x=rating_num, y=avg_rating)) + 
  geom_point(alpha=0.15) + scale_x_continuous(trans="log10")
ggExtra::ggMarginal(p = my_plot, type = "histogram")

# There seems to be a user dependency, because different users have different average
# ratings. However, the distribution of average ratings per user seems to be normal


test_indices <- createDataPartition(edx$rating, times=1, p=0.2, list=FALSE)

test_set <- edx[test_indices]
train_set <- edx[-test_indices]

fit_glm <- train(rating~movieId+userId+genres+number_of_ratings+average_rating, 
                   data=edx_downsized, method="glm")














