# importing necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(glue)) install.packages("glue", repos = "http://cran.us.r-project.org")
if(!require(ggExtra)) install.packages("ggExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(glue)
library(ggExtra)
library(stringr)
library(scales)

################################################################################
calculate_RMSE <- function(y,y_hat){
  N <- length(y)
  sqrt((1/N)*sum((y - y_hat)^2))}

calculate_mean_rating_per_genre <- function(df, all_genres){
  sapply(all_genres, function(my_genre){
    mean(df %>% filter(str_detect(genres, my_genre)) %>% 
           pull(rating))})}

################################################################################
# loading the dataset
load("edx.Rdata")

# quickly see how the dataset looks like
head(edx)

# Initial exploration of data
n_users <- n_distinct(edx$userId)
n_movies <- n_distinct(edx$movieId)
n_missing_rating <- (n_movies*n_users) - nrow(edx)
sparsity <- 1 - nrow(edx)/(n_movies*n_users)

cat("number users in dataset: ", n_users)
cat("number movie in dataset: ", n_movies)
cat("number of missing movie ratings missing in dataset: ", n_missing_rating)
cat("number of movie ratings present in dataset: ", nrow(edx))
cat("sparsity: ", sparsity)

# EDA and visualization

# Visualize relationship between rating and number of ratings for a movie

rating_vs_number <- edx %>% 
  group_by(movieId) %>% 
  summarize(number_of_ratings = n(),
            average_rating = mean(rating))

joint_plot_movie <- rating_vs_number %>%
  ggplot(aes(x=number_of_ratings, y=average_rating)) + 
  geom_point(alpha=0.15, color="blue3") + 
  scale_x_continuous(trans="log10") + 
  labs(x = "Number of ratings for movie", 
       y = "Average rating for movie",
       title ="Number of ratings vs average rating for movies")
ggExtra::ggMarginal(p = joint_plot_movie, 
                    type = "histogram", 
                    bins = 50,
                    color = "steelblue", fill="blue3")

rating_vs_number_corr <- cor(
  rating_vs_number$average_rating, 
  rating_vs_number$number_of_ratings)

cat(glue("correlation coefficient between average rating and number of ratings is {rating_vs_number_corr}"))

# So it may makes sense to add number of ratings as a feature to our data set at
# some if we need it. But we should do that after splitting the dataset to avoid
# data leakage. But for further data visualization, we do it here and update it later
edx <- left_join(edx, rating_vs_number, by=c("movieId"))

rm(rating_vs_number) # remove things we don't use from the environment

cat(glue("There are {n_distinct(edx$genres)} distinct genre combinations in the dataset"))

# Each movie has a combination of genres. Let's see list of the core genres
edx <- edx %>% mutate(genre_list=str_split(genres, "\\|"))
all_genres <- unique(unlist(edx$genre_list))

# now we can calculate the mean rating for movies that has each of the genres listed
genre_means <- calculate_mean_rating_per_genre(df=edx, all_genres=all_genres)

df_mean_per_genre <- data.frame(
  genre_name = all_genres[-length(all_genres)], # remove the last element (corresponds to no_genre)
  mean_rating = genre_means[-length(genre_means)]) %>% # remove the last element (corresponds to NA)
  mutate(genre_name=reorder(genre_name, mean_rating))

rm(genre_means) # remove things we don't use from the environment

df_mean_per_genre %>%
  ggplot(aes(x=genre_name, y=mean_rating)) + 
  geom_col(color="steelblue", fill="blue3") + 
  coord_flip() + 
  labs(x = "Genre present in the movie", 
       y = "Average rating",
       title ="Average movie rating per genre")

# compare best and worst performing genres
my_genres <- c("Film-Noir", "Horror")
edx %>% mutate(genre_1 = ifelse(str_detect(genres,my_genres[1]), 
                                my_genres[1], 
                                glue("No {my_genres[1]}")),
               genre_2 = ifelse(str_detect(genres,my_genres[2]), 
                                my_genres[2], 
                                glue("No {my_genres[2]}"))) %>%
  select(average_rating, genre_1, genre_2) %>%
  distinct() %>% # keep unique rows only
  ggplot(aes(average_rating)) + 
  geom_density(color="blue3" , linewidth=1) + 
  facet_grid(genre_2~genre_1) + 
  labs(x = "Average rating", 
       title ="Average movie rating vs genre combination")

# there seems to be a genre dependency on rating. So we can use genre as a feature
# now let's look at the effect of user on the rating

joint_plot_user <- edx %>%
  group_by(userId) %>% 
  summarize(rating_num = n(),
            avg_rating = mean(rating)) %>%
  ggplot(aes(x=rating_num, y=avg_rating)) + 
  geom_point(alpha=0.075, color="blue3") + 
  scale_x_continuous(trans="log10") +
  labs(x = "Number of ratings by the users", 
       y = "User average ratings",
       title ="Number of ratings per user vs user average rating")
ggExtra::ggMarginal(p = joint_plot_user, 
                    type = "histogram", 
                    bins = 50,
                    color = "steelblue", fill="blue3")

# There seems to be a user dependency, because different users have different average
# ratings. And a big majority of users have rated very few movies. However, the 
# distribution of average ratings per user seems to be normal


# Extract year and month as possibly useful features
edx$rating_year <- year(as_datetime(edx$timestamp))
edx$rating_month <- month(as_datetime(edx$timestamp))

edx %>% 
  group_by(rating_year) %>% 
  summarize(mean_rating = mean(rating), 
            n_rating = n()) %>%
  ggplot(aes(x=rating_year, y=mean_rating)) + 
  geom_line(linewidth=1, color="black") + 
  geom_point(aes(size=n_rating), color="blue3") + 
  ylim(3, 4.5) + 
  scale_x_continuous(breaks = seq(min(edx$rating_year), 
                                  max(edx$rating_year), by = 1)) +  # Set custom x-axis breaks
  scale_size_continuous(labels = scales::label_number(
    scale = 1/1e6, 
    suffix = " Million")) +  # Format size axis labels
  labs(x = "Year", 
       y = "Yearly average ratings",
       size = "Number ratings",
       title ="Average rating per year")

edx %>% 
  group_by(rating_month) %>% 
  summarize(mean_rating = mean(rating), 
            n_rating = n()) %>%
  ggplot(aes(x=rating_month, y=mean_rating)) + 
  geom_line(linewidth=1, color="black") + 
  geom_point(aes(size=n_rating), color="blue3") + 
  ylim(3, 4.5) + 
  scale_x_continuous(breaks = seq(min(edx$rating_month), 
                                  max(edx$rating_month), by = 1)) +  # Set custom x-axis breaks
  scale_size_continuous(labels = scales::label_number(
    scale = 1/1e6, 
    suffix = " Million")) +  # Format size axis labels
  labs(x = "Month", 
       y = "Monthly average ratings",
       size = "Number ratings",
       title ="Average rating per month")


# month effect seems to be minimal. It makes sense to not use it as a feature 
# to avoid an over-fitting situation


# for a careful eye, there is more information in the dataset. One can extract
# the release year of the movies from the title
# extract movie release year from the title
edx <- edx %>% 
  mutate(release_year = as.numeric(sub(".*\\((\\d+)\\)", "\\1", title)))

# plot average rating per release year
edx %>% 
  group_by(release_year) %>% 
  summarize(mean_rating = mean(rating), 
            n_rating = n()) %>%
  ggplot(aes(x=release_year, y=mean_rating)) + 
  geom_line(linewidth=1, color="black") + 
  geom_point(aes(size=n_rating), color="blue3") + 
  ylim(3, 4.5) + 
  scale_x_continuous(
    breaks = seq(min(edx$release_year), 
                 max(edx$release_year), 
                 by = 10)) +  # Set custom x-axis breaks
  scale_size_continuous(labels = scales::label_number(
    scale = 1/1e6, 
    suffix = " Million")) +  # Format size axis labels
  geom_smooth(method="loess", color="steelblue") + 
  labs(x = "Movie release year", 
       y = "Average ratings",
       size = "Number ratings",
       title ="Average rating per release year of the movie")

# calculate difference in years between movie release and its rating
edx <- edx %>% mutate(years_difference = rating_year - release_year)

# see if there is any rows with negative years_difference
head(edx %>% 
       filter(years_difference < 0) %>% 
       select(title, release_year, rating_year, years_difference))

edx %>% 
  filter(years_difference >= 0) %>% 
  group_by(years_difference) %>% 
  summarize(mean_rating = mean(rating),
           n_rating = n()) %>%
  ggplot(aes(x=years_difference, y=mean_rating)) + 
  geom_line(linewidth=1, color="black") + 
  geom_point(aes(size=n_rating), color="blue3") + 
  ylim(3, 4.5) + 
  scale_x_continuous(
    breaks = seq(0, 
                 max(edx$years_difference), 
                 by = 10)) +  # Set custom x-axis breaks
  scale_size_continuous(labels = scales::label_number(
    scale = 1/1e6, 
    suffix = " Million")) +  # Format size axis labels
  geom_smooth(method="loess", color="steelblue") + 
  labs(x = "Rating year - Release year ", 
       y = "Average ratings",
       size = "Number ratings",
       title ="Average rating vs time difference (i.t.o years) between movie rating and release year")

#################################################################################

# Let's split the data set into training and test sets
test_indices <- createDataPartition(edx$rating, times=1, p=0.2, list=FALSE)

test_set <- edx[test_indices,]
train_set <- edx[-test_indices,]

# make sure we don't have any new user/movie ids in the test set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# average of all ratings
mu <- mean(train_set$rating)

# use mu (mean) as the prediction for all ==> rating = mu + randomness (model 0)
predictions <- test_set %>% 
  mutate(pred = mu) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 1.060539


# use b_i to represent movie effect ==> rating = mu + b_i + randomness (model 1)
movie_effect <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# add the obtained movie effects to the training set
train_set <- left_join(train_set, movie_effect, by=c("movieId"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 0.9434635


# use b_u to represent user effect ==> rating = mu + b_i + b_u + randomness (model 2)
user_effect <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

# add the obtained user effects to the training set
train_set <- left_join(train_set, user_effect, by=c("userId"))

# do a prediction based on model 2 and calculate RMSE
predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 0.8659932

# although we took into account the individual movie effect using b_i, however,
# the movie effect might be due to several reasons (e.g. actors, producer, etc.)
# genre can be one of these reasons, but let's see if we improve the model by 
# adding a specific genre effect b_g
# ==> rating = mu + b_i + b_u + b_g + randomness (model 3)

genre_effect <- train_set %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))

train_set <- left_join(train_set, genre_effect, by=c("genres"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(genre_effect, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 0.8656635


# Adding the genre didn't help a lot, just a bit. Let's try the effect of year 
# difference between release and rating of a movie. 
# ==> rating = mu + b_i + b_u + b_g + b_yd +randomness (model 3)
year_difference_effect <- train_set %>% 
  group_by(years_difference) %>% 
  summarize(b_yd = mean(rating - mu - b_i - b_u - b_g))

train_set <- left_join(train_set, year_difference_effect, by=c("years_difference"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(genre_effect, by='genres') %>%
  left_join(year_difference_effect, by='years_difference') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_yd) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 0.8652424


# Let's also add the release year effect 
# rating = mu + b_i + b_u + b_g + b_yd + + b_rl + randomness (model 4)

release_year_effect <- train_set %>% 
  group_by(release_year) %>% 
  summarize(b_rl = mean(rating - mu - b_i - b_u - b_g - b_yd))

train_set <- left_join(train_set, release_year_effect, by=c("release_year"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(genre_effect, by='genres') %>%
  left_join(year_difference_effect, by='years_difference') %>%
  left_join(release_year_effect, by='release_year') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_yd + b_rl) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 0.8650521


# Let's regularize all
# to see if we can reduce the loss. It can be possible that a small number of 
# observations related to a movie, user, etc. can have a disproportionately
# big effect

# Do a grid search to find the optimal lambda. Note that b_i, b_u, and b_g are
# all more or less centered around 0. So we don't need to do an extra step of 
# removing the mean to make sure we are "shrinking the values to zero" 
# Add plots of the bi, bu, etc.

train_set <- train_set %>% select(-any_of(c("b_i", "b_u", "b_g", 
                                            "b_yd", "b_rl")))

lambdas <- seq(0, 10, length=15)

RMSEs <- sapply(lambdas, function(lambda){
  
  print(lambda)
  
  movie_effect <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(lambda+n()))
  train_set <- left_join(train_set, movie_effect, by=c("movieId"))
  
  user_effect <- train_set %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i)/(lambda + n()))
  train_set <- left_join(train_set, user_effect, by=c("userId"))
  
  genre_effect <- train_set %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu - b_i - b_u)/(lambda + n()))
  train_set <- left_join(train_set, genre_effect, by=c("genres"))
  
  year_difference_effect <- train_set %>% 
    group_by(years_difference) %>% 
    summarize(b_yd = sum(rating - mu - b_i - b_u - b_g)/(lambda + n()))
  train_set <- left_join(train_set, year_difference_effect, by=c("years_difference"))
  
  release_year_effect <- train_set %>% 
    group_by(release_year) %>% 
    summarize(b_rl = sum(rating - mu - b_i - b_u - b_g - b_yd)/(lambda + n()))
  train_set <- left_join(train_set, release_year_effect, by=c("release_year"))

  
  predictions <- test_set %>% 
    left_join(movie_effect, by='movieId') %>%
    left_join(user_effect, by='userId') %>%
    left_join(genre_effect, by='genres') %>%
    left_join(year_difference_effect, by='years_difference') %>%
    left_join(release_year_effect, by='release_year') %>%
    mutate(pred = mu + b_i + b_u +b_g + b_yd + b_rl) %>%
    pull(pred)
  
  train_set <- train_set %>% select(-any_of(c("b_i", "b_u", "b_g", 
                                              "b_rl", "b_yd")))

  calculate_RMSE(test_set$rating, predictions)
  }
)


plot(lambdas, RMSEs, 
     xlab = "lambda", ylab = "RMSE",
     col = "blue3", 
     main = "RMSE values with regularization using different lambda values",
     grid(col = "grey", lty = "dotted"))


lambdas[which(RMSEs==min(RMSEs))]

min(RMSEs) # 0.8643476

################################################################################
################################################################################

lambda <- lambdas[which(RMSEs==min(RMSEs))]

load("final_holdout_test.Rdata")

final_holdout_test <- final_holdout_test %>% 
  mutate(release_year = as.numeric(sub(".*\\((\\d+)\\)", "\\1", title)))

final_holdout_test$rating_year <- year(
  as_datetime(final_holdout_test$timestamp))

final_holdout_test <- final_holdout_test %>% 
  mutate(years_difference = rating_year - release_year)

# Now we can use entire edx dataset to train the model for prediction on final test set

movie_effect <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(lambda+n()))
edx <- left_join(edx, movie_effect, by=c("movieId"))

user_effect <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_i)/(lambda + n()))
edx <- left_join(edx, user_effect, by=c("userId"))

genre_effect <- edx %>% 
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_i - b_u)/(lambda + n()))
edx <- left_join(edx, genre_effect, by=c("genres"))

year_difference_effect <- edx %>% 
  group_by(years_difference) %>% 
  summarize(b_yd = sum(rating - mu - b_i - b_u - b_g)/(lambda + n()))
edx <- left_join(edx, year_difference_effect, by=c("years_difference"))

release_year_effect <- edx %>% 
  group_by(release_year) %>% 
  summarize(b_rl = sum(rating - mu - b_i - b_u - b_g - b_yd)/(lambda + n()))
edx <- left_join(edx, release_year_effect, by=c("release_year"))

predictions <- final_holdout_test %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(genre_effect, by='genres') %>%
  left_join(year_difference_effect, by='years_difference') %>%
  left_join(release_year_effect, by='release_year') %>%
  mutate(pred = mu + b_i + b_u +b_g + b_yd + b_rl) %>%
  pull(pred)

edx <- edx %>% select(-any_of(c("b_i", "b_u", "b_g", 
                                            "b_rl", "b_yd")))

calculate_RMSE(final_holdout_test$rating, predictions) # 0.8638654

edx <- edx %>% select(
  -any_of(c("b_i", "b_u", "b_g", "b_y", "b_rl", "b_yd")))



