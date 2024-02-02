# importing necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(glue)) install.packages("glue", repos = "http://cran.us.r-project.org")
if(!require(ggExtra)) install.packages("ggExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(glue)
library(ggExtra)
library(stringr)

################################################################################
calculate_RMSE <- function(y,y_hat){
  N <- length(y)
  sqrt((1/N)*sum((y - y_hat)^2))}

calculate_mean_rating_per_genre <- function(df, all_genres){
  sapply(all_genres, function(my_genre){
    mean(df %>% filter(str_detect(genres, my_genre)) %>% 
           pull(rating))})}

get_genre_score <- function(df, df_genres){
  genre_combinations <- df$genre_list
  genre_score <- sapply(genre_combinations, function(my_combination){
    movie_genre_score <- mean(
      df_genres %>% 
        filter(genre_name %in% my_combination) %>% 
        pull(mean_rating))
    movie_genre_score})
  genre_score}

################################################################################
# loading the dataset
load("edx.Rdata")

# quickly see how the dataset looks like
head(edx)

edx_downsized <- edx[sample(nrow(edx), size = 50000, replace = FALSE), ]

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

# Visualize relationship between rating and frequency of rating for a movie
rating_vs_number <- edx %>% 
  group_by(movieId) %>% 
  summarize(number_of_ratings = n(),
            average_rating = mean(rating))

joint_plot <- rating_vs_number %>%
  ggplot(aes(x=number_of_ratings, y=average_rating)) + 
  geom_point(alpha=0.15) + scale_x_continuous(trans="log10")
ggExtra::ggMarginal(p = joint_plot, type = "histogram")

rating_vs_number_corr <- cor(
  rating_vs_number$average_rating, 
  rating_vs_number$number_of_ratings)

cat(glue("correlation coefficient between average rating and number of ratings is {rating_vs_number_corr}"))

# So it may makes sense to add number of ratings as a feature to our data set
edx <- left_join(edx, rating_vs_number, by=c("movieId")) # ==> data leakage?

rm(rating_vs_number)

# now let's look at the effect of genres on the rating. 
cat(glue("There are {n_distinct(edx$genres)} distinct genres in the dataset"))

# but each movie has a combination of genres. Let's see list of the core genres

edx <- edx %>% 
  mutate(genre_list=str_split(
    genres, 
    "\\|"))

all_genres <- unique(unlist(edx$genre_list))

# now we can calculate the mean rating for movies that has each of the genres listed
genre_means <- calculate_mean_rating_per_genre(df=edx, all_genres=all_genres)

df_mean_per_genre <- data.frame(
  genre_name = all_genres[-length(all_genres)], # remove the last element (corresponds to no_genre)
  mean_rating = genre_means[-length(genre_means)]) %>% # remove the last element (corresponds to NA)
  mutate(genre_name=reorder(genre_name, mean_rating))

rm(genre_means)

df_mean_per_genre %>%
  ggplot(aes(x=genre_name, y=mean_rating)) + 
  geom_col() + 
  coord_flip()

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
  ggplot(aes(average_rating)) + geom_density() + facet_grid(genre_2~genre_1)

# there seems to be a genre dependency on rating. So we keep genre as a feature
# now let's look at the effect of user on the rating

my_plot <- edx %>%
  group_by(userId) %>% 
  summarize(rating_num = n(),
            avg_rating = mean(rating)) %>%
  ggplot(aes(x=rating_num, y=avg_rating)) + 
  geom_point(alpha=0.10) + scale_x_continuous(trans="log10")
ggExtra::ggMarginal(p = my_plot, type = "histogram", bins = 50) 

# There seems to be a user dependency, because different users have different average
# ratings. And a big majority of users have rated very few movies. However, the 
# distribution of average ratings per user seems to be normal


# Extract year and month as possibly useful features
edx$year <- year(as_datetime(edx$timestamp))
edx$month <- month(as_datetime(edx$timestamp))

edx %>% 
  group_by(year) %>% 
  summarize(mean_rating = mean(rating), 
            n_rating = n()) %>%
  ggplot(aes(x=year, y=mean_rating)) + 
  geom_point(aes(size=n_rating)) + 
  geom_line()

edx %>% 
  group_by(month) %>% 
  summarize(mean_rating = mean(rating), 
            n_rating = n()) %>%
  ggplot(aes(x=month, y=mean_rating)) + 
  geom_point(aes(size=n_rating)) + 
  geom_line()

# month effect seems to be minimal. It makes sense to not use it as a feature 
# to avoid an over-fitting situation


# for a careful eye, there is more information in the dataset. One can extract
# the release year of the movies from the title

edx <- edx %>% mutate(release_year = as.numeric(sub(".*\\((\\d+)\\)", "\\1", title)))

edx %>% 
  group_by(release_year) %>% 
  summarize(mean_rating = mean(rating), 
            n_rating = n()) %>%
  ggplot(aes(x=release_year, y=mean_rating)) + 
  geom_point(aes(size=n_rating)) + 
  geom_line() + geom_smooth()

edx <- edx %>% mutate(years_difference = year - release_year)

edx %>% 
  group_by(years_difference) %>% 
  summarize(mean_rating = mean(rating),
           n_rating = n()) %>%
  ggplot(aes(x=years_difference, y=mean_rating)) + 
  geom_point(aes(size=n_rating)) + 
  geom_line() + geom_smooth()


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

calculate_RMSE(test_set$rating, predictions) # 1.059996


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

calculate_RMSE(test_set$rating, predictions) # 0.9440672


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

calculate_RMSE(test_set$rating, predictions) # 0.8661476

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

calculate_RMSE(test_set$rating, predictions) # 0.8658121


# Adding the genre didn't help a lot, just a bit. Let's try the effect of year. 
# ==> rating = mu + b_i + b_u + b_g + b_y +randomness (model 3)
year_effect <- train_set %>% 
  group_by(year) %>% 
  summarize(b_y = mean(rating - mu - b_i - b_u - b_g))

train_set <- left_join(train_set, year_effect, by=c("year"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(genre_effect, by='genres') %>%
  left_join(year_effect, by='year') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 0.8657893


# Adding the effect of year didn't introduce much change. Let's try to add the 
# release year effect rating = mu + b_i + b_u + b_g + b_y + + b_rl + randomness (model 3)

release_year_effect <- train_set %>% 
  group_by(release_year) %>% 
  summarize(b_rl = mean(rating - mu - b_i - b_u - b_g - b_y))

train_set <- left_join(train_set, release_year_effect, by=c("release_year"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(genre_effect, by='genres') %>%
  left_join(year_effect, by='year') %>%
  left_join(release_year_effect, by='release_year') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y + b_rl) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions) # 0.8657893


# Let's regularize all
# to see if we can reduce the loss. It can be possible that a small number of 
# observations related to a movie, user, etc. can have a disproportionately
# big effect

# Do a grid search to find the optimal lambda. Note that b_i, b_u, and b_g are
# all more or less centered around 0. So we don't need to do an extra step of 
# removing the mean to make sure we are "shrinking the values to zero" 
# Add plots of the bi, bu, etc.

train_set <- train_set %>% select(-any_of(c("b_i", "b_u", "b_g", "b_y", "b_rl")))

lambdas <- seq(0, 15)

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
  
  year_difference_effect <- train_set %>% 
    group_by(years_difference) %>% 
    summarize(b_yd = sum(rating - mu - b_i - b_u)/(n()))
  train_set <- left_join(train_set, year_difference_effect, by=c("years_difference"))
  
  rl_year_effect <- train_set %>% 
    group_by(release_year) %>% 
    summarize(b_rl = sum(rating - mu - b_i - b_u - b_yd)/(n()))
  train_set <- left_join(train_set, rl_year_effect, by=c("release_year"))

  
  genre_effect <- train_set %>% 
    group_by(genres) %>% 
    summarize(b_g = sum(rating - mu - b_i - b_u - b_yd - b_rl)/(n()))
  train_set <- left_join(train_set, genre_effect, by=c("genres"))
  
  
  predictions <- test_set %>% 
    left_join(movie_effect, by='movieId') %>%
    left_join(user_effect, by='userId') %>%
    left_join(year_difference_effect, by='years_difference') %>%
    left_join(genre_effect, by='genres') %>%
    left_join(rl_year_effect, by='release_year') %>%
    mutate(pred = mu + b_i + b_u + b_yd + b_g + b_rl) %>%
    pull(pred)
  
  train_set <- train_set %>% select(-any_of(c("b_i", "b_u", "b_g", "b_y", "b_rl", "b_yd")))
  
  calculate_RMSE(test_set$rating, predictions)
  
  }
)

plot(lambdas, RMSEs) 

lambdas[which(RMSEs==min(RMSEs))]

min(RMSEs) # 0.8638806
