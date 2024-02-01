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

calculate_mean_rating_per_gerne <- function(df, all_genres){
  sapply(all_genres, function(my_genre){
    mean(df %>% filter(str_detect(genres, my_genre)) %>% 
           pull(rating))})}

get_genre_score <- function(df, df_genres){
  genre_combinations <- df$genre_list
  genre_score <- sapply(genre_combinations, function(my_combination){
    movie_genre_score <- sum(
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

cat(glue("The average of the rating range is {range_average}"))
cat(glue("About {round(ratio_above_range_average*100)}% of the ratings are above the range average"))

# Visualize relationship between rating and frequency of rating for a movie
rating_vs_number <- edx %>% 
  group_by(movieId) %>% 
  summarize(number_of_ratings = n(),
            average_rating = mean(rating))

joint_plot <- rating_vs_number %>%
  ggplot(aes(x=number_of_ratings, y=average_rating)) + 
  geom_point(alpha=0.15) + scale_x_continuous(trans="log10")
ggExtra::ggMarginal(p = joint_plot, type = "histogram")

rating_vs_number_corr <- cor(rating_vs_number$average_rating, rating_vs_number$number_of_ratings)
cat(glue("correlation coefficient between average rating and number of ratings is {rating_vs_number_corr}"))

# So it makes sense to add number of ratings as a feature to our data set
edx <- left_join(edx, rating_vs_number, by=c("movieId"))

rm(rating_vs_number)

# now let's look at the effect of genres on the rating. 
cat(glue("There are {n_distinct(edx$genres)} distinct genres in the dataset"))

# but each movie has a combination of genres. Let's see list of the core genres
all_genres <- unique(unlist(str_split(edx$genres, "\\|"), recursive = FALSE))

edx <- edx %>% 
  mutate(genre_list=str_split(
    genres, 
    "\\|"))

# now we can calculate the mean rating for movies that has each of the genres listed
genre_means <- calculate_mean_rating_per_gerne(df=edx, all_genres=all_genres)

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

edx <- edx %>% rename(c("genre_list" = "genre_list.x"))

df_edx_distinct_movies <- edx[c("movieId", "genre_list")] %>% distinct()

df_edx_distinct_movies$genre_scores <- get_genre_score(df=df_edx_distinct_movies,
                                                       df_genres=df_mean_per_genre)

df_edx_distinct_movies$genre_scores[is.na(df_edx_distinct_movies$genre_scores)] <- mean(
  df_edx_distinct_movies$genre_scores,na.rm=TRUE)

df_edx_distinct_movies <- df_edx_distinct_movies %>%
  mutate(genre_score_category = cut(
    genre_scores,
    breaks = seq(min(genre_scores), 
                 max(genre_scores), 
                 length=6),
    labels = c("F", "D", "C", "B", "A")
  ))


edx <- left_join(edx,
                 df_edx_distinct_movies, 
                 by = "movieId")


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

# use b_i to represent movie effect ==> rating = mu + b_i + randomness (model 1)
movie_effect <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# add the obtained movie effects to the training set
train_set <- left_join(train_set, movie_effect, by=c("movieId"))

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

calculate_RMSE(test_set$rating, predictions)

# although we took into account the individual movie effect using b_i, however,
# the movie effect might be due to several reasons (e.g. actors, producer, etc.)
# genre can be one of these reasons, but let's see if we improve the model by 
# adding a specific genre effect g_i


genre_effect <- train_set %>% 
  group_by(genre_score_category) %>% 
  summarize(g_i = mean(rating - mu - b_i - b_i))

train_set <- left_join(train_set, genre_effect, by=c("genre_score_category"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  left_join(genre_effect, by='genre_score_category') %>%
  mutate(pred = mu + b_i + b_u + g_i) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions)


# Ok adding the genre didn't help. Let's not consider it. but let's regularize
# the movie effect, as it can be possible that a movie with small number of 
# rating can have disproportionately large effect

# let's first optimize lambda for model 1, and then apply the optimized b_i's to
# model 2

lambdas <- seq(1, 3, 0.1)

RMSEs <- sapply(lambdas, function(lambda){
    movie_effect <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(lambda+n()))
    
    train_set <- subset(train_set, select = -b_i)
    train_set <- left_join(train_set, movie_effect, by=c("movieId"))
    
    predictions <- test_set %>% 
      left_join(movie_effect, by='movieId') %>%
      mutate(pred = mu + b_i) %>%
      pull(pred)
    calculate_RMSE(test_set$rating, predictions)
}
)

# lambda = 2.1 minimized RMSE. Let's use it and use user effect to see how much 
# it improves RMSE

lambda <- 2.1

movie_effect <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(lambda+n()))

train_set <- subset(train_set, select = -b_i)
train_set <- subset(train_set, select = -b_u)

train_set <- left_join(train_set, movie_effect, by=c("movieId"))

user_effect <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

train_set <- left_join(train_set, user_effect, by=c("userId"))

predictions <- test_set %>% 
  left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

calculate_RMSE(test_set$rating, predictions)







