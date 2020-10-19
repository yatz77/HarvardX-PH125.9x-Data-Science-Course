library(dslabs)
library(tidyverse)
data("movielens")
head(movielens)

movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:30)) %>%
  filter(movieId %in% keep) %>%
  select(userId, title, rating) %>%
  spread(title, rating)
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>%
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

movielens %>%
  dplyr::count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ggtitle("Users")

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
?semi_join
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# now the models
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie Effect Model",
                                     RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n() >=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu -b_i))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()

# Comprehension check
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# plot number of ratings for each movie against plot of year it came out
head(movielens)

movielens %>% group_by(movieId) %>%
  mutate(n_ratings = n()) %>%
  ggplot(aes(x=year, y=n_ratings)) +
  geom_point()

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# the top 25 movies after 1993

movielens %>% 
  filter(year > 1993) %>%
  group_by(movieId) %>%
  mutate(avg_rating = mean(rating)) %>%
  top_n(n = 25, wt = avg_rating) %>%
  arrange(desc(avg_rating)) %>%
  filter(title == "Shawshank Redemption, The")

movielens %>% 
  filter(year >= 1993) %>%
  mutate(rating_year = year(as_datetime(timestamp))) %>%
  filter(rating_year < 2019) %>%
  group_by(movieId) %>%
  mutate(avg_n_ratings_year = n()/(2019-1995)) %>%
  arrange(desc(avg_n_ratings_year))

# plot avg rating vs ratings per year
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  arrange(desc(rate)) %>%
  ggplot(aes(x=rate, y=rating)) +
  geom_point() +
  geom_smooth()


# average rating for each week and plot against date
?round_date
movielens %>% 
  mutate(rating_week = week(as_datetime(timestamp)))%>%
  group_by(rating_week) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(rating_week,avg_rating))+
  geom_line()

movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# determine the genre with the lowest average rating

movielens %>% group_by(genres) %>%
  filter(n() > 1000) %>%
  mutate(avg_genre = mean(rating), se_genre = sd(rating)/sqrt(length(rating))) %>%
  ggplot(aes(x = genres, y = avg_genre)) +
  geom_errorbar(aes(ymin=avg_genre-se_genre, ymax = avg_genre + se_genre)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  