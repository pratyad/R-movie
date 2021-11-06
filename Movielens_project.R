if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

head(edx)
glimpse(edx)
#How many distinct movie, users and genres

n_distinct(edx$userId)
n_distinct(edx$genres)
n_distinct(edx$movieId)
nrow(edx)

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Histogram of edx 
#Distribution of Movie Ratings
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "aliceblue", color = "grey", bins = 10) +
  scale_x_log10() +
  ggtitle("Number of Movies Ratings")

#Distribution of Users
edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "aliceblue", color = "grey", bins = 10) +
  scale_x_log10() + 
  ggtitle("Number of Users Ratings")


# partition the edx set to create 2 sets for training and testing
set.seed(1)
y <- edx$rating
test_index1 <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train_set <- edx %>% slice(-test_index1)
test_set_temp <- edx %>% slice(test_index1)

# Make sure userId and movieId in test set are also in train set
test_set <- test_set_temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set

removed <- anti_join(test_set_temp, test_set)

train_set<-rbind(train_set, removed)
## RMSE compute root mean square error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
 
mean1<-mean(train_set$rating)
mean1
rmse_1 <- RMSE(test_set$rating, mean1) # compute root mean square error

rmse_1
rmse_results <- bind_rows(tibble(method="Average",  
                                 RMSE = rmse_1 ))

#Do Genres have an effect on ratings? 
#Extracted the genres from the data with the idea to do an analysis on each genre

dat <- train_set %>% separate_rows(genres, sep ="\\|")

head(dat)

movie_avgs_genres<-dat%>%
  group_by(genres)%>%
  summarise(b_g= mean(rating-mean1))

dat_test<- test_set%>% separate_rows(genres, sep ="\\|")

predicted_ratings <- mean1 + dat_test %>% 
  left_join(movie_avgs_genres, by='genres') %>%
  .$b_g

# calculate rmse using predicted ratings considering each genre
rmse_genre <- RMSE(predicted_ratings, dat_test$rating)
rmse_genre

rmse_results <- bind_rows(rmse_results, tibble(method="RMSE with genre",  
                                 RMSE = rmse_genre ))
# RMSE is still very high so we can conclude that genre does not lower RMSE considerably


#Modeling Movie Effects


#calculate the average rating for movie i
# movie avgs is mean of difference between the individual movie rating and mean of rating
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mean1))
#plotting histogram of movie avg 
movie_avgs%>%ggplot(aes(b_i)) + geom_histogram(bins = 10, color = "blue")
# calculating predicted ratings for individual movie
predicted_ratings <- mean1 + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# calculate rmse using predicted ratings considering each movie
rmse_2 <- RMSE(predicted_ratings, test_set$rating)
rmse_2


rmse_results<-bind_rows(rmse_results, tibble(method = "movie effect", RMSE= rmse_2))

#Modeling user effects
#calculate the average rating for user u
# also plot it using histogram
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mean1)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 20, color = "black")
# calculate user effect b_u
user_avgs <- train_set%>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mean1 - b_i))


# calculate predicted values on test set
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mean1 + b_i + b_u) %>%
  .$pred

# calculate rmse with user effect
rmse_3 <- RMSE(predicted_ratings, test_set$rating)
rmse_3

                                 
rmse_results_final<-bind_rows(rmse_results, tibble(method = "user effect", RMSE = rmse_3))                                  
rmse_results_final

# improvement in rmse is approx 6% after considering individual movie effect and user effect
# there are lots of movies which are rated very few times and based on those few ratings the 
# calculation of bi and bu is either high or low.

# we will find out using below code mistakes in user effect model


# find top 10 worst and best movies based on user effect
# we will have to create a table with movie ID and title and then extract distinct titles

movie_titles <- train_set %>% 
  select(movieId, title) %>%
  distinct()
# find top ten best movies using user effect
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
 tibble()

# top 10 worse movies based on user effect on ratings
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  tibble()
# all the movies in above two lists are not very famous movies 
# We will have to check how many times they were rated to see if there is a penalty of using
# small sample size in estimating ratings
#Regularization helps in reducing the overfitting of training data set in return for a small 
# amount of bias

# counting number of rating of the best movies by user effects on ratings
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)
# counting number of rating of the worst movies by user effects on ratings
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)

# the best and worst movies were rated by few users in most cases it is just 1 
# we need small value for parameter, simpler hypothesis which will be less prone to overfitting
#choosing regularization term lambda 

#We use regularization for the estimate both movie and user effects. We are minimizing:

#use cross-validation to pick a λ:

  lambda<-seq(0, 10, 0.25)
rmses<-sapply(lambda, function(l){
  mean_1<-mean(train_set$rating)
  b_i<-train_set%>%
    group_by(movieId)%>%
    summarise(b_i=sum(rating-mean1)/(n()+l))
  b_u<-train_set%>%
    left_join(b_i, by = "movieId")%>%
    group_by(userId)%>%
    summarise(b_u = sum(rating-mean1-b_i)/(n() + l))
  
   predicted_ratings<-train_set%>%
     left_join(b_i, by = "movieId") %>%
     left_join(b_u, by = "userId") %>%
     mutate(pred = mean1 + b_i + b_u) %>%
     .$pred
   
   return(RMSE(train_set$rating, predicted_ratings))
})
qplot(lambda, rmses)
#find value of lambda for which rmses is minimum
lambda<-lambda[which.min(rmses)]
#print lambda
lambda


# use lambda to calculate movie effect with regularization on training set

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mean1)/(n()+lambda))

#calculate user effect with regularization

b_u<-train_set%>%
  left_join(b_i, by ="movieId")%>%
  group_by(userId)%>%
  summarise(b_u= sum(rating-mean1-b_i)/(n() + lambda))


predicted_ratings<-test_set%>%
  left_join(b_i, by ="movieId")%>%
  left_join(b_u, by = "userId")%>%
  mutate(pred= mean1 +b_i +b_u)%>%
  pull(pred)
 rmse_4 <-RMSE(test_set$rating, predicted_ratings)
rmse_4 
rmse_results_final<-bind_rows(rmse_results_final, tibble(method = "Regularization  and user effect", RMSE = rmse_4))                                  
rmse_results_final<-rmse_results_final[-4,]
rmse_results_final


# regularization on validation set

  mean1 <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mean1) / (n()+lambda))
  
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mean1) / (n()+lambda))
  
  predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mean1 + b_i + b_u) %>%
    pull(pred)
  
rmse_5<-RMSE(validation$rating, predicted_ratings)
rmse_5
  
rmse_results_final<-bind_rows(rmse_results_final, tibble(method = "Regularization and user effect validation set", RMSE = rmse_5))                                  

rmse_results_final
install.packages('tinytex')
tinytex::install_tinytex()

