################################
# Create edx set, validation set
################################
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# MovieLens dataset is downloaded.
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
title = as.character(title),
genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set is created, which consists of 10% of the MovieLens data.
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# We make sure that userId and movieId in the validation set are present in the edX set as well.
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Data exploration

#Check class of data
class(edx)

#Get brief overview of the data
glimpse(edx)

#See a summary of the data
summary(edx)

#Check whether there are any missing values
anyNA(edx)

#Show unique values of movie ratings
unique(edx$rating)

#Show number of users, movies and genres
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId), n_genres = n_distinct(genres))

#Convert timestamp to date-format
edx <- edx %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "GMT"))

edx$timestamp <- format(edx$timestamp, "%Y")

#Show that this has been done
head(edx)

#Rename the timestamp variable
names(edx)[4] <- "ratingyear"
names(edx)

#Separate release year of the movie from the movie title
edx <- edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))

#Confirm release year has been removed
head(edx)

#Separate release year from movie title in the validation set
validation <- validation %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))

#Rename timestamp variable with year of rating to rating year
names(validation)[4] <- "ratingyear"

#Convert timestamp variable in the validation set to date format
validation <- validation %>% mutate(ratingyear = as.POSIXct(ratingyear, origin = "1970-01-01", tz = "GMT"))
validation$ratingyear <- format(validation$ratingyear, "%Y")

#Show both datasets again for comparability
head(validation)

head(edx)

#Create age of movie feature
edx <- edx %>% mutate(movieage = 2019 - releaseyear, release_rating_range = as.numeric(ratingyear) - releaseyear)

#Show data
head(edx)

#Repeat for the validation set
validation <- validation %>% mutate(movieage = 2019 - releaseyear, release_rating_range = as.numeric(ratingyear) - releaseyear)

#Show the data
head(validation)

#Create a edx dataset that includes the split genres
edx_gsplit  <- edx  %>% separate_rows(genres, sep = "\\|")
val_gsplit <- validation %>% separate_rows(genres, sep = "\\|")

#Create dataset with separated individual genres and relevant variables
genre_data <- edx %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% summarize(n_ratings = n(), ratings_mean = mean(rating), n_users = n_distinct(userId), n_movies = n_distinct(movieId))
genre_data_val <- validation %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% summarize(n_ratings = n(), ratings_mean = mean(rating), n_users = n_distinct(userId), n_movies = n_distinct(movieId))

#Visualisation of the data
#Distribution of ratings in the training set
edx %>% count(movieId) %>%
ggplot(aes(n)) + geom_histogram(fill = "deepskyblue4", color = "azure3", bins = 30) +
scale_x_log10() + ggtitle("Number of vs. number of movies") + xlab("Ratings") + ylab("Movies")

#Check distributions of ratings in the training set
edx %>% ggplot(aes(rating)) + geom_histogram(bins = 30, binwidth=0.2, color = "azure3", fill = "deepskyblue4") +
ggtitle("Rating point distribution")

#Number of ratings by user
edx %>% count(userId) %>% ggplot(aes(n)) + geom_histogram(bins = 30, fill = "deepskyblue4", color = "azure3") + scale_x_log10() + ggtitle("Number of ratings by user") + xlab("Ratings") + ylab("Users")

#Create column with average user rating
edx <- edx %>% group_by(userId) %>% mutate(avg_user_rating = mean(rating))
head(edx)

#Show distribution of the average rating of users
edx %>% group_by(userId) %>% summarize(b_u = mean(rating)) %>% ggplot(aes(b_u)) +
geom_histogram(fill = "deepskyblue4", color = "azure3", bins = 30) +
 xlab("Avg. rating") + ylab("Number of users")

#Create column with average movie rating
edx <- edx %>% group_by(movieId) %>% mutate(avg_movie_rating = mean(rating))
head(edx)

#Give an overview of average movie rating
edx %>% group_by(userId) %>% summarize(b_u = mean(rating)) %>% ggplot(aes(b_u)) +
geom_histogram(fill = "deepskyblue4", color = "azure3", bins = 30) +
xlab("Avg. rating") + ylab("Number of movies")

#Give an overview of average movie rating based on those that were rated more than a 100 times
edx %>% group_by(movieId) %>% filter(n() >= 100) %>% summarize(b_u = mean(rating)) %>% ggplot(aes(b_u)) +
geom_histogram(fill = "deepskyblue4", color = "azure3", bins = 30) +
xlab("Avg. rating") + ylab("Number of movies")

#Install and load packages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Show the genres that were rated the most times
wordcloud(words = genre_data$genres, freq = genre_data$n_ratings,
min.freq = 10, max.words = 10, random.order = FALSE, random.color = FALSE,
rot.per = 0.35, scale = c(5, 0.2), font = 4, colors = brewer.pal(8,"Spectral"),
main = "Most frequently rated genres")

#Find the most popular genres by their average rating
genre_averages <- edx_gsplit %>%
group_by(genres) %>%
summarize(mean_rating_genre = mean(rating)) %>%
arrange(-mean_rating_genre)
#Show them in a histogram
genre_averages %>% ggplot(aes(reorder(genres, mean_rating_genre), mean_rating_genre, fill = mean_rating_genre)) +
geom_bar(stat = "identity") + coord_flip() +
scale_fill_distiller(palette = "Greens") + labs(y = "Rating mean", x = "Genre") +
ggtitle("Genre average ratings")

  
#Movies per release year
edx %>% group_by(releaseyear) %>% summarize(n = n_distinct(movieId)) %>%
ggplot(aes(releaseyear, n)) + geom_line() + ggtitle("Movies per release year")

#Average rating per release year
edx %>% group_by(releaseyear) %>% summarize(rating = mean(rating)) %>%
ggplot(aes(releaseyear, rating)) + geom_point() + geom_smooth() + ggtitle("Ratings per release year")

#Get top 20 most rated movies
most_rated_movies <- edx %>% group_by(title) %>% summarize(count = n()) %>% top_n(20, count) %>% arrange(desc(count))
most_rated_movies

#Get 20 least rated movies
least_rated_movies <- edx %>% group_by(title) %>% summarize(count = n()) %>% top_n(-20, count) %>% arrange(desc(-count)) %>% slice(1:20)
least_rated_movies

#Get all least rated movies
all_least_rated <- edx %>% group_by(title) %>% summarize(count = n()) %>% top_n(-20, count) %>% arrange(desc(-count))
all_least_rated

#Get top 20 movies based on average ratings
top_movies <- edx %>% group_by(title) %>% summarize(rating = mean(rating), count = n()) %>% top_n(20, rating) %>% arrange(desc(rating)) %>% slice(1:20)
top_movies

#Get top 20 most rated movies
most_rated_movies <- edx %>% group_by(title) %>% summarize(count = n()) %>% top_n(20, count) %>% arrange(desc(count))
most_rated_movies

#Get 20 worst movies based on average ratings
bottom_movies <- edx %>% group_by(title) %>% summarize(rating = mean(rating), count = n()) %>% top_n(-20, rating) %>% arrange(desc(-rating))
bottom_movies

#Split edx data further in to test and training sets
# Test set will be 10% of current edx data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index2,]
temp2 <- edx[test_index2,]

# Make sure userId and movieId in test set are also in training set
test_set <- temp2 %>% 
semi_join(train_set, by = "movieId") %>%
semi_join(train_set, by = "userId")
# Add rows removed from test set back into training set
removed <- anti_join(temp2, test_set)
train_set <- rbind(train_set, removed)
rm(test_index2, temp2, removed)

#Define the function that calculates RMSE
RMSE <- function(true_ratings, predicted_ratings){
sqrt(mean((true_ratings - predicted_ratings)^2, na.rm=T))
}

#Get mu_hat with the simplest model
mu_hat <- mean(train_set$rating)
mu_hat

#Predict the known ratings with mu_hat
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#Create the results table
rmse_results <- tibble(method = "Simple average model", RMSE = naive_rmse)

#Penalize movie effects and adjust the mean
b_i <- train_set %>% group_by(movieId) %>%
summarize(b_i = sum(rating - mu_hat)/(n() + 1))

#Save and plot the movie averages with the movie effect model
movie_effect_avgs <- train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu_hat))
movie_effect_avgs %>% qplot(b_i, geom = "histogram", bins = 10, data = ., color = I("azure3"), xlab = "b_i", ylab = "Number of movies")

#Save the new predicted ratings
predicted_ratings <- mu_hat + test_set %>% left_join(movie_effect_avgs, by='movieId') %>%
pull(b_i)

#Calculate the RMSE for the movie effect model
movie_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_effect_rmse

#Save with previous model results
rmse_results <- bind_rows(rmse_results, tibble(method="Movie effect model", RMSE = movie_effect_rmse))
rmse_results

#Penalize user and movie effects and adjust the mean
b_u <- train_set %>% group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n() + 1))

#Save and plot the user averages with the movie and user effect model
user_effect_avgs <- train_set %>% left_join(movie_effect_avgs, by='movieId') %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu_hat - b_i))
user_effect_avgs %>% qplot(b_u, geom = "histogram", bins = 30, data = ., color = I("azure3"), xlab = "b_u", ylab = "Number of movies")

#Calculate the new predicted ratings
predicted_ratings <- test_set %>%
left_join(movie_effect_avgs, by='movieId') %>%
left_join(user_effect_avgs, by = 'userId') %>%
mutate(pred = mu_hat + b_i + b_u) %>%
pull(pred)

#Calculate the new RMSE
user_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
user_effect_rmse

#Save with the previous model results
rmse_results <- bind_rows(rmse_results,
tibble(method="Movie + User Effects Model",
RMSE = user_effect_rmse))
rmse_results

#Chose the tuning parameter
lambdas <- seq(0, 10, 0.25)

rmses_lambda <- sapply(lambdas, function(l){

mu_hat <- mean(train_set$rating)

b_i <- train_set %>%
group_by(movieId) %>%
summarize(b_i = sum(rating - mu_hat)/(n()+l))

b_u <- train_set %>%
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))

predicted_ratings <-
test_set %>%
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
mutate(pred = mu_hat + b_i + b_u) %>%
pull(pred)

return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses_lambda)

#Find lambda that minimizes RMSE
lambda <- lambdas[which.min(rmses_lambda)]
lambda

#Apply the optimal lambda on the test set

predicted_reg <- sapply(lambda, function(l){
  
mu_hat <- mean(train_set$rating)
  
b_i <- train_set %>%
group_by(movieId) %>%
summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
b_u <- train_set %>%
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  
predicted_ratings <-
test_set %>%
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
mutate(pred = mu_hat + b_i + b_u) %>%
pull(pred)
  
return(RMSE(predicted_ratings, test_set$rating))
})

#Show the new RMSE
predicted_reg

#Save rmse with remaining results
rmse_results <- bind_rows(rmse_results,
tibble(method="Regularized Movie + User Effect Model",
RMSE = predicted_reg))
rmse_results

#Make new regularized model with year effect added

#Chose the tuning parameter
lambdas_y <- seq(0, 10, 0.25)

rmses_lambda_y <- sapply(lambdas_y, function(l){
  
mu_hat <- mean(train_set$rating)
  
b_i <- train_set %>%
group_by(movieId) %>%
summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
b_u <- train_set %>%
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))

b_y <- train_set %>%
left_join(b_i, by="movieId") %>%
left_join(b_u, by="userId") %>%
group_by(releaseyear) %>%
summarize(b_y = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  
predicted_ratings <-
test_set %>%
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
left_join(b_y, by = "releaseyear") %>%
mutate(pred = mu_hat + b_i + b_u + b_y) %>%
pull(pred)
  
return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas_y, rmses_lambda_y)

#Find lambda that minimizes RMSE
lambda_y <- lambdas_y[which.min(rmses_lambda_y)]
lambda_y

#Apply the optimal lambda on the test set

predicted_reg_y <- sapply(lambda_y, function(l){
  
mu_hat <- mean(train_set$rating)
  
b_i <- train_set %>%
group_by(movieId) %>%
summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
b_u <- train_set %>%
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  
b_y <- train_set %>%
left_join(b_i, by="movieId") %>%
left_join(b_u, by="userId") %>%
group_by(releaseyear) %>%
summarize(b_y = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  
predicted_ratings <-
test_set %>%
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
left_join(b_y, by = "releaseyear") %>%
mutate(pred = mu_hat + b_i + b_u + b_y) %>%
pull(pred)

return(RMSE(predicted_ratings, test_set$rating))
})

predicted_reg_y

#Save rmse with remaining results
rmse_results <- bind_rows(rmse_results,
tibble(method="Regularized Movie + User + Year Effect Model",
RMSE = predicted_reg_y))
rmse_results

#Use the optimal model on the validation set
chosen_model_RMSE <- sapply(lambda_y, function(l){
  
mu_hat <- mean(edx$rating)
  
b_i <- edx %>%
group_by(movieId) %>%
summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
b_u <- edx %>%
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  
b_y <- edx %>%
left_join(b_i, by="movieId") %>%
left_join(b_u, by="userId") %>%
group_by(releaseyear) %>%
summarize(b_y = sum(rating - b_i - b_u - mu_hat)/(n()+l))

predicted_ratings <-
validation %>%
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
left_join(b_y, by = "releaseyear") %>%
mutate(pred = mu_hat + b_i + b_u + b_y) %>%
pull(pred)
  
return(RMSE(predicted_ratings, validation$rating))
})

chosen_model_RMSE
