################################
# Create edx set, validation set
################################

# Loading of Data set

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(dplyr)
library(ggplot2)
library(stringr)
library(gganimate)
library(hrbrthemes)
options(scipen=10000)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



#Creation 10% of edx for testing of models
edx_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_data <- edx[-edx_index,]
temp <- edx[edx_index,]
edx_val <- temp %>% 
  semi_join(edx_data, by = "movieId") %>%
  semi_join(edx_data, by = "userId")

# Add rows removed from edx_val set back into edx_data
removed <- anti_join(temp, edx_val)
edx_data <- rbind(edx_data, removed)

rm(temp, removed, edx_index)

#Data Preprocessing and Cleanup

#Separating Year from title
edx <- edx %>% mutate(year = str_extract(title, "(\\d{4})") )
validation <- validation %>% mutate(year = str_extract(title, "(\\d{4})") )

edx_data <- edx_data %>% mutate(year = str_extract(title, "(\\d{4})") )
edx_val <- edx_val %>% mutate(year = str_extract(title, "(\\d{4})") )


edx %>% summarize(unique_users = n_distinct(userId), unique_movies = n_distinct(movieId))
#from the result above, we an deduce that not all users rated all movies. 

edx %>% group_by(movieId)%>% summarize(count = n()) %>% ggplot(aes(count)) +
              geom_histogram(bins=40,  fill = "blue", col = "black") +
              scale_x_log10() +
              xlab("Movie Distribution") +
              ggtitle("Number of Movies Rated in Movielens")
#The chart above shows that some movies are reated more than others. We can call such movies blockbusters.

edx %>% group_by(userId)%>% summarize(count = n()) %>% ggplot(aes(count)) +
  geom_histogram(bins=40,  fill = "green", col = "black") +
  scale_x_log10() +
  xlab("User Distribution") +
  ggtitle("Number of Users that rated movies in Movielens")

#The chart above shows that not all users are active. Some users rate more than others. Comparing this result to the summary of uniques users and movies,
#it is evident that some users watch the movie without rating.


edx  %>% filter(genres %in% c( "War", "Western", "Thriller", "Sci-Fi","Romance", "Mystery", "Musical", "Horror", "	Film-Noir", "Drama", "Documentary","Action", "Crime", "Comedy", "Adventure")) %>% 
  group_by(genres) %>% summarise(count = n()) %>%
  mutate(genres = reorder(genres, count)) %>%
  ggplot(aes(genres, count, fill = count)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_distiller(palette = "YlOrBr")+
  xlab("Movie count in genres") +
  ylab("Selected Genres") +
  ggtitle("Exploring movies count based on Genres")
#The chart above 
#Next we explore the genre variation in regards to year.

year_info <- edx  %>% filter(genres %in% c( "War", "Western", "Thriller", "Sci-Fi","Romance", "Mystery", "Musical", "Horror", "	Film-Noir", "Drama", "Documentary","Action", "Crime", "Comedy", "Adventure")) %>% 
                  select(genres, year, rating)
year_info <- year_info %>% group_by(genres, year) %>% summarise(count = n())
year_info %>% ggplot(aes(x = year, y = count, group = genres, color = genres)) +
                              geom_line ()+
                              geom_point() +
  scale_y_continuous() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Genres distribution over the years") +
  ylab("No") 

#from the chart above, we can see that there is a spike in certain genres from 1960. Thus we stream line our data to show only those genres.

year_info %>% filter(genres %in% c( "Thriller", "Sci-Fi", "Drama", "Documentary","Action", "Crime", "Comedy")) %>%
          filter(year >= 1960) %>% ggplot(aes(x = year, y = count, group = genres, color = genres)) +
          geom_line ()+
          geom_point() +
          scale_y_continuous() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          ggtitle("Genres distribution over the years") +
          ylab("No of movies") 


edx  %>% filter(genres %in% c( "War", "Western", "Thriller", "Sci-Fi","Romance", "Mystery", "Musical", "Horror", "	Film-Noir", "Drama", "Documentary","Action", "Crime", "Comedy", "Adventure")) %>% 
   group_by(genres) %>% summarise(count = n()) %>%
   mutate(genres = reorder(genres, count)) %>%
  ggplot(aes(genres, count, fill = count)) +
  geom_bar(stat="identity") +
  coord_flip() +
 scale_fill_distiller(palette = "YlOrBr")+
  xlab("Movie count in genres") +
  ylab("Selected Genres") +
  ggtitle("Exploring movies count based on Genres")

## All 
edx %>% group_by(genres, year) %>% summarise(nrate = sum(rating)) %>% 
   filter(year > 1960 & genres %in% c( "War", "Western", "Thriller", "Sci-Fi","Romance", "Mystery", "Musical", "Horror", "	Film-Noir", "Drama", "Documentary","Action", "Crime", "Comedy", "Adventure")) %>%
  ggplot(aes(year, genres, fill = nrate)) +
  geom_tile(color = "grey50") +
  scale_fill_gradientn(colors = terrain.colors(1000), trans = "sqrt") +
  geom_vline(xintercept=1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle("Movie Genres") +
  ylab("") +
  xlab("")


  
  edx  %>% filter(genres %in% c( "War", "Western", "Thriller", "Sci-Fi","Romance", "Mystery", "Musical", "Horror", "	Film-Noir", "Drama", "Documentary","Action", "Crime", "Comedy", "Adventure", "Film-Noir") & year > 1960) %>% 
  group_by(genres) %>% summarise(count = mean(rating)) %>%
  mutate(genres = reorder(genres, count)) %>%
  ggplot(aes(genres, count, fill = count)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_distiller(palette = "Spectral")+
  xlab("Selected Genres") +
  ylab("Average Rating") +
  ggtitle("Average Rating per genres")
  #We notice from the chart above that the genre with the highest average rating is Documentary even though Drama and comedy have the most rating.
  #It can be concluded at this stage that genre influences popularity but not rating.
  
  #To further support the theory, vizualization of movie ratings is required
#Analyzing top rated movie
  edx %>% group_by(title) %>% summarise(Avg_rating = mean(rating)) %>%
    filter(Avg_rating >= 4) %>% arrange(desc(Avg_rating)) %>% top_n(50) %>%
    mutate(title = reorder(title, Avg_rating)) %>%
    ggplot(aes(title, Avg_rating, fill = Avg_rating)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_distiller(palette = "Spectral")+
    xlab("Movie Titles") +
    ylab("Average Rating") +
    ggtitle("Top 50 rated movies")
  #Using the Avg_rating per movie to determine the Top rated movie we will see that Sun Alley, Shadows of forgotten ANcestors etc are rated high. 
  #Is this really the case? Next we explore the Top rated movies using average rating and Number of rating
  edx %>% group_by(title) %>% summarise(Avg_rating = mean(rating), no = n()) %>%
    filter(Avg_rating >= 4) %>% arrange(desc(no, Avg_rating)) %>% top_n(50) %>%
    mutate(title = reorder(title,Avg_rating)) %>%
    ggplot(aes(title, Avg_rating, fill = no)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_distiller(palette = "Spectral")+
    xlab("Movie Titles") +
    ylab("Average Rating") +
    ggtitle("Top 50 rated movies with high number of ratings")
  #Here, we can see that Shawshank Redemption 1994 has the highest Avg rating, though Pulp fiction (1994) has the highest rating.
  
  
#RMSE
#RMSE formula
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#To build the model edx_data and edx_val will be used to train and test respectively
#Using Guessing Algorithm to estimate movie rating
#Calculate average of all ratings. This means predicting the same rating for the movie regardless of the user, length or year of movie
Average_rating <- mean(edx_data$rating)
#Calculating RMSE based on average
avg_rmse <- RMSE(edx_val$rating, Average_rating)


#movie effect
#understand how the movieId affects ratings
movie_average <- edx_data %>% group_by(movieId) %>% summarize(LSE = mean(rating - Average_rating))
#plotting the movie average to see the result
movie_average %>% qplot(LSE, geom ="histogram", bins = 10, data = ., color = I("green"))

#The graph shows a distribution of move movie ratings equal or below average rating

#calculating predicted rating which is mean + bias
movie_effect_pred <- Average_rating + edx_val %>% left_join(movie_average, by="movieId") %>% pull(LSE)
movie_effect_rmse <- RMSE(movie_effect_pred, edx_val$rating)


#user effect
user_average = edx_data %>% group_by(userId) %>% summarize(UASE = mean(rating - Average_rating))

user_average %>% ggplot(aes(UASE)) +  geom_histogram(bins = 30, color = "black", fill = "purple")
user_effect_pred <- Average_rating + edx_val %>% left_join(user_average, by="userId") %>% pull(UASE)
user_effect_rmse <- RMSE(user_effect_pred, edx_val$rating)


#Year effect
year_average = edx_data %>% group_by(year) %>% summarise(YLSE = mean(rating - Average_rating))
year_average %>% ggplot(aes(YLSE)) +  geom_histogram(bins = 30, color = "black", fill = "blue")
year_effect_pred <- Average_rating + edx_val %>% left_join(year_average, by="year") %>% pull(YLSE)
year_effect_rmse <- RMSE(year_effect_pred, edx_val$rating)



#User effect and Movie effect
userMovie_average <- edx_data %>% left_join(movie_average, by="movieId") %>% group_by(userId) %>% 
                  summarize(ULSE = mean(rating - Average_rating - LSE))
userMovie_average %>% ggplot(aes(ULSE)) +  geom_histogram(bins = 30, color = "black", fill = "pink")
userMovie_effect_pred <- edx_val %>%
                    left_join(movie_average, by ="movieId") %>%
                    left_join(userMovie_average, by="userId") %>%
                    mutate(predicted = Average_rating + LSE + ULSE) %>% pull(predicted)


userMovie_effect_rmse <- RMSE(userMovie_effect_pred, edx_val$rating)

#Year, User and Movie Effect
YuserMovie_average <- edx_data %>% 
    left_join(movie_average, by="movieId") %>%
    left_join (userMovie_average, by="userId") %>% 
    group_by( year) %>% 
  summarize(YLSE = mean(rating - Average_rating - LSE - ULSE))
YuserMovie_average %>% ggplot(aes(YLSE)) +  geom_histogram(bins = 30, color = "black", fill = "brown")
YuserMovie_effect_pred <- edx_val %>%
  left_join(movie_average, by ="movieId") %>%
  left_join(userMovie_average, by="userId") %>%
  left_join(year_average, by="year") %>%
  mutate(predicted = Average_rating + LSE + ULSE + YLSE) %>% pull(predicted)


YuserMovie_effect_rmse <- RMSE(YuserMovie_effect_pred, edx_val$rating)

#Genres, user year effect
GenresMovie_avarage <- edx_data %>% 
  left_join(movie_average, by="movieId") %>%
  left_join (userMovie_average, by="userId") %>% 
  left_join(YuserMovie_average, by="year") %>%
  group_by(genres) %>%
  summarize(GLSE = mean(rating- Average_rating - LSE- ULSE - YLSE))
GenresMovie_avarage %>% ggplot(aes(GLSE)) +  geom_histogram(bins = 30, color = "black", fill = "red")
GenresM_movies_pred <- edx_val %>% 
  left_join(movie_average, by="movieId") %>%
  left_join (userMovie_average, by="userId") %>% 
  left_join(YuserMovie_average, by="year") %>%
  left_join(GenresMovie_avarage, by ="genres") %>%
  mutate(predicted = Average_rating + LSE +ULSE +YLSE +GLSE) %>% pull(predicted)
GuserMovie_effect_rmse <- RMSE(GenresM_movies_pred, edx_val$rating)



#Regularization
#From the charts and previous analysis of top rated movies, we wil notice that some of the best rated movies are rated by few users.
#Thus the need to remove noise

##selecting the tuning parameter lamda for cross validation on edx dataset
lambdas <- seq(0, 5, 0.25)


#Regularized Movie Effect
Reg_movie <- edx_data %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - Average_rating), n_i = n())

rmses_Reg_movie <- sapply(lambdas, function(l){
  predicted_ratings <- edx_val %>%
    left_join(Reg_movie, by='movieId') %>%
    mutate(b_u = b_i/(n_i+l)) %>%
    mutate(pred = Average_rating + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_val$rating))
})
qplot(lambdas, rmses_Reg_movie)
#Minimum lamda
lambdas[which.min(rmses_Reg_movie)]
#RMSE for the model
min(rmses_Reg_movie)

#Regularized Movie and User Effect

rmses_Reg_movie_user <- sapply(lambdas, function(l)
  {
  
 
  b_i <- edx_data %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - Average_rating)/(n()+l))
  b_u <- edx_data %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - Average_rating)/(n()+l))
  predicted_ratings <-
    edx_val %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = Average_rating + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_val$rating))
})
qplot(lambdas, rmses_Reg_movie_user)

lambda <- lambdas[which.min(rmses_Reg_movie_user)]
lambda
#RMSE for the model
min(rmses_Reg_movie_user)





#creating a table to aggregate all RMSE Models of Dataset edx
rmse_results <- tibble(method = "Using the average", RMSE = avg_rmse)
rmse_results <- bind_rows(rmse_results, tibble(method = "The movie effect", RMSE = movie_effect_rmse))
rmse_results <- bind_rows(rmse_results, tibble(method = "The User effect", RMSE = user_effect_rmse))
rmse_results <- bind_rows(rmse_results, tibble(method = "The Year effect", RMSE = year_effect_rmse))
rmse_results <- bind_rows(rmse_results, tibble(method ="The user + Movie Effect Model", RMSE = userMovie_effect_rmse))
rmse_results <- bind_rows(rmse_results, tibble(method ="The user + Movie + Year Effect Model ", RMSE = YuserMovie_effect_rmse))
rmse_results <- bind_rows(rmse_results, tibble(method ="The user + Movie + Year + Genres Effect Model ", RMSE = GuserMovie_effect_rmse))
rmse_results <- bind_rows(rmse_results, tibble(method ="Regularized Movie Effect", RMSE = min(rmses_Reg_movie)))
rmse_results <- bind_rows(rmse_results, tibble(method ="Regularized Movie + User Effect", RMSE = min(rmses_Reg_movie_user)))
rmse_results %>% knitr::kable()


##Using the regularized model for validation data
l <- lambdas[which.min(rmses_Reg_movie_user)]

mu <- mean(validation$rating)

b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))
b_u <- validation %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
predicted_ratings <-
  validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE(predicted_ratings, validation$rating)

## The RMSE of the model using validated dataset is
RMSE(predicted_ratings, validation$rating)