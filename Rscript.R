################################################################################
########### Create edx set, validation set (final hold-out test set) ###########
################################################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


if(as.numeric(version$major) < 4) { # if using R 3.6 or earlier:
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
} else { # if using R 4.0 or later:
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                             title = as.character(title),
                                             genres = as.character(genres))
}

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

################################################################################
############################ End of code supplied by EDX #######################
################################################################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem ", repos = "http://cran.us.r-project.org")
if(!require(parallel)) install.packages("parallel ", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(cowplot)
library(purrr)
library(reshape2)
library(recosystem)
library(parallel)

################################################################################
################################# Introduction #################################
################################################################################

# See information about the data, how many observations, variables and variables names with data type:
edx %>% 
  select(userId, movieId, rating, timestamp, title, genres) %>% 
  glimpse()

################################################################################
################################### Methods ####################################
################################################################################

###### Data exploration ######

### Summarise the data: how many users, movies, titles and genre combinations:
edx %>% 
  summarize(Users = n_distinct(userId),
            Movies = n_distinct(movieId),
            Titles = n_distinct(title),
            Genres = n_distinct(genres)) %>% 
  t() %>% 
  data.frame() %>% 
  `colnames<-`("Total")

### How many movies per genre:

# vector of unique genres
Genres <- edx$genres %>% 
  unique() %>% 
  str_split(pattern = "\\|") %>%
  unlist() %>%
  unique()

# select only the movieIDs and genres
edx_unique_movies <- edx %>% 
  select(movieId, genres) %>% 
  unique()

# count how many items each unique genre appear in the data.frame of movieIds and genres
Count_genres <- NULL
for(i in Genres) {
  Count_genres[[i]] <- str_count(edx_unique_movies$genres, i) %>% sum()
}

# create data.frame of results
data.frame(Count_genres) %>% 
  t() %>% 
  data.frame() %>% 
  `colnames<-`("Number of movies") %>% 
  arrange(desc(`Number of movies`))


# there is one title with two movieID:
edx %>% 
  filter(!duplicated(movieId)) %>% 
  filter(title %in% title[duplicated(title)])


###### Rating system ######

# How the users rates the movies?
edx %>% 
  group_by(rating) %>% 
  tally() %>% # count how many times each rate appears in the edx set
  ggplot(aes(x = rating, y = n)) + 
  geom_col() +
  geom_vline(xintercept = mean(edx$rating)) +
  geom_vline(xintercept = median(edx$rating), color = "red") +
  ylab("Number of ratings") +
  labs(caption = "Black vertical line represents the mean movie rating while the red line represents the median")



###### Release date ######

# How the release date of the movie influences the rating?

# This code extracts the year from the movie title, creating a new column (year)
edx <- edx %>% 
  mutate(year = str_extract(title, "\\([:digit:]{4}\\)$"),
         year = str_remove_all(year, "\\(|\\)"),
         year = as.numeric(year))

# create boxplot for each year, to show the variation in rating according to the year of the movie release.
# each red dot represents a movie that received a rating outside of the ~95% (1.5 * IQR) confidence interval around the median
# the function cut_width discretise numeric data to make the boxplot
edx %>% 
  group_by(movieId, year) %>% 
  summarise(rating = mean(rating)) %>% 
  ggplot(aes(x = year, y = rating)) +
  geom_boxplot(aes(group = cut_width(year, 0.25)), color = "red") +
  geom_point(alpha = 0.05) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  ylab("Mean rating") +
  labs(caption = "Each black dot represents a movie, and since each dot is almost invisible,\nwe can see that there are a lot more movies releases in recent years\nby the formation of a black shadow over the boxplot.")

# How the mean rating varies when we group all movies releases by year?
edx %>% 
  group_by(year) %>% 
  summarise(rating = mean(rating)) %>% 
  ggplot(aes(x = year, y = rating)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) +
  ylab("Mean rating") 



###### Rating year ######

# How the users rates the movies changed over the years?
# transform the timestamp of the review in date
edx <- mutate(edx, date = as_datetime(timestamp))

# How the users rates the movies changed over the years?
Rating_week <- edx %>% 
  mutate(date = round_date(date, "week")) %>% 
  group_by(date) %>% 
  summarise(rating = mean(rating)) %>% 
  ggplot(aes(y = rating, x = date)) +
  geom_point() +
  geom_smooth() +
  ylab("Mean rating") +
  xlab("Date of rating")

Rating_year <- edx %>% 
  mutate(date = round_date(date, "year")) %>% 
  group_by(date) %>% 
  summarise(rating = mean(rating)) %>% 
  ggplot(aes(y = rating, x = date)) +
  geom_point() +
  geom_smooth() +
  ylab("Mean rating") +
  xlab("Year of rating")

plot_grid(Rating_week, Rating_year, labels = c("A", "B"))


# find how many movies and active users there are by year
edx %>% 
  mutate(date = round_date(date, "year")) %>% 
  group_by(date) %>% 
  summarise(User = n_distinct(userId),
            Movie = n_distinct(movieId)) %>% 
  mutate(date = year(date))



###### Seasonal movies ######

# The ratings varies by month? Say blockbusters releases around christmas?
edx %>%
  mutate(date = month(date)) %>% 
  group_by(date) %>% 
  summarise(mean = mean(rating),
            sd = sd(rating)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) +
  scale_x_continuous(breaks = 1:12, labels = month(1:12, label = T, locale = "en"))


###### Genres ######

### Influence of the genre of the movie in the review

# lets create new columns, one for each genre:
# loop over the genres with "map", create columns with "transmute" and bind these columns to the original data
# after this we can transform this new dataset to long format, so that we can plot using ggplot2
# this way is more efficient than using separate_rows, even if the end result is the same

if(!any(str_detect(colnames(edx), "Action"))) {
  edx <- Genres %>% 
    map_dfc(~ edx %>% 
              transmute(!! .x := as.numeric(str_detect(genres, .x)))) %>% 
    bind_cols(edx, .) 
  
  edx_long <- edx %>% 
    reshape2::melt(id.vars = c("userId", "movieId", "rating", "timestamp", "title", "genres", "year", "date")) %>% 
    filter(value != 0)
}
# this if statement is only here in case the dataset was loaded from a file
# it verifies if the columns for the genres are already in the data.frame

# calculate mean and se to plot confidence interval
Plot_genres_a <- edx_long %>% 
  group_by(variable) %>%
  summarize(n = n(), 
            avg = mean(rating), 
            se = sd(rating)/sqrt(n())) %>%
  mutate(variable = reorder(variable, avg)) %>%
  ggplot(aes(x = variable, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point(aes(size = n), alpha = 0.3) +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab("Genre") +
  ylab("Mean rating") +
  scale_size(name = "Number of ratings")

# remove movies without genres:
Plot_genres_b <- edx_long %>% 
  filter(variable != "(no genres listed)") %>% 
  group_by(variable) %>%
  summarize(n = n(), 
            avg = mean(rating), 
            se = sd(rating)/sqrt(n())) %>%
  mutate(variable = reorder(variable, avg)) %>%
  ggplot(aes(x = variable, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point(aes(size = n), alpha = 0.3) +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab("Genre") +
  ylab("Mean rating") +
  labs(caption = "The bars represents 95% confidence intervals of the mean") +
  scale_size(name = "Number of ratings")

# plot the figures without legend in the same grid, 
# and then this grid with only the legend of the plots
plot_grid(Plot_genres_a + theme(legend.position = "none"),
          Plot_genres_b + theme(legend.position = "none"), 
          align = "h",
          labels = c("A", "B")) %>% 
  plot_grid(get_legend(Plot_genres_a + theme(legend.position = "bottom")),
            rel_heights = c(0.9, 0.1),
            ncol = 1)



###### Number of ratings by user ######

# How each user rates the movies:
edx %>%
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  xlab("Number of ratings (log)") +
  ylab("Number of users")




###### Modeling approach ######

### create train and test datasets:
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, 
                                  times = 1,
                                  p = 0.2, 
                                  list = FALSE)

train_set <- edx[-test_index,]
test_set_temp <- edx[test_index,]

# Select only users and movies that appear in the train dataset
test_set <- test_set_temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(test_set_temp, test_set)
train_set <- rbind(train_set, removed)


### RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

rm(test_set_temp, removed)


### parallel matrix factorization

## detect number of CPU cores avaiable for the test, minus 1 for other tasks of the operating system:
Cores <- detectCores() - 1

## for parallel matrix factorization, we only need the userId, the movieId and the rating
## all other variables will be created with single value decomposition
## for this package we also need to specify if the IDs start with 0 (index1 = FALSE) or with 1 (index1 = TRUE)
## for the test set, we don't need the ratings

## Create train set in memory:
train_set_PMF <- data_memory(user_index = train_set$userId,
                             item_index = train_set$movieId, 
                             rating = train_set$rating,
                             index1 = TRUE)

## Create test set in memory:
test_set_PMF <- data_memory(user_index = test_set$userId,
                            item_index = test_set$movieId, 
                            rating = NULL,
                            index1 = TRUE)


## Create object that will hold the results
set.seed(123, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
r = Reco()

## Cross validation to tune the model parameters

# all tune, train and validation tests will be enclosed in two functions:
start_time <- Sys.time()
end_time <- Sys.time()
# to calculate how many minutes each function takes to run in my pc
TIME <- end_time - start_time

start_time_tune <- Sys.time()
opts = r$tune(train_set_PMF, 
              opts = list(dim = c(10, 20, 30), # number of latent factors, features of the movies that appeal to some users but not to others.
                          lrate = c(0.1, 0.2), # learning rate, which can be thought of as the step size in gradient descent.
                          nfold = 5, #number of folds for cross validation
                          niter = 10, # the number of iterations.
                          nthread = Cores # number of threads.
              ))
end_time_tune <- Sys.time()


## Train the model with the best parameters from CV
# Store the model parameters in a file, to reduce memory usage:
Model <- file.path(tempdir(), "model.txt")

start_time_train <- Sys.time()
r$train(train_set_PMF, 
        out_model = Model,
        opts = c(opts$min, 
                 nthread = Cores, 
                 niter = 20))
end_time_train <- Sys.time()




## Predict the ratings in the test set, this result will be stored in memory
# if memory is a concern, it is possible to store in a file, in the same way that the train parameters were stored
start_time_predict <- Sys.time()
Y_hat <- r$predict(test_set_PMF, 
                   out_pred = out_memory())
end_time_predict <- Sys.time()



## Calculate RMSE for the model:
RMSE_test <- RMSE(test_set$rating, Y_hat)


################################################################################
################################### Results ####################################
################################################################################

### Now that the model is complete, 
### lets see how well it holds when predicting the ratings in the validation set


## Create validation set
Validation_set <- data_memory(user_index = validation$userId,
                              item_index = validation$movieId, 
                              rating = NULL,
                              index1 = TRUE)

## Predict the ratings
start_time_predict_validation <- Sys.time()
Y_hat_validation_1 <- r$predict(Validation_set, 
                                out_pred = out_memory())
end_time_predict_validation <- Sys.time()


## RMSE
RMSE_validation_1 <- RMSE(validation$rating, Y_hat_validation_1)


#######################################
# As a test, I will also train the model using all the edx data to see if the RMSE of the validation set
# will improve with 20% more data to tune the parameters in CV:

set.seed(123, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

edx_set <- data_memory(user_index = edx$userId,
                       item_index = edx$movieId, 
                       rating = edx$rating,
                       index1 = TRUE)

edx_model = Reco()

start_time_tune_edx <- Sys.time()
opts_new = edx_model$tune(edx_set, 
                          opts = list(dim = c(10, 20, 30), # number of latent factors
                                      lrate = c(0.1, 0.2), # learning rate, which can be thought of as the step size in gradient descent.
                                      nfold = 5, #number of folds for cross validation
                                      niter = 10, # the number of iterations.
                                      nthread = Cores # number of threads.
                          ))
end_time_tune_edx <- Sys.time()



Model_edx <- file.path(tempdir(), "model_edx.txt")

start_time_train_edx <- Sys.time()
edx_model$train(edx_set, 
                out_model = Model_edx,
                opts = c(opts_new$min, 
                         nthread = Cores, 
                         niter = 20))
end_time_train_edx <- Sys.time()



start_time_predict_edx <- Sys.time()
Y_hat_validation_2 <- edx_model$predict(Validation_set, out_memory())
end_time_predict_edx <- Sys.time()



RMSE_validation_2 <- RMSE(validation$rating, Y_hat_validation_2)

## Table with final RMSE
data.frame(model = c("test_set", "edx"),
           RMSE = c(RMSE_validation_1, RMSE_validation_2))





## Calculate computation time
# Not in report

time_tune <- end_time_tune - start_time_tune
time_train <- end_time_train - start_time_train
time_predict <- end_time_predict - start_time_predict

time_predict_validation <- end_time_predict_validation - start_time_predict_validation

time_tune_edx <- end_time_tune_edx - start_time_tune_edx
time_train_edx <- end_time_train_edx - start_time_train_edx
time_predict_edx <- end_time_predict_edx - start_time_predict_edx


data.frame("Train set" = c("test_set", "edx"), 
           "Tune time" = c(time_tune, time_tune_edx),
           "Train time" = c(time_train, time_train_edx),
           "Predict time" = c(time_predict_validation, time_predict_edx),
           check.names = F)