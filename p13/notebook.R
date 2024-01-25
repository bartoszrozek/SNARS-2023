library(data.table)
library(recommenderlab)
library(arules)

file_df <- list()
max_movie_id <- 0
for (i in 1:4){
    file_df[[i]] <-
        fread(
            glue::glue("p13/data/combined_data_{i}.txt"),
            fill = T,
            sep = ",",
            col.names = c("user_id", "rating", "date")
        )
    
    idx_to_drop <- stringi::stri_detect_regex(file_df[[i]][[1]], ":")
    movie_idx <- idx_to_drop |> cumsum() + max_movie_id
    max_movie_id <- max(movie_idx)
    file_df[[i]][,movie_id := movie_idx]
    file_df[[i]] <- file_df[[i]][!idx_to_drop]
    
}

data_frame <- rbindlist(file_df)
data_frame$date <- NULL
dates <- sort(unique(data_frame$date))

remove(file_df)
gc()

data.table::fwrite(data_frame, "p13/data/merged.csv")

data_frame <- fread("p13/data/merged.csv")
#EDA
#ratings distribution, ratings per user, reviews per movie
#dates from to, best and worse film

ratings_dist <- table(data_frame$rating) |> as.data.frame()
colnames(ratings_dist) <- c("rating", "frequency")
write.csv(ratings_dist, "p13/output/rat_dist.csv", row.names = F)

dist <- table(data_frame$movie_id) |> as.data.frame()
colnames(dist) <- c("movie_id", "n_ratings")
write.csv(dist, "p13/output/movie_rat_dist.csv", row.names = F)

dist <- table(data_frame$user_id) |> as.data.frame()
colnames(dist) <- c("user_id", "n_ratings")
write.csv(dist, "p13/output/user_rat_dist.csv", row.names = F)



#cutoff for 2005-01-01
users <- unique(data_frame$user_id)
sample_users <- sample(users, 0.1 * length(users))
data_frame <- data_frame[user_id %in% sample_users]
data_frame$date <- NULL


train_set <- data_frame[user_id %in% sample_users & date < "2005-01-01"]
test_set <- data_frame[user_id %in% sample_users & date >= "2005-01-01"]
train_set$date <- NULL

library(tidyr)

train_set <- data_frame[sample(nrow(data_frame), 100000)]

# Pivot the data frame
ratings_matrix <- data.table::dcast(train_set, user_id ~ movie_id,  value.var = "rating")
ratings_matrix <- as.matrix(ratings_matrix)
rownames_ <- ratings_matrix[,1]
ratings_matrix <- ratings_matrix[,-1]
dimnames(ratings_matrix) <- list(user = rownames_, movies = colnames(ratings_matrix))

ratings_matrix <- as(ratings_matrix, "realRatingMatrix")


model_params <- list(method = "cosine",
                     nn = 10, # find each user's 10 most similar users.
                     sample = FALSE, # already did this.
                     normalize = "center")




model <- ratings_matrix  |> #only fit on the 75% training data.
    Recommender(method = "UBCF", parameter = model_params)

model1_pred <- predict(model, ratings_matrix, type = "ratings")

test_error <- calcPredictionAccuracy(model1_pred, ratings_matrix, byUser = TRUE)
head(test_error)

#UBCF
#IBCF
#AR
#HybridRecommender
#Latent factor models

#dashboard -
# - select method
# - rate 10 films - select those films 
# - get 5 best films for you!

train_proportion <- 0.8
items_per_test_user_keep <- 10
good_threshold <- 4

model_train_scheme <- ratings_matrix |> 
    evaluationScheme(method = 'split', # single train/test split
                     train = train_proportion, # proportion of rows to train.
                     given = items_per_test_user_keep, # shouldn't keep n rec. items > min(rowCounts(movie_r))
                     k = 1)

saveRDS(colnames(model_train_scheme@data), file = "p13/colnames_m.rds")

model_params <- list(method = "cosine",
                     nn = 10, # find each user's 10 most similar users.
                     sample = FALSE, # already did this.
                     normalize = "center")

models <- list()

for (method in c("UBCF", "IBCF", "SVD", "LIBMF", "POPULAR")){
    model <- getData(model_train_scheme, "train") |>  
        Recommender(method = method)

    model_pred <- predict(model, getData(model_train_scheme, "known"), type = "ratings")
    test_error <- calcPredictionAccuracy(model_pred, getData(model_train_scheme, "unknown"))
    models[[method]] <- list(model = model, results = test_error)
    
}




model1 <- getData(model_train_scheme, "train") |>  
    Recommender(method = "UBCF", parameter = model_params)


pred <- predict(model1, as(t(matrix(c(1,2,3,1,2, rep(NA, 9170 - 5)))), "realRatingMatrix"), type = "ratings")
model1_pred <- predict(model1, getData(model_train_scheme, "known"), type = "ratings")
test_error <- calcPredictionAccuracy(model1_pred, getData(model_train_scheme, "unknown"))
head(test_error)


saveRDS(models, file = "p13/models.rds")
