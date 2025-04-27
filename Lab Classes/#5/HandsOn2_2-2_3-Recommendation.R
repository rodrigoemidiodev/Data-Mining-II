# Set the working directory dinamically
wd <- readline(prompt = "Enter the path of your working directory: ")
setwd(wd)

# Load required packages
library(recommenderlab)

# Read the csv file into a data frame
df <- read.csv("log1.csv")

# Coerce the data frame with user-page access information from log1.csv file to a binaryRatingMatrix
brm <- as(df, "binaryRatingMatrix")

# Select the information on the first 6 users to be used as training offline data and save it to a new variable
brm_offline <- brm[1:6]

# Inspect the content of brm_offline
brm_matrix <- getRatingMatrix(brm_offline)
brm_df <- as(brm_offline, "data.frame")
head(brm_df)

# Apply the function rowCounts and colCounts to brm_offline
user_counts <- rowCounts(brm_offline)
print(user_counts)

page_counts <- colCounts(brm_offline)
print(page_counts)

# Apply the function image to brm_offline
image(brm_offline, main = "User-Page Access Matrix")

# Obtain the recommender model based on association rules with the instruction modelAR <- Recommender(brm_offline, "AR")
modelAR <- Recommender(brm_offline, "AR")

# Inspect te association rules that compose the model
AR_model <- getModel(modelAR)
print(str(AR_model))

# Apply predict function and get the top 2 recommendations
u7 <- brm["u7"]
pred_u7 <- predict(modelAR, u7, n = 2)
print(pred_u7)

# Apply the function getList to the obtained predictions to inspect the actual recommendations
recom_u7 <- getList(pred_u7)
print(recom_u7)

# Filter the rules which have been triggered for this active user
u7_df <- as(u7, "data.frame")
u7_pages <- as.character(u7_df$item[u7_df$rating == 1])
print(u7_pages)

print(AR_model$confidence)

# Deploy the recommendation model for u8
u8 <- brm["u8"]
pred_u8 <- predict(modelAR, u8, n = 2)
recom_u8 <- getList(pred_u8)
print(recom_u8)

u8_df <- as(u8, "data.frame")
u8_pages <- as.character(u8_df$item[u8_df$rating == 1])
print(u8_pages)

# Explore the types of recommendation models available for binary rating matrices
models <- recommenderRegistry$get_entries(dataType="binaryRatingMatrix")
print(names(models))

# Make the top 2 recommendations to u7 and u8 using the popularity of the pages
modelPOP <- Recommender(brm_offline, "POPULAR")
pred_u7_pop <- predict(modelPOP, u7, n = 2)
recom_u7_pop <- getList(pred_u7_pop)
print(recom_u7_pop)

pred_u8_pop <- predict(modelPOP, u8, n = 2)
recom_u8_pop <- getList(pred_u8_pop)
print(recom_u8_pop)

# Build the similarity cosine matrix (only for first 6 users) using a user-based approach
user_sim <- similarity(brm_offline, method = "cosine", which = "users")
print(as.matrix(user_sim))

# Build the similarity cosine matrix (only for first 6 users) using a item-based approach
item_sim <- similarity(brm_offline, method = "cosine", which = "items")
print(as.matrix(item_sim))

# Obtain the top 2 recommendations with user-based CF and item-based CF methods using the cosine similarity with a neighborhood of size 3 
modelUBCF <- Recommender(brm_offline, "UBCF", parameter = list(method = "cosine", nn = 3))
modelIBCF <- Recommender(brm_offline, "IBCF", parameter = list(method = "cosine", k = 3))

# For active user u8
pred_u8_ubcf <- predict(modelUBCF, u8, n = 2)
recom_u8_ubcf <- getList(pred_u8_ubcf)
print(recom_u8_ubcf)

pred_u8_icbf <- predict(modelIBCF, u8, n = 2)
recom_u8_ibcf <- getList(pred_u8_icbf)
print(recom_u8_ibcf)

# For active user u7
pred_u7_ubcf <- predict(modelUBCF, u7, n = 2)
recom_u7_ubcf <- getList(pred_u7_ubcf)
print(recom_u7_ubcf)

pred_u7_icbf <- predict(modelIBCF, u7, n = 2)
recom_u7_ibcf <- getList(pred_u7_icbf)
print(recom_u7_ibcf)

# Explore the types of recommendation models available for real rating matrices
real_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
print(real_models)

# Read the file log1Ratings.csv into a data frame
df_ratings <- read.csv("log1Ratings.csv")

# Convert to realRatingMatrix and select the first 6 users for training
rrm <- as(df_ratings, "realRatingMatrix")
rrm_offline <- rrm[1:6]

# Build and deploy collaborative filtering recommendation models for a user-based CF approach with two neighbors to predict the ratings of users u7 and u8 using the first 6 users for training
modelUBCF_real <- Recommender(rrm_offline, "UBCF", parameter = list(nn = 2))
u7_real <- rrm["u7"]
u8_real <- rrm["u8"]
pred_u7_ubcf_real <- predict(modelUBCF_real, u7_real, type = "ratings")
pred_u8_ubcf_real <- predict(modelUBCF_real, u8_real, type = "ratings")
print(as(pred_u7_ubcf_real, "list"))
print(as(pred_u8_ubcf_real, "list"))

# Build and deploy collaborative filtering recommendation models for a item-based CF approach with two neighbors to predict the ratings of users u7 and u8 using the first 6 users for training
modelIBCF_real <- Recommender(rrm_offline, "IBCF", parameter = list(k = 2))
pred_u7_ibcf_real <- predict(modelIBCF_real, u7_real, type = "ratings")
pred_u8_ibcf_real <- predict(modelIBCF_real, u8_real, type = "ratings")
print(as(pred_u7_ibcf_real, "list"))
print(as(pred_u8_ibcf_real, "list"))
