# Set the working directory dinamically
wd <- readline(prompt = "Enter the path of your working directory: ")
setwd(wd)

# Load required packages
library(recommenderlab)
library(ggplot2)

# Read the csv file into a data frame
df <- read.csv("log1.csv")

# Convert the data frame to a binary matrix
brm <- as(df, "binaryRatingMatrix")

# Set the seed to 2021
set.seed(2021)

# Check the number of ratings per user
user_item_counts <- rowCounts(brm)

# Identify users with at least 3 ratings 
valid_users <- names(user_item_counts[user_item_counts >= 3])

# Create a filtered matrix with the valid users
filtered_brm <- brm[valid_users]

# Define an evaluation scheme that splits the data into train and test set and establish that 2 items of test cases are already known
eval_scheme <- evaluationScheme(data = filtered_brm, method = "split", train = 0.8, given = 2, goodRating = 1)

# Check how the data was split according to the previous evaluation scheme
train_data <- getData(eval_scheme, "train")
print(dim(train_data))

known_data <- getData(eval_scheme, "known")
print(dim(known_data))

unknown_data <- getData(eval_scheme, "unknown")
print(dim(unknown_data))

# Define the list of methods that will be used to obtain the top N recommendations
methods <- list("popular" = list(name="POPULAR", param=NULL), "user-based CF" = list(name="UBCF", param=NULL), "item-based CF" = list(name="IBCF", param=NULL))

# Use the function evaluate with the previously defined evaluation scheme, methods and considering top 1, 3 and 5 recommendations for each of the models
results <- evaluate(eval_scheme, methods, n = c(1,3,5))

# Explore the obtained object
print(str(results))
print(avg(results))

# Plot the ROC curves for each of the methods
plot(results, "ROC", annotate = TRUE, main = "ROC Curves")

# Plot the precision/recall curves for each of the methods 
plot(results, "prec/rec", annotate = TRUE, main = "Precision-Recall Curve")
