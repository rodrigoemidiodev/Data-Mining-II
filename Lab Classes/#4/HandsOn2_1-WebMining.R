# Set the working directory dinamically
wd <- readline(prompt = "Enter the path of your working directory: ")
setwd(wd)

# Read the csv file into a data frame
df <- read.csv("log.csv")

# Inspect how many times each page was visited
page_visits <- table(df$PAGE)
print(page_visits)

# Sort the pages by decreasing number of visits
sorted_pages <- sort(page_visits, decreasing = TRUE)
print(sorted_pages)

# Obtain the top 3 pages for recommendation
top_3_pages <- names(sorted_pages)[1:3]
print(top_3_pages)

# Transform the log access data into a matrix with a row for each user and for each user the information on his visits to this page
user_page_matrix <- table(df$USER, df$PAGE)
print(user_page_matrix)

# Obtain a distance matrix with the Euclidean distance between the users 
dist_matrix <- dist(user_page_matrix, method = "euclidean")
print(dist_matrix)

# Obtain an agglomerative clustering model with the distance matrix
hclust_model <- hclust(dist_matrix, method = "complete")

# Visualize the obtained dendogram 
plot(hclust_model, main = "Hierarchical Clustering Dendogram")

# Visualize the dendogram with hang = -0.1
plot(hclust_model, hang = -0.1, main = "Hierarchical Clustering Dendogram with hang = -0.1")

# Cut the hierarchical clustering in two clusters and inspect the cluster membership of each user
clusters <- cutree(hclust_model, k = 2)
print(clusters)

# Draw the previous solution in the dendogram
rect.hclust(hclust_model, k = 2, border = "red")

# Inspect the pages visited by users in cluster 1
cluster1_users <- names(clusters[clusters == 1])
print(cluster1_users)
cluster1_data <- df[df$USER %in% cluster1_users, ]
print(table(cluster1_data$USER, cluster1_data$PAGE))

# Inspect how many times each page was visited
cluster1_page_visits <- table(cluster1_data$PAGE)
print(cluster1_page_visits)

# Sort the pages by decreasing order of visits
sorted_cluster1_pages <- sort(cluster1_page_visits, decreasing = TRUE)
print(sorted_cluster1_pages)

# Obtain the top 2 pages for recommendation
top2_pages_cluster1 <- names(sorted_cluster1_pages)[1:2]
print(top2_pages_cluster1)

# Recommend the top 2 pages for users of cluster 2
cluster2_users <- names(clusters[clusters == 2])
print(cluster2_users)
cluster2_data <- df[df$USER %in% cluster2_users, ]
print(table(cluster2_data$USER, cluster2_data$PAGE))

cluster2_page_visits <- table(cluster2_data$PAGE)
print(cluster2_page_visits)

sorted_cluster2_pages <- sort(cluster2_page_visits, decreasing = TRUE)
print(sorted_cluster2_pages)

top2_pages_cluster2 <- names(sorted_cluster2_pages)[1:2]
print(top2_pages_cluster2)

# Recommend the top 3 pages for user u2, removing the pages that the user has already visited
u2_cluster <- clusters["u2"]
print(u2_cluster)

u2_visited_pages <- df$PAGE[df$USER == "u2"]
print(u2_visited_pages)

recommendation_candidates <- names(sorted_cluster1_pages)[!names(sorted_cluster1_pages) %in% u2_visited_pages]
print(recommendation_candidates)

top_n <- min(3, length(recommendation_candidates))
top_pages_for_u2 <- recommendation_candidates[1:top_n]
print(top_pages_for_u2)
