#Title: "Clustering and Association Rule Learning on Customer behavior based on Online store."
#Name: "Pramodh Reddy Savasani" 
#Date: "05-08-2023"

### Step 1: Data Preprocessing

# Load the necessary libraries
library(tidyverse)
library(cluster)

# Load the dataset
online_retail_51 <- read.csv("OnlineRetail_randomized_51.csv")

# Remove missing values and duplicates
online_retail_51 <- online_retail_51 %>%
  filter(!is.na(InvoiceNo), !is.na(StockCode), !is.na(Description), !is.na(Quantity), !is.na(InvoiceDate), !is.na(UnitPrice)) %>%
  distinct()

# Convert InvoiceDate column to date format
online_retail_51$InvoiceDate <- as.POSIXct(online_retail_51$InvoiceDate, format="%m/%d/%Y %H:%M")

# Create a new column for the total purchase amount
online_retail_51$TotalAmount <- online_retail_51$Quantity * online_retail_51$UnitPrice

# Select the columns to be used for clustering
clustering_data_51 <- online_retail_51 %>%
  group_by(CustomerID) %>%
  summarise(TotalAmount = sum(TotalAmount)) %>%
  ungroup()

# Normalize the data
clustering_data_norm_51 <- scale(clustering_data_51$TotalAmount)

### Step 2: Clustering with k-means

# Determine the optimal number of clusters using the elbow method
set.seed(123)
wss <- sapply(1:10, function(k){kmeans(clustering_data_norm_51, k, nstart=10 )$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Choose the number of clusters based on the elbow method
num_clusters_51 <- 4

# Run k-means clustering
set.seed(123)
kmeans_clusters_51 <- kmeans(clustering_data_norm_51, centers=num_clusters_51, nstart=25)

## Visualize the clusters
library(cluster)
clusplot(clustering_data_norm_51, kmeans_clusters_51$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

### Step 3: Clustering with hierarchical clustering

# Determine the optimal number of clusters using the dendrogram
set.seed(123)
hc_dist_51 <- dist(clustering_data_norm_51, method = "euclidean")
hc_complete_51 <- hclust(hc_dist_51, method = "complete")
plot(hc_complete_51, cex = 0.6, hang = -1)

# Choose the number of clusters based on the dendrogram
num_clusters_51 <- 4

# Run hierarchical clustering
set.seed(123)
hc_clusters_51 <- cutree(hc_complete_51, k = num_clusters_51)

## Visualize the clusters
library(cluster)
clusplot(clustering_data_norm_51, hc_clusters_51, color=TRUE, shade=TRUE, labels=2, lines=0)

### Step 4: Generating rules with k-means

# Assign cluster labels to original data
clustering_data_51$cluster <- kmeans_clusters_51$cluster

# Compute cluster means
cluster_means_51 <- clustering_data_51 %>%
  group_by(cluster) %>%
  summarise(mean_total_amount = mean(TotalAmount)) %>%
  ungroup()

# Compute cluster size
cluster_sizes_51 <- clustering_data_51 %>%
  count(cluster)

# Generate rules for each cluster
cluster_rules_51 <- cluster_means_51 %>%
  left_join(cluster_sizes_51, by = "cluster") %>%
  mutate(support = n / nrow(clustering_data_51),
         confidence = mean_total_amount / max(mean_total_amount),
         lift = confidence / mean(support))

# Print the rules
print(cluster_rules_51)

### Step 5: Generating rules with hierarchical clustering

# Assign cluster labels to original data
clustering_data_51$cluster <- hc_clusters_51

# Compute cluster means
cluster_means_51 <- clustering_data_51 %>%
  group_by(cluster) %>%
  summarise(mean_total_amount = mean(TotalAmount)) %>%
  ungroup()

# Compute cluster size
cluster_sizes_51 <- clustering_data_51 %>%
  count(cluster)

# Generate rules for each cluster
cluster_rules_51 <- cluster_means_51 %>%
  left_join(cluster_sizes_51, by = "cluster") %>%
  mutate(support = n / nrow(clustering_data_51),
         confidence = mean_total_amount / max(mean_total_amount),
         lift = confidence / mean(support))

# Print the rules
print(cluster_rules_51)

