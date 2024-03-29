### 1.1 Unsupervised learning in R

# Create the k-means model:
 
km.out <- kmeans(x, centers = 3, nstart = 20)


# Inspect the result
summary(km.out)



# Print the cluster membership component of the model
km.out$cluster

# Print the km.out object
km.out



# Scatter plot of x
plot(x, 
  col = km.out$cluster,
  main = "k-means with 3 clusters",
  xlab = "",
  ylab = "")
  
  
  
# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))

# Set seed
set.seed(1)

for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(x, centers = 3, nstart = 1)
  
  # Plot clusters
  plot(x, col = km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
}



# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Set k equal to the number of clusters corresponding to the elbow location
k <- 2



# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters (2, 3, 4 probably OK)
k <- 3

# Build model with k clusters: km.out
km.out <- kmeans(pokemon, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")
     
### 1.2 Hierarchical clustering

# Create hierarchical clustering model: hclust.out
hclust.out <- hclust(dist(x))

# Inspect the result
summary(hclust.out)



# Cut by height
cutree(hclust.out, h = 7)

# Cut by number of clusters
cutree(hclust.out, k = 3)



# Cluster using complete linkage: hclust.complete
hclust.complete <- hclust(dist(x), method = "complete")

# Cluster using average linkage: hclust.average
hclust.average <- hclust(dist(x), method = "average")

# Cluster using single linkage: hclust.single
hclust.single <- hclust(dist(x), method = "single")

# Plot dendrogram of hclust.complete
plot(hclust.complete, main = "Complete")

# Plot dendrogram of hclust.average
plot(hclust.average, main = "Average")

# Plot dendrogram of hclust.single
plot(hclust.single, main = "Single")



# View column means
colMeans(pokemon)

# View column standard deviations
apply(pokemon, 2, sd)

# Scale the data
pokemon.scaled <- scale(pokemon)

# Create hierarchical clustering model: hclust.pokemon
hclust.pokemon <- hclust(dist(pokemon.scaled), method = "complete")



# Apply cutree() to hclust.pokemon: cut.pokemon
cut.pokemon <- cutree(hclust.pokemon, k = 3)

# Compare methods
table(km.pokemon$cluster, cut.pokemon)

### 1.3 Dimensionality reduction with PCA

# Perform scaled PCA: pr.out
pr.out <- prcomp(pokemon, scale = TRUE)

# Inspect model output
summary(pr.out)



# Variability of each principal component: pr.var
pr.var <- pr.out$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)
pve



# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
     
     
     
 # Mean of each variable
colMeans(pokemon)

# Standard deviation of each variable
apply(pokemon, 2, sd)

# PCA model with scaling: pr.with.scaling
pr.with.scaling <- prcomp(pokemon, scale = TRUE)

# PCA model without scaling: pr.without.scaling
pr.without.scaling <- prcomp(pokemon, scale = FALSE)

# Create biplots of both for comparison
biplot(pr.with.scaling)
biplot(pr.without.scaling)




# Mean of each variable
colMeans(pokemon)

# Standard deviation of each variable
apply(pokemon, 2, sd)

# PCA model with scaling: pr.with.scaling
pr.with.scaling <- prcomp(pokemon, scale = TRUE)

# PCA model without scaling: pr.without.scaling
pr.without.scaling <- prcomp(pokemon, scale = FALSE)

# Create biplots of both for comparison
biplot(pr.with.scaling)
biplot(pr.without.scaling)

### 1.4 Putting it all together with a case study


url <- "https://assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Download the data: wisc.df
wisc.df <- read.csv(url)
str(wisc.df)

# Convert the features of the data: wisc.data
wisc.data <- as.matrix(wisc.df[, 3:32])
str(wisc.data)

# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id
names(wisc.data)

# Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")



# Check column means and standard deviations
colMeans(wisc.data)
apply(wisc.data, 2, sd)

# Execute PCA, scaling if appropriate: wisc.pr
wisc.pr <- prcomp(wisc.data, scale = TRUE)

# Look at summary of results
summary(wisc.pr)



# Create a biplot of wisc.pr
biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

# Do additional data exploration of your choosing below (optional)
plot(wisc.pr$x[, c(2, 3)], col = (diagnosis + 1), 
     xlab = "PC2", ylab = "PC3", main = "Optional PCA Analysis")
     
     
 
 # Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wisc.pr$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")
     
     
     
# Scale the wisc.data data: data.scaled
head(wisc.data)
# scale
data.scaled <- scale(wisc.data)
head(data.scaled)

# Calculate the (Euclidean) distances: data.dist
data.dist <- dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust <- hclust(data.dist, method = "complete")



# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters <- cutree(wisc.hclust, k = 4)

# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, diagnosis)



# Create a k-means model on wisc.data: wisc.km
# sum row mins
sum(apply(table(wisc.hclust.clusters, diagnosis), 1, min))
#  54 tumors that are not clear with the diagnosis based on the general cluster

# Compare k-means to actual diagnoses
wisc.km <- kmeans(scale(wisc.data), centers = 2, nstart = 20)
table(wisc.km$cluster, diagnosis)
sum(apply(table(wisc.km$cluster, diagnosis), 1, min))
# 51

# Compare k-means to hierarchical clustering
table(wisc.hclust.clusters, wisc.km$cluster)
sum(apply(table(wisc.hclust.clusters, wisc.km$cluster), 1, min))
# 37



# Create a hierarchical clustering model: wisc.pr.hclust
wisc.pr.hclust <- hclust(dist(wisc.pr$x[, 1:7]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k = 4)

# Compare to actual diagnoses
table(diagnosis, wisc.pr.hclust.clusters)

# Compare to k-means and hierarchical
table(diagnosis, wisc.hclust.clusters)
table(diagnosis, wisc.km$cluster)
