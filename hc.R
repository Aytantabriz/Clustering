# Hierarchical Clustering

# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')
install.packages('ape')
library(ape)
plot(as.phylo(hc), type = "cladogram", cex = 0.9, label.offset = 1)
plot(as.phylo(hc), type = "fan")
install.packages('sparcl')
library(sparcl)
ColorDendrogram(hc, y = y_hc, labels = names(y_hc), main = "My Simulated Data", 
                branchlength = 80)

#install.packages('ggdendro')
library(ggdendro)
ggdendrogram(hc, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")


source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(dendrogram, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("#FF6B6B", 
                                                                  "#4ECDC4", "#556270"))



