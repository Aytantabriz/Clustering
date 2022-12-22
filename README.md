# Clustering
K-means and PCA analysis

## K-Means Intuition

**STEP 1:** Choose the number K of clusters
![Elbow method]()
```
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
```
**STEP 2:** Select at random K points, the centroids (not necessarily from your dataset)

**STEP 3:** Assign each data point to the closes centroid That forms K clusters

**STEP 4:** Compute and place the new centroid of each cluster

**STEP 5:** Reassign each data point to the new closest centroid.

If any reassignment took place, go to **STEP4**, otherwise, go to FIN

![Clusters of customers]()


## PCA
To interpret each component, we need to calculate the relationships between the original data and each principal component.
These relationships are obtained by the correlation procedure. In addition to all nine of the original variables, we include the first three principal components, "prin1, prin2, prin3, and prin4." We use the relationships between the principal components and the original variables to interpret these principal components

![Correlation plot for PCA]()

![PCA result]()
