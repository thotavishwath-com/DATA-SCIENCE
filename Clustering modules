#10) CLUSTERING MODEL
#a) Clustering algorithms for unsupervised classification.
#b) Plot the cluster data using R visualizations.

#load dataset
df<-iris
df
# display first 6 rows
head(df)

#EDA
library(ggplot2)
ggplot(df,aes(x=Petal.Length,y=Petal.Width))+geom_point(aes(col=Species),size=4)

ggplot(df,aes(x=Sepal.Length,y=Sepal.Width))+geom_point(aes(col=Species),size=3)

ggplot(df,aes(Petal.Length,Sepal.Length))+geom_point(aes(col=Species),size=3)

ggplot(df,aes(x=Petal.Width,y=Sepal.Width))+geom_point(aes(col=Species),size=3)

# Model Build
set.seed(101)
k=3 # no of clusters
iriscluster<-kmeans(df[,1:4],centers = 3,nstart = 25)
iriscluster
table(iriscluster$cluster,df$Species)
# clusterplot
library(cluster)
clusplot(iris, iriscluster$cluster, color=T, shade=T, labels=0, lines=2)


k=4 # no of clusters
iriscluster<-kmeans(df[,1:4],centers = 4,nstart = 25)
iriscluster
table(iriscluster$cluster,df$Species)
# clusterplot
library(cluster)
clusplot(iris, iriscluster$cluster, color=T, shade=T, labels=0, lines=0)

k=5 # no of clusters
iriscluster<-kmeans(df[,1:4],centers = 5)
iriscluster
table(iriscluster$cluster,df$Species)
# clusterplot
library(cluster)
clusplot(iris, iriscluster$cluster, color=T, shade=T, labels=0, lines=0)

iriscluster$cluster
iriscluster$centers
iriscluster$totss
iriscluster$size

clusters <- hclust(dist(iris[, 1:2]))
plot(clusters)

clusters <- hclust(dist(iris[, 2:3]))
plot(clusters)

clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)
