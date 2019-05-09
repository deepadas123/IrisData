#1. IRIS data
data("iris")  

#1. Random sample a training data set 
#training data
set.seed(12462210)
iris_train<-iris[sample(x=nrow(iris), size=nrow(iris)*0.80),]
summary(iris_train)
#testing data
set.seed(12462210)
iris_test<-iris[c(1:nrow(iris))[! c(1:nrow(iris)) %in% train_id],]
summary(iris_test)
#Exploratory Data Analysis: Iris Data - Training Data ##################################################
##Histogam & density function
summary(iris_train)
hist(iris_train$Sepal.Length)
#Sepal.Length###################updated 1-27-19
hist(iris_train$Sepal.Length, prob=T, col="green", breaks=20, main="Histogram and Density of Sepal Length", xlim=c(3,9), xlab="Sepal Length")
lines(density(iris$Sepal.Length), col="red", lwd=2)
# Add a vertical line that indicates the average of Sepal Length
abline(v=mean(iris_train$Sepal.Length), col="blue", lty=2, lwd=1.5)
#Sepal.Width###################
hist(iris_train$Sepal.Width, prob=T, col="yellow", breaks=20, main="Histogram and Density of Sepal Width", xlim=c(1,5), xlab="Sepal Width")
lines(density(iris_train$Sepal.Width), col="blue", lwd=2)
# Add a vertical line that indicates the average of Sepal width
abline(v=mean(iris$Sepal.Width), col="red", lty=2, lwd=1.5)

###scatter matrix
plot(iris_train)
pairs(iris_train[,1:4])
####### KNN - k nearest neighbor#######################
install.packages("class")
library(class) #load the package
iris_test$Species
knn_iris <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=10)
knn_iris
#prediction Accuracy
table(iris_test[,5], knn_iris, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris)

knn_iris <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=5)
knn_iris
#prediction Accuracy
table(iris_test[,5], knn_iris, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris)

knn_iris <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=3)
knn_iris
#prediction Accuracy
table(iris_test[,5], knn_iris, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris)

knn_iris <- knn(train = iris_train[, -5], test = iris_test[, -5], cl=iris_train[,5], k=2)
knn_iris
#prediction Accuracy
table(iris_test[,5], knn_iris, dnn = c("True", "Predicted"))
sum(iris_test[,5] != knn_iris)

####### K- means clustering: UnSupervised learning #######################
install.packages("fpc")
library(fpc) #load the package

set.seed(12462210)
fit10 <- kmeans(x=iris_train[,1:4], centers=10)
plotcluster(iris_train[,1:4], fit10$cluster)
#Hierarchical clustering
hc_result <- hclust(dist(iris_train[,1:4]))
plot(hc_result)
#Cut Dendrogram into 10 Clusters
rect.hclust(hc_result, k=10)

set.seed(12462210)
fit8<- kmeans(x=iris_train[,1:4], centers=8)
plotcluster(iris_train[,1:4], fit8$cluster)
#Hierarchical clustering
hc_result <- hclust(dist(iris_train[,1:4]))
plot(hc_result)
#Cut Dendrogram into 5 Clusters
rect.hclust(hc_result, k=8)



set.seed(12462210)
fit5<- kmeans(x=iris_train[,1:4], centers=5)
plotcluster(iris_train[,1:4], fit5$cluster)
#Hierarchical clustering
hc_result <- hclust(dist(iris_train[,1:4]))
plot(hc_result)
#Cut Dendrogram into 5 Clusters
rect.hclust(hc_result, k=5)


set.seed(12462210)
fit4 <- kmeans(x=iris_train[,1:4], centers=4)
plotcluster(iris_train[,1:4], fit4$cluster)
#Hierarchical clustering
hc_result <- hclust(dist(iris_train[,1:4]))
plot(hc_result)
#Cut Dendrogram into 4 Clusters
rect.hclust(hc_result, k=4)

set.seed(12462210)
fit3 <- kmeans(x=iris_train[,1:4], centers=3)
plotcluster(iris_train[,1:4], fit3$cluster)
#Hierarchical clustering
hc_result <- hclust(dist(iris_train[,1:4]))
plot(hc_result)
#Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k=3)


set.seed(12462210)
fit2 <- kmeans(x=iris_train[,1:4], centers=2)
plotcluster(iris_train[,1:4], fit2$cluster)
#Hierarchical clustering
hc_result<- hclust(dist(iris_train[,1:4]))
plot(hc_result)
#Cut Dendrogram into 2 Clusters
rect.hclust(hc_result, k=2)

