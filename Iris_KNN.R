
set.seed(12927356) # required to reproduce the results
rnum<- sample(rep(1:150)) # randomly generate numbers from 1 to 150
rnum
iris<- iris[rnum,] #randomize "iris" dataset
head(iris)


normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new<- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
head(iris.new)



iris.train<- iris.new[1:120,]
iris.train.target<- iris[1:120,5]
iris.test<- iris.new[121:150,]
iris.test.target<- iris[121:150,5]
summary(iris.new)
head(iris.train.target)


model5<- knn(train=iris.train, test=iris.test, cl=iris.train.target, k=10)

table(iris.test.target, model5,dnn = c("True", "Predicted"))
