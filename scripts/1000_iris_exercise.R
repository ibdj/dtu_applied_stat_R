#Iris data The dataset “Fishers Iris data” is aclassical dataset used in many examples. The dataset consists of 150 observations/objects, 50 Iris Setosa, 50 Iris versicolor and 50 Iris virginica. The flowers on these plant have been measured (mm). The measured variables are sepal length, sepal width, petal length and petalwidth (4 variables). The original hypothesis was that versicolor was a hybrid of the two other species.   
#1.Read in the data iris.txt. 
iris <- read.delim("data/iris.txt")
#2.Make descriptive plots.
par(mfrow = c(4,4))
plot(iris)
str(iris)

par(mfrow = c(2,2))
for (i in 1:5) boxplot(iris[,i] ~ iris$Species, col = 1:3, 
                        main=paste(names(iris)[i]))

#3.Carry out a PCA on scaled data.

iris_subset <- iris[, 1:4]
round(var(iris_subset),digits=2)
round(var(scale(iris_subset)),digits=2)
X<-var(scale(iris_subset))
X
sum(diag(X))
T<-eigen(X)$vectors
T
Lambda<-t(T)%*%X%*%T
Lambda
#a. How many principal components are need for an adequate description of  the variation in data? 
#3???
plot(100*(4-cumsum(diag(Lambda)))/4,type="b",main="Percentage Variance Unexplained",
     xlab='Number of eigenvectors included',
     ylab='Percentage of total variance')
#b.Describe how you chose the number of principal components. 
#after 3, there is no further decrease in the unexplained variance.

#4.How can the principal components be interpreted? 
iris.PC<- PCA(scale(iris_subset))

head(iris.PC$loadings,n=3)
head(T,n=3)
head(iris.PC$scores,n=3)
head(scale(iris_subset)%*%T,n=3)

iris.PC$var
iris.PC$totalvar
iris.PC$centered.data

plot(1:4,iris.PC$var,xlab="PC",ylab="var")

scoreplot(iris.PC, col = vintages, pch= as.numeric(vintages), lwd=2)

#5.Can the principal components be used to distinguish between the sorts? 
#6.Investigate if the model fits well: look at residuals and influential observations (leverage)