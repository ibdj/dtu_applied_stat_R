# setwd(<your favorite directory>)

# Slide 6:
# reading in data:
load("data/10/Winedata.Rdata")

# two objects: wines, with characteristics, and vintages the wine type.
ls(wines)

#Slwines#Slide 7:
par(mfrow = c(4,4))
for (i in 1:13) boxplot(wines[,i] ~ vintages, col = 2:4, 
                        main=paste(names(wines)[i]))
par(mfrow = c(1,1))

# Slide 8:

library(car)
rownames(iris) = iris$Species

#slide 10:
# looking at variation:
round(var(iris),digits=2)

# Slide 11:
round(var(scale(wines)),digits=2)

# Slide 12:

# similar scale for all observatiosen
X<-var(scale(wines))

sum(diag(X))


# Slide 14:
T<-eigen(X)$vectors

# The inverse of T is equal to the matrix transpose t(T):

Lambda<-t(T)%*%X%*%T

round(Lambda, digits=2)

# Slide 15:

T[,1]

# Slide 16:

round(T[,1],digits=2)

# Slide 17:

sum(diag(Lambda))

# Slide 18:


plot(100*(13-cumsum(diag(Lambda)))/13,type="b",main="Percentage Variance Unexplained",
      xlab='Number of eigenvectors included',
      ylab='Percentage of total variance')

data.frame("Eigenvectors"=1:13,"Variance Explained"=round(100*(cumsum(diag(Lambda)))/13))


# Slide 23:
install.packages("remotes")
library(remotes)
install_github("rwehrens/ChemometricsWithR")
library(ChemometricsWithR)
wines.PC<- PCA(scale(wines))
names(wines.PC)
summary(wines.PC)

# Slide 24:

head(wines.PC$loadings,n=3)
head(T,n=3)

# Slide 25:

head(wines.PC$scores,n=3)
head(scale(wines.PC)%*%T,n=3)

# Slide 26:

wines.PC$var
wines.PC$totalvar
wines.PC$centered.data

# Slide 27:

plot(1:13,wines.PC$var,xlab="PC",ylab="var")


# Slide 28:

scoreplot(wines.PC, col = vintages, pch= as.numeric(vintages), lwd=2)
legend("bottomright",levels(vintages), col=1:3,pch=1:3)

# Slide 29:
loadingplot(wines.PC, show.names= TRUE)

# Slide 30:

par(mfrow=c(1,2))
loadingplot(wines.PC, pc=c(1,3), show.names= TRUE)
loadingplot(wines.PC, pc=c(2,3), show.names= TRUE)
par(mfrow=c(1,1))

# Slide 31:

biplot(wines.PC, score.col = vintages, show.names = "loadings")
legend("bottomright",levels(vintages), col=1:3,pch=1:3)

# slide 32:
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(prcomp(scale(wines)),groups=vintages,ellipse=T)

# Slide 34:
install.packages("chemometrics")
library(chemometrics)
wines.PCA<- princomp(wines, cor = TRUE)
res<-pcaDiagplot(wines, wines.PCA, a=3)

# Slide 35:

par(mfrow=c(1,1))
plot(res$SDist, res$ODist, type="n")
text(res$SDist, res$ODist, labels=as.character(1:178))


# Slide 38:

jam<-read.table("data/10/jam.txt", header=TRUE, quote="\"")

# first column is names, last column is outcome:

pca.1<- PCA(scale(jam[ ,-c(1,14)]))

summary(pca.1)

# Slide 39:

plot(1:length(pca.1$var), pca.1$var, type="b")

# Slide 40:

loadingplot(pca.1, show.names= TRUE)

# Slide 41:

par(mfrow=c(1,2))
loadingplot(pca.1, pc=c(1,3), show.names= TRUE)
loadingplot(pca.1, pc=c(2,3), show.names= TRUE)
par(mfrow=c(1,1))

# Slise 42:

loadings(pca.1)

# Slide 45:

scores<-pca.1$scores
# leaving out an outlier:
analysis<-lm(jam$PREFEREN[-12] ~  . , as.data.frame(scores[-12 ,1:3]))

par(mfrow=c(2,2))
plot(analysis,which=1:4)
par(mfrow=c(1,1))


# Slide 46:
drop1(analysis,test="F")

analysis<-lm(jam$PREFEREN ~  . , as.data.frame(scores[,2:3]))
drop1(analysis,test="F")

# Slide 47:

summary(analysis)$coef


# Slide 49:


install.packages("jpeg")
library(jpeg)

horse <- readJPEG("data/horse.jpg")

ncol(horse)
## [1] 480
	
nrow(horse)
## [1] 341

#480*341 pixels - 480*341*3=491.040 number


# array with 3 layers
str(horse)



# Slide 51:

str(horse) 

red  <- horse[,,1]
green<- horse[,,2]
blue <- horse[,,3]

horse.red.pca <-  prcomp(red, center = FALSE)
horse.green.pca<- prcomp(green, center = FALSE)
horse.blue.pca <- prcomp(blue, center = FALSE)

# Gather PCA object sin one list:
	
rgb.pca <- list(horse.red.pca, horse.green.pca, horse.blue.pca)



# Slide 52:

index<-c(3,6,9,12,15,18,50,100)
# function for reconstruction from pc (scores):

my.reconstruct<-function(j) {
    return(j$x[,1:i] %*% t(j$rotation[,1:i]))
  }
# reconstructs and writes to disc:

for (i in index) {
  pca.picture <- sapply(rgb.pca,my.reconstruct,simplify = 'array')
  writeJPEG(pca.picture, 
    paste("picture/horse_compressed_", i, "_components.jpg", sep =""))
  }

