setwd("C:/Teaching/PhD_applied_statistics_F24/Inference/Data")



# Slide 5:
BWTdata <- read.csv2("data/01/lowbwt.csv")
head(BWTdata)

# Slide 13:
#PROBABILITY AS THE LIMIT FOR FREQUENCY
set.seed(22)
rep<-2500

coin <- sample(x=c(0,1), size=rep,replace=TRUE,prob=c(.50,.50))

sum<-numeric(rep)
sum[1] <- coin[1]
freq<-numeric(rep)
freq[1] <- coin[1]


for(i in 2:rep){
  sum[i]=sum[i-1]+coin[i]
  freq[i]=sum[i]/i
}
mean(coin)
plot(1:rep, freq, type="l", xlab = "Coin tosses",
     ylab = "Frequency", las=1)
#axis(2, at=c(0.25, 0.5, 0.75, 1), label = c(0.25, 0.50, 0.75, 1.00), las=1)
abline(h=0.5, lty=2, col="red")

# Slide 16:


install.packages("shape")
library(shape)

curve(dnorm(x),-3,3,lty=0,ylab='Density',xlab='x',xaxt="n")
axis(1)
axis(1,labels=c("a","b"),at=c(0.5,1.5),cex=0.2,col.axis="pink")
polygon(c(0.5,0.5,0.5+1:100/100,1.5),c(0,dnorm(0.5+0:100/100),0),col="pink",lty=0)
curve(dnorm(x),-3,3,add=T)
lines(c(-4,4),rep(0,2),type="l")
lines(rep(0.5,2),c(par("usr")[3],dnorm(0.5)),lty=3,lwd=2)
lines(rep(1.5,2),c(par("usr")[3],dnorm(1.5)),lty=3,lwd=2)
text(-1,dnorm(-1)+0.06,"f(x)",cex=1.5)
text(2,0.35,expression("P(a<X "<=" b)"),cex=1.5)
Arrows(1.25,0.34,1,0.28,arr.type="curved")



# Slide 17:

plot(BWTdata$BWT)

# Slide 22:
mean(BWTdata$BWT)
summary(BWTdata$BWT)
quantile(BWTdata$BWT,probs=seq(0,1,by=0.1),type=2)


# Slide 23:

install.packages("shape")
library(shape)

curve(dnorm(x),-3,3,lty=0,ylab='Density',xlab='x',xaxt="n")
axis(1)
axis(1,labels=c("a","b"),at=c(0.5,1.5),cex=0.2,col.axis="pink")
polygon(c(0.5,0.5,0.5+1:100/100,1.5),c(0,dnorm(0.5+0:100/100),0),col="pink",lty=0)
curve(dnorm(x),-3,3,add=T)
lines(c(-4,4),rep(0,2),type="l")
lines(rep(0.5,2),c(par("usr")[3],dnorm(0.5)),lty=3,lwd=2)
lines(rep(1.5,2),c(par("usr")[3],dnorm(1.5)),lty=3,lwd=2)
text(-1,dnorm(-1)+0.06,"f(x)",cex=1.5)
text(2,0.35,expression("P(a<X "<=" b)"),cex=1.5)
Arrows(1.25,0.34,1,0.28,arr.type="curved")



# Slide 24:
# NORMAL DENSITY
x <- seq(-3, 3, length=100)
hx <- dnorm(x)

plot(x, hx, type = "l", lty = 1, xlab="x value",
     ylab = "Normal Density")


# Slide 26:
set.seed(538)
u<-runif(2500)
hist(u, col="cyan",probability=T,breaks=20,ylim=c(0,1.5))
curve(dnorm(x,mean=mean(u),sd=sd(u)),0,1,add=T,col="red",lwd=2)

# Slide 27:
# CENTRAL LIMIT THEOREM
# U IS UNIFORM OVER 0 TO 1, NOT NORMAL

u<-matrix(u,ncol=1)
for(i in 2:30){
  u<-cbind(u,runif(2500))
  }

par(mfrow=c(2,2))
#AVERAGE OF 2 NON NORMAL VARIABLES
hist(rowMeans(u[,1:2]),col="cyan",probability=T,main="n=2",xlim=c(0,1))
curve(dnorm(x,mean=mean(rowMeans(u[,1:2])),sd=sd(rowMeans(u[,1:2]))),
      0,1,add=T,col="red",lwd=2)

#AVERAGE OF 5 NON NORMAL VARIABLES
hist(rowMeans(u[,1:5]),col="cyan",probability=T,main="n=5",xlim=c(0,1))
curve(dnorm(x,mean=mean(rowMeans(u[,1:5])),sd=sd(rowMeans(u[,1:5]))),
      0,1,add=T,col="red",lwd=2)

#AVERAGE OF 10 NON NORMAL VARIABLES
hist(rowMeans(u[,1:10]),col="cyan",probability=T,main="n=10",xlim=c(0,1))
curve(dnorm(x,mean=mean(rowMeans(u[,1:10])),sd=sd(rowMeans(u[,1:10]))),
      0,1,add=T,col="red",lwd=2)

#AVERAGE OF 30 NON NORMAL VARIABLES
hist(rowMeans(u[,1:30]),col="cyan",probability=T,main="n=30",xlim=c(0,1))
curve(dnorm(x,mean=mean(rowMeans(u[,1:30])),sd=sd(rowMeans(u[,1:30]))),
      0,1,add=T,col="red",lwd=2)
par(mfrow=c(1,1))
  

#slide 28:

hist(BWTdata$BWT,probability=T,col="blue",xlab="Birth weight",
     main="Histogram with Normal curve")
curve(dnorm(x,mean=mean(BWTdata$BWT),sd=sd(BWTdata$BWT)),
      min(BWTdata$BWT),max(BWTdata$BWT),add=T,col="red",lwd=3)


# Slide 29:

qqnorm(BWTdata$BWT)
lines((-3):3,((-3):3)*sd(BWTdata$BWT)+mean(BWTdata$BWT),
  type="l",col="red",lwd=2)


# slide 30:

par(mfrow=c(1,2))
hist(BWTdata$BWT,probability=T,col="blue",xlab="Birth weight",
     main="Histogram with Normal curve",ylim=c(0,7e-4))
curve(dnorm(x,mean=mean(BWTdata$BWT),sd=sd(BWTdata$BWT)),
      min(BWTdata$BWT),max(BWTdata$BWT),add=T,col="red",lwd=3)
qqnorm(BWTdata$BWT)
lines((-3):3,((-3):3)*sd(BWTdata$BWT)+mean(BWTdata$BWT),
  type="l",col="red",lwd=2)
par(mfrow=c(1,1))


# slide 32 exersice 1 ####

# Set your working directory to where you keep your data for today.
# Load the cars dataset mtcars.txt
mtcars <- read.delim("data/01/mtcars.txt")
# Describe the data
head(mtcars)
dim(mtcars)
# Make plots of the variable miles per gallon, mpg.
hist(mtcars$mpg)
# Calculate summary statistics for mpg.
summary(mtcars$mpg)
summary(mtcars)
# Where would we expect most of the observations to be found?
# median is 19.20, mean is 20.09

# Calculate IQR and 0.025, 0.975 percentiles.
quantile(mtcars$mpg,probs=seq(0,1,by=0.025),type=2)
IQR <- quantile(mtcars$mps, probs = 0.75, type = 2)
IQR

# slide 33 exersice 2 ####
# Load the cars data set mtcars.txt.
# can we assume that miles per gallon are normally distributed?
qqnorm(mtcars$mpg)
lines((-3):3,((-3):3)*sd(mtcars$mpg)+mean(mtcars$mpg),
      type="l",col="red",lwd=2)
# The variable am is 0 for cars with automatic transmission and 1 for cars with manual. Make a boxplot for the two levels of am.
boxplot(mtcars$am,
        xlab="atomatic transmission")
str(mtcars)
mtcars$am <- factor(mtcars$am)
str(mtcars)

boxplot(mpg ~ am, data = mtcars, las = 1,
        ylab = "Mile pr gallon", col = 2:3)
# can we assume that miles per gallon are normally distributed for each level of am?


# Slide 37:

Y<-BWTdata$BWT; mean(Y);var(Y)

# Slide 39:

SEM<-sd(BWTdata$BWT)/sqrt(length(BWTdata$BWT))
SEM

# slide 44:
my.colors<-c("red","blue","dark green","yellow")
my.df<-c(1,3,8,30)
curve(dnorm,-4,4,ylab='Density',lwd=2)
for(i in 1:4){
curve(dt(x,df=my.df[i]),-4,4,add=T,col=my.colors[i],lwd=2)
}

legend("topright",c(paste("df=",my.df),"normal"),col=c(my.colors,"black"),
       lty=1,lwd=2)

# Slide 45:

2*pt(-1.0437,188)

# slide 46:

curve(dnorm(x),-3,3,lty=0,ylab='Density',xlab='x',xaxt="n")
axis(1)
polygon(c(-4,-4,-4+(1:100/100)*(4-1.0437),-1.0437),
        c(0,dnorm(-4+(0:100/100)*(4-1.0437)),0),col="pink",lty=0)
polygon(c(4,4,4+(1:100/100)*(-4+1.0437),1.0737),
        c(0,dnorm(4+(0:100/100)*(-4+1.0437)),0),col="pink",lty=0)
curve(dnorm(x),-3,3,add=T)
lines(c(-4,4),rep(0,2),type="l")
lines(rep(-1.0437,2),c(par("usr")[3],dnorm(-1.0437)),lty=3,lwd=2)
lines(rep(1.0437,2),c(par("usr")[3],dnorm(1.0437)),lty=3,lwd=2)
axis(1,labels=c("t","-t"),at=c(-1.0437,1.0437),cex=0.2,col.axis="red")


# Slide 49:
t.test(BWTdata$BWT,mu=3000)


# Slide 50:
t.test(BWTdata$BWT,mu=2840.05)
t.test(BWTdata$BWT,mu=3049.264)

# Slide 56:
plot.power<-matrix(nrow=380,ncol=3)
for(i in 1:380){
  plot.power[i,1]<-power.t.test(power=0.6+i/1000,delta=150, sd=700,type="one.sample")$n
  plot.power[i,2]<-power.t.test(power=0.6+i/1000,delta=200, sd=700,type="one.sample")$n
  plot.power[i,3]<-power.t.test(power=0.6+i/1000,delta=250, sd=700,type="one.sample")$n
  }
plot(rep(0.6+(1:380)/1000,3),plot.power,pch="",
     xlab='Power\n SD=700 and alpha=0.05',ylab='Sample size')
for(i in 1:3){
 lines(0.6+(1:380)/1000,plot.power[,i],type="l",col=i,lwd=2)
  }
legend("topleft",paste("Delta=",c(150,200,250)),col=1:3,lty=1)

# Slide 57:
plot.power<-matrix(nrow=801,ncol=3)
for(i in 1:801){
  plot.power[i,1]<-power.t.test(delta=i-401,n=50, sd=700,type="one.sample")$power
  plot.power[i,2]<-power.t.test(delta=i-401,n=100, sd=700,type="one.sample")$power
  plot.power[i,3]<-power.t.test(delta=i-401,n=150, sd=700,type="one.sample")$power
  }
plot(rep(1:801-401,3),plot.power,pch="",
     xlab='Delta\n SD=700 and alpha=0.05',ylab='Power')
for(i in 1:3){
 lines(1:801-401,plot.power[,i],type="l",col=i,lwd=2)
  }
legend("bottomright",paste("n=",c(100,200,300)),col=1:3,lty=1)

# slide 58:
plot.power<-matrix(nrow=401,ncol=3)
for(i in 20:401){
  plot.power[i,1]<-power.t.test(power=0.7,n=i, sd=700,type="one.sample")$delta
  plot.power[i,2]<-power.t.test(power=0.8,n=i, sd=700,type="one.sample")$delta
  plot.power[i,3]<-power.t.test(power=0.9,n=i, sd=700,type="one.sample")$delta
  }
plot(rep(20:401,3),plot.power[20:401,],pch="",
     xlab='n\n SD=700 and alpha=0.05',ylab='Delta')
for(i in 1:3){
 lines(20:401,plot.power[20:401,i],type="l",col=i,lwd=2)
  }
legend("topright",paste("Power=",c(0.7,0.8,0.9)),col=1:3,lty=1,lwd=2)

# Slide 60:

power.t.test(power=0.8,delta=250,sd=750 , type="one.sample")

# Slide 61:

power.t.test(n=150,delta=100,sd=750 , type="one.sample")

# Slide 62:

power.t.test(n=150, power=0.8, sd=750 , type="one.sample")

# slide 65 exercise 5

# In a one-sample setting with α= 0.05:
# Calculate the sample size to get a power of 80% when trying to detect a difference of 2, when SD=6 is expected.
power.t.test(delta = 2, power=0.8, sd=6, type="one.sample")
# n = 72.58407

# Calculate the power in a study planned to include 40 subjects, if we want to detect a dierence of 3 and expect SD=6.
power.t.test(n=40,delta=3,sd=6 , type="one.sample")
# power = 0.8693979

# What dierence can we detect in a study with power of 80% , 45 subjects and SD=4?
power.t.test(n=45, power=0.8, sd=4 , type="one.sample")
# delta = 1.708128

