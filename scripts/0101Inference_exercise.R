
# EXERCISE 1

# SET WORKING DIRECTORY
setwd("C:/Users/ANST/Undervisning/Kurser/phd kursus i basal statistik Jan25/Inferens og t-tests/Inferens")

# READ IN
Cars <- read.delim("Data/mtcars.txt")

#DESCRIBE
summary(Cars)

str(Cars)

#SUMMARY STATS FOR mpg
summary(Cars$mpg)
sd(Cars$mpg)

# PLOTS FOR mpg
hist(Cars$mpg)

boxplot(Cars$mpg, horizontal = TRUE, xlab="Miles per Gallon", col=5)
rug(Cars$mpg)

# IF WE HAVE NORMAL DATA WE WOULD EXPECT MOST BETWEEN

mean(Cars$mpg) - 1.96*sd(Cars$mpg)
mean(Cars$mpg) + 1.96*sd(Cars$mpg)

#HISTOGRAM WITH NORMAL DISTRIBUTION
x<-Cars$mpg
hist(x, col="red", xlab="Miles per Gallon",probability=T,
        main="Histogram with Normal Curve", las=1, ylim=c(0,0.08))
curve(dnorm(x,mean=mean(x),sd=sd(x)),min(x),max(x),col="blue",lwd=3, add=T)

#PERCENTILES
quantile(Cars$mpg, probs = seq(0, 1, by = 0.1), type=2)
IQR <- quantile(Cars$mpg, probs = 0.75, type=2)-
  quantile(Cars$mpg, probs = 0.25, type=2)
IQR

quantile(Cars$mpg, probs = c(0.025,0.975), type=2)

##############################
# EXERCISE 2

Cars_auto <- Cars[Cars$am==0,]
Cars_manual <- Cars[Cars$am==1,]


qqnorm(Cars$mpg)
lines((-3):3,((-3):3)*sd(Cars$mpg)+mean(Cars$mpg),
  type="l",col="red",lwd=2)


par(mfrow=c(1,2))
qqnorm(Cars_auto$mpg,main="Q-Q plot, auto")
lines((-3):3,((-3):3)*sd(Cars_auto$mpg)+mean(Cars_auto$mpg),
  type="l",col="red",lwd=2)


qqnorm(Cars_manual$mpg,main="Q-Q plot, manual")
lines((-3):3,((-3):3)*sd(Cars_manual$mpg)+mean(Cars_manual$mpg),
  type="l",col="red",lwd=2)
par(mfrow=c(1,1))



#################
# EXERCISE 3

#Calculate the mean and SD for miles per gallon (mpg)

Mean <- mean(Cars$mpg)
SD <- sd(Cars$mpg)

n <- length(Cars$mpg)
SEM <- SD/sqrt(n)

#Calculate the 95% confidence interval for mean mpg.

qt(0.975,df=n-1)

error <- qt(0.975,df=n-1)*sd(Cars$mpg)/sqrt(n)
left <- mean(Cars$mpg)-error
right <- mean(Cars$mpg)+error
left
right

# THE CONFIDENCE INTERVAL 17.9 TO 22.3
# IS WHERE WE EXPECT THE TRUE MEAN TO BE FOUND

##################
# EXERCISE 4

#Test the hypothesis that the mean mpg is 22
t.test(Cars$mpg, mu = 22)
# WE GET A P VALUE OF 0.08>0.05 SO WE ACCEPT
# H0: mu=22.

#Which values would have been acceptable at a 5% significance level?

# THE VALUES OF THE CONFIDENCE INTERVAL
#ARE THE ACCEPTABLE VALUES 
# I.E. 17.9 to 22.3 AND 25 IS NOT ACCEPTABLE

#Which values would have been acceptable at a 1% significance level?
# WE NEED TO CALCULATE THE 99% CONFIDENCE
# INTERVAL

qt(0.995,df=n-1)

error1 <- qt(0.995,df=n-1)*sd(Cars$mpg)/sqrt(n)
left1 <- mean(Cars$mpg)-error1
right1 <- mean(Cars$mpg)+error1
left1
right1

# SO THE ACCEPTABLE VALUES ARE
# 17.2 to 23.0

#ALTERNATIVE
t.test(Cars$mpg, mu = 22, conf.level=0.99)

#################################3
# POWER CALCULATIONS

#SAMPLE SIZE FOR DELTA =2, SD=6 AND POWER=0.8
power.t.test(power = 0.80, delta = 2, sd = 6,
             type = "one.sample")

#WE GET N=72.58 I.E. 73

#POWER FOR DELTA =3, SD=6 AND n=40
power.t.test(n = 40, delta = 3, sd = 6,
             type = "one.sample")

# WE GET POWER=0.87

# DELTA
power.t.test(n = 45, power = 0.80, sd = 4,
             type = "one.sample")

# WE GET DELTA=1.71
