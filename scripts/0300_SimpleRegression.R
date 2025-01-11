setwd("C:/ANST/Undervisning/Kurser/phd kursus i basal statistik Jun2024/Regression/Simple/Data")


# Slide 8:
crime <- read.delim("data/us_statewide_crime.txt")

head(crime)

# Slide 9:
plot(crime$college, crime$murder.rate, xlab="Percentage with college education",
     ylab="Murder rate", las=1, cex = 1.5, col = "blue", lwd = 2)

# Slide 16:
reg1 <- lm(murder.rate ~  college, data = crime)
summary(reg1)

#Slide 17:
plot(murder.rate ~ college, data = crime, las=1)
abline(reg1)

#Slide 19:

x <- crime$college
y <- crime$murder.rate
(beta <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2))

(alpha <- mean(y)- beta*mean(x))

Fitted <- alpha + beta*x
Resid <- y - Fitted
sigma2 <- sum(Resid^2)/(length(y)-2)
sqrt(sigma2)

# Slide 22:

my.t<-beta/(sqrt(sigma2)/sqrt(sum((x-mean(x))^2)))
2*(1.pt(my.t,49))

#Slide 23:


# TO USE FOR REPORT
confint(reg1)
#Nice table
tab <- cbind(coef(summary(reg1))[ , 1:2], "Lower" = confint(reg1)[ , 1],
             "Upper" = confint(reg1)[ , 2])
tab
#Nice table with p-values
data.frame(round(tab, 2),
           "p-value" = format.pval(coef(summary(reg1))[ , 4], digits = 3, eps = 1e-3))

# slide 27
# residual plots

#Slide 29:

par(mfrow=c(2,2))
plot(reg1, which=1:4)
par(mfrow=c(1,1))

# cooks distance: not good indicator if above 1 

# Slide 31:

crime50 <- crime[-9, ]

reg2 <- lm(murder.rate ~  college, data = crime50)

#Slide 32:

par(mfrow=c(2,2))
plot(reg2, which=1:4)
par(mfrow=c(1,1))

# Slide 33:

#checking linearity
plot(residuals(reg2) ~ college, data=crime50)
abline(h = 0, lty = 2)
#no structure; that is good

# Slide 35:

residualPlots(reg2)

# Slide 36:

summary(reg2)

# Slide 37:

tab <- cbind(coef(summary(reg2))[ , 1:2], 
             "Lower" = confint(reg2)[ , 1],
             "Upper" = confint(reg2)[ , 2])
data.frame(round(tab, 2),
           "p-value" = format.pval(coef(summary(reg2))[ , 4], 
           digits = 3, eps = 1e-3))


# Slide 38:

plot(crime50$college,crime50$murder.rate,xlab='college',ylab='murder rate')
abline(reg1,lty=2,lwd=3)
abline(reg2,lwd=3,col="blue")


# Slide 43:

xval <- seq(from = 15, to = 35, length.out = 500)
newData <- data.frame(college = xval)
Pred.ci <- predict(reg2, newdata = newData,
interval = "confidence",
level = .95)
## Plot data, model and intervals:
plot(murder.rate ~ college, data = crime50, pch = 20, las = 1)
lines(xval, Pred.ci[, "fit"], lwd = 2) ## or use: abline(reg2)
lines(xval, Pred.ci[, "lwr"], lty = 2, col = "red", lwd = 2)
lines(xval, Pred.ci[, "upr"], lty = 2, col = "red", lwd = 2)

# Slide 46:


Pred.pi <- predict(reg2, newdata = newData,
interval = "prediction")
lines(xval,Pred.pi[,"lwr"],lty=2,col="blue", lwd=2)
lines(xval,Pred.pi[,"upr"],lty=2,col="blue", lwd=2)


# Slide 51:

janka <- read.table("janka.txt", header=TRUE, quote="\"")
names(janka) <- c("Density", "Hardness")

reg3 <- lm(Hardness ~ Density, data = janka)
plot(Hardness ~ Density, data = janka, pch = 20, las = 1)
abline(reg3)

#Slide 52:

par(mfrow=c(2,2))
plot(reg3, which=1:4)
par(mfrow=c(1,1))


#Slide 53:

plot(residuals(reg3) ~ Density, data = janka, 
       ylab="residuals")
abline(h = 0)


#Slide 54:

residualPlots(reg3)



#Slide 61:

janka$Log2Hard <- log2(janka$Hardness)

reg4 <- lm(Log2Hard ~ Density, data = janka)

plot(reg4, which=1)


# Slide 62:
reg5 <- lm(Log2Hard ~ Density + I(Density^2), data = janka)
plot(reg5,which=1)

#Slide 63:

summary(reg5)

#Slide 64:

par(mfrow=c(2,2))
plot(reg5, which=1:4)
par(mfrow=c(1,1))

# Slide 66:

xval <- seq(from = 25, to = 70, length.out = 500)
newData <- data.frame(Density = xval)
Pred.ci <- predict(reg5, newdata = newData,
interval = "confidence", level = .95)


plot(Hardness ~ Density, data = janka, pch = 20, las = 1)
lines(xval, 2^Pred.ci[, "fit"], lwd = 2)
lines(xval, 2^Pred.ci[, "lwr"], lty = 2, col = "red", lwd = 2)
lines(xval, 2^Pred.ci[, "upr"], lty = 2, col = "red", lwd = 2)
