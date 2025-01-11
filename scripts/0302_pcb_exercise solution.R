# read in data:

troutpcb<-read.table("Data/troutpcb.txt", header=TRUE, sep="\t")

#Data checkup
head(troutpcb)
str(troutpcb)
summary(troutpcb)
tail(troutpcb)

#plotting
plot(troutpcb$PCB~troutpcb$Age)

#linear regression model and add fit to the plot
fm <- lm(PCB~Age,data=troutpcb)
abline(fm)

#extraction of coefficients of lm
coef(fm)
summary(fm)

#model diagnostic and residual plotting
par(mfrow=c(2, 2))
plot(fm, which = 1:4)
#clear trumpet shape, log can be a solution

#logarithm of PCB concentration
troutpcb$log10PCB <- log10(troutpcb$PCB)
head(troutpcb$log10PCB)

#logPCB vs age
par(mfrow=c(1,1))
plot(troutpcb$log10PCB~troutpcb$Age)

#2nd linear regression model and fit to plot
fm2<-lm(log10PCB~Age,data=troutpcb)
fm2<-lm(log10PCB~Age+I(Age^2),data=troutpcb)
abline(fm2)

#2nd residual check
par(mfrow=c(2,2))
plot(fm2, which=1:4)
#the plots all look much better
#slight increase in variance heterogeneity
#observation 28 could be influential (0.4 cd)

#2nd extraction of coefficients
summary(fm2)

#nice table
tab <- cbind(coef(summary(fm2))[ ,1:2], 
             "Lower"=confint(fm2)[ ,1], 
             "Upper"=confint(fm2)[ ,2])
tab

#nice table with p-values
data.frame(round(tab, 2),
           "p-value"= format.pval(coef(summary(fm2))[ ,4], digits=3, eps= 1e-3))

#compute the coefficients
10^coef(fm2)

#compute the confidence intervals
10^confint(fm2)

#final table fm2
B<-coef(summary(fm2))
df<-data.frame(Estimate = round (10^B[ , 1],2),
               Lower = round (10^confint(fm2)[,1 ],2),
               Upper = round (10^confint(fm2)[,2 ],2),
               "p-value"=format.pval(B[ , 4], digits =3, eps=1e-3))
df

#plot in original scale of measurement
par(mfrow=c(1,1))
plot(troutpcb$PCB~troutpcb$Age, ylim=c(0,35),
     xlab="Age[years]", ylab="PCB concentration [ppm]",
     bty="n", las=1)

#confidence interval(?)-problem starts here
xval <- seq(1, 12, length=500)
pred <- predict(fm2,newdata=data.frame(Age=xval), 
                interval="confidence") 
lines(xval, 10^pred[, "fit"], lwd=2) 
lines(xval, 10^pred[, "lwr"], col="red", lwd=2, lty=2) 
lines(xval, 10^pred[, "upr"], col="red", lwd=2, lty=2)

#prediction intervals(?)
pred <- predict(fm2, newdata=data.frame(Age=xval), interval="prediction") 
lines(xval, 10^pred[, "lwr"], col="blue", lty=3, lwd=2) 
lines(xval, 10^pred[, "upr"], col="blue", lty=3, lwd=2)

#add legend
legend("topleft", legend=c("Fit", "95% Confidence interval", "95% Prediction interval"), 
       lwd=2, col=c("black", "red", "blue"), lty=1:3, bty="n")
