setwd("<your favorite directory>")

getwd()

# Slide 14:
CADdata <- read.csv("Data/cadmium.txt", sep="")
CADdata$group <- as.factor(CADdata$group)

plot(CADdata$age, CADdata$vitcap,
col = c("blue","red","green")[CADdata$group],
xlab = "Age", ylab = "Vital Capacity (L)",
las = 1, cex = 1.5, pch = c(16,16,16))
legend(55,5.8, c(">10 years","<10 years", "Not exposed"),
col = c("blue","red","green"),
pch = c(16,16,16), title = "Exposure")

# Slide 16:

par(mfrow = c(1,2), mgp = c(2,0.7,0), mar = c(3,3,1,1))
boxplot(vitcap ~ group, data = CADdata, ylab = 'Vital Capacity (L)',
las = 1, xaxt = "n", col = 5)
axis(1, at = c(1,2,3),
labels = c(">10 years","<10 years", "Not exposed"))
boxplot(age ~ group, data = CADdata ,ylab = 'Age',
las = 1, xaxt = "n", col = 5)
axis(1, at = c(1,2,3),
labels = c(">10 years", "<10 years", "Not exposed"))
par(mfrow = c(1,1))


#Slide 27:
replacing values!

CADdata$expo[CADdata$group==3] <- 1
CADdata$expo[CADdata$group==2] <- 2
CADdata$expo[CADdata$group==1] <- 3

CADdata$expo<-as.factor(CADdata$expo)

Model1<-lm(vitcap ~ age + expo, data = CADdata)

#Slide 28:
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(Model1, which = 1:4)
par(mfrow = c(1,1))

# Slide 29:

library(MESS)
wallyplot(Model1)

# Slide 30:

qqwrap <- function(x, y, ...) {qqnorm(y,main="",...); abline(a=0, b=1)}
wallyplot(Model1, FUN=qqwrap)

# Slide 31:

plot(CADdata$age, Model1$residuals, xlab = 'Age',
ylab = 'Residuals')

# Slide 32:

library(car)
residualPlots(Model1)

# Slide 35:

Model1 <- lm(vitcap ~ age + expo, data = CADdata)
summary(Model1)

# Slide 36:

confint(Model1)

tab <- cbind(coef(summary(Model1))[ , 1:2], "Lower" = confint(Model1)[ , 1],
"Upper" = confint(Model1)[ , 2])

data.frame(round(tab, 2),
"p-value" = format.pval(coef(summary(Model1))[ , 4], digits = 3, eps = 1e-3))


# Slide 38:

coef <- coef(Model1)

 
#Plot points in a single plot
par(mfrow=c(1,1))

plot(CADdata$age, CADdata$vitcap, col=c("green","red","blue")[CADdata$expo],
     xlab = "Age", 
     ylab = "Vital Capacity (L)",xlim=c(15,68), ylim=c(2.5,6), pch=c(16,16,16), main = "Exposure to cadmium")

legend(57.2,6.2, c("Not exposed","Low","High" ),
       col=c("green","red","blue"), pch = c(16,16,16) )

# plot line for first group, expo=1
lines(18:65, coef[1] + coef[2]*(18:65), col="green", type="l",lwd=2)
# plot line for first group, expo=2
lines(21:58, coef[1] + coef[3]+ coef[2]*(21:58), col="red", type="l", lwd=2)
# plot line for first group, expo=3
lines(39:65, coef[1] + coef[4] + coef[2]*(39:65), col="blue", type="l", lwd=2)



# Slide 40:

Model2 <- lm(vitcap ~ age + expo + age:expo, data = CADdata)
summary(Model2)

# Slide 41:

drop1(Model2, test = "F")

# Slide 42:

Model2B<-lm(vitcap ~ 0 + expo + age:expo, data = CADdata)


# Slide 43:

summary(Model2B)

# Slide 45:

#Plot the lines for the model with interaction -final.

#Copy coefficients into an auxiliary vector for easy reading. 
coef <- coef(Model2B)
coef
 
#Plot points in a single plot
par(mfrow=c(1,1))

plot(CADdata$age, CADdata$vitcap, col=c("green","red","blue")[CADdata$expo],
     xlab = "Age", 
     ylab = "Vital Capacity (L)",xlim=c(15,68), ylim=c(2.5,6), pch=c(16,16,16), main = "Exposure to cadmium")

legend(57.2,6.2, c("Not exposed","Low","High" ),
       col=c("green","red","blue"), pch = c(16,16,16) )


# plot line for first group, expo=1
lines(18:65, coef[1] + coef[4]*(18:65), col="green", type="l",lwd=2)
# plot line for first group, expo=2
lines(21:58, coef[2] + coef[5]*(21:58), col="red", type="l", lwd=2)
# plot line for first group, expo=3
lines(39:65, coef[3] + coef[6]*(39:65), col="blue", type="l", lwd=2)
