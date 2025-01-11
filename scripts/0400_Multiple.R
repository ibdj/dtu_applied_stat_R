setwd("C:/ANST/Undervisning/Kurser/phd kursus i basal statistik Jun2024/Regression/Multiple")

# slide 11:

oz <- read.delim("Data/ozone.data.txt")

library(car)
scatterplotMatrix(~ ozone + rad + temp + wind, 
diagonal=list(method="boxplot"), 
                  data = oz)

# Slide 17:

reg1 <- lm(ozone ~ rad + temp + wind, data = oz)

# Slide 18:
par(mfrow=c(2,2))
plot(reg1)
par(mfrow=c(1,1))

# Slide 20:

summary(reg1)

# NICER:

tab <- cbind(coef(summary(reg1))[ , 1:2], 
             "Lower" = confint(reg1)[ , 1],
             "Upper" = confint(reg1)[ , 2])
data.frame(round(tab, 2),
           "p-value" = format.pval(coef(summary(reg1))[ , 4], 
           digits = 3, eps = 1e-3))


summary(reg1)$sigma
summary(reg1)$sigma^2
# The value on the slides is based on the rounded sigma with two decimals


# Slide 26:

library(mgcv)
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1,1))
model <- gam(ozone ~ s(rad) + s(temp) + s(wind), data = oz)
plot(model) 
par(mfrow = c(1,1))


# Slide 33:

library(tree)
model<-tree(ozone~., data = oz)
plot(model)
text(model)


# Slide 38:

reg2 <- lm(ozone ~ rad + temp + wind + I(temp^2) + 
             I(wind^2) + temp:wind + rad:wind, data = oz)
summary(reg2)

# Slide 39:

par(mfrow=c(2,2))
plot(reg2)
par(mfrow=c(1,1))

# Slide 44:

reg3<-lm(log(ozone) ~ rad + temp + wind + I(temp^2) + 
                      I(wind^2) + I(rad^2) + rad:temp + rad:wind + 
                      temp:wind, data = oz)



# Slide 45:

par(mfrow=c(2,2))
plot(reg3, which=1:4)
par(mfrow=c(1,1))

# Slide 46:

reg3<-update(reg3,data=oz[-17,])
par(mfrow=c(2,2))
plot(reg3,which=1:4)
par(mfrow=c(1,1))

# Slide 47:
summary(reg3)


# Slide 53:

drop1(reg3, test = "F")

# Slide 54:

reg4 <- update(reg3, ~. -rad:wind)
drop1(reg4, test = "F")

# Slide 55:

reg4 <- update(reg4, ~. -rad:temp)
drop1(reg4, test = "F")


# Slide 56:

reg4 <- update(reg4, ~. -temp:wind)
drop1(reg4, test = "F")

# Slide 57:

reg4 <- update(reg4, ~. -I(rad^2))
drop1(reg4, test = "F")

# Slide 58:

reg4 <- update(reg4, ~. -I(temp^2))
drop1(reg4, test = "F")


# Slide 60:

summary(reg4)$coef



