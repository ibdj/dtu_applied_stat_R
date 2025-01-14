# One- and Two way Analysis of Variance
getwd()

setwd("<your favorite directory>")

#Slide 8:

lbw <- read.delim("data/lbw.txt")
by(lbw$bwt, lbw$race, summary)
## lbw$


# Slide 9: 

boxplot(bwt ~ race, data = lbw, xlab = 'Race', ylab = 'Birthweight',
las = 1, col = 2:4)

# Slide 10:

stripchart(bwt ~ race, data = lbw, vertical = TRUE, xlab = "Race",
ylab = "Birthweight", method = "jitter", las = 1, col = 2:4)

# Slide 16:

model1<-lm(bwt ~ race, data = lbw)
anova(model1)

# Slide 18:

summary(model1)

  #The first category is the reference; sorted alphabetically
  #p-values here are not really relevant

# slide 19:

confint(model1)

# slide 20



# Slide 22:
#variance homogeneity
boxplot(bwt ~ race, data =lbw, xlab='Race', ylab='Birth weight', las=1)

# Slide 23:

# install.packages("MESS")

library(MESS)
wallyplot(model1)



# Slide 24:

library(car)
leveneTest(model1)

# Slide 26:

qqwrap <- function(x, y, ...) {qqnorm(y,main="",...)
abline(a=0, b=1)}
wallyplot(model1, FUN=qqwrap)

# Slide 29:

pairwise.t.test(lbw$bwt, lbw$race, p.adj = "none")

# slide 30:

pairwise.t.test(lbw$bwt, lbw$race, p.adj = "bonferroni")

# Slide 34:

boxplot(bwt ~ race*smoke, data=lbw, xlab = 'Race and Smoke',
ylab = 'Birthweight', las = 1, col = 2:4)

# Slide 38:

interaction.plot(lbw$race, lbw$smoke, lbw$bwt, type=c("b"),
ylab = "Birth Weight", las=1, trace.label = "Smoke", xlab = "Race")

# Slide 39:

lbw$smoke <- as.factor(lbw$smoke)
model2 <- lm(bwt ~ race + smoke + race:smoke, data = lbw)
summary(model2)

# Slide 45:

anova(model2)

# Slide 47:

model3 <- lm(bwt ~ race + smoke, data = lbw)
anova(model3)

# Slide 48:

confint(model3)



lbw$smoke<-factor(smoke)

lbw$bwt<- as.numeric(lbw$bwt)

str(lbw)

summary(lbw)

by(lbw$bwt, lbw$race, summary)

par(mfrow=c(1,2)); par(mar= c(4,4,2,1.5))
boxplot(bwt ~ race, data =lbw, xlab='Race', ylab='Birth weight', las=1, col=2:4)

stripchart(bwt~race, data=lbw, vertical=TRUE, method="jitter", xlab="Race", 
           ylab="Birth weight",cex=1.2,pch=16, las=1, col=2:4)

lbw$race<-as.factor(lbw$race)

model1<-lm(bwt ~ race, data = lbw)

anova(model1)
drop1(model1, test="F") #alternative extraction of the F-test
summary(model1)

model1B<-lm(bwt ~ race - 1, data = lbw)

summary(model1B)
options(digits=4)
confint(model1, level=.95)
confint(model1B, level=.95)

par(mfrow=c(2,2))

plot(model1, which=1:4)

library(car)

leveneTest(model1)

pairwise.t.test(lbw$bwt,lbw$race, p.adj = "none")

pairwise.t.test(lbw$bwt,lbw$race, p.adj = "bonferroni")

#Two-way Analysis of Variance

lbw$smoke<-as.factor(lbw$smoke)

par(mfrow=c(1,2)); par(mar= c(4,4,2,1.5))
boxplot(bwt ~ smoke*race, data = lbw, xlab='Race and Smoke', ylab='Birth weight', las=1, col=2:4)
stripchart(bwt ~ smoke*race, data = lbw, vertical=TRUE, method="jitter", xlab='Race and Smoke', ylab='Birth weight', las=1, col=2:4)

interaction.plot(lbw$race,lbw$smoke,lbw$bwt, fun=mean, type=c("b"), ylab="Birth weight", las=1,lwd=2, trace.label="Smoke",xlab="Race")

#Model with interaction

model2<-lm(bwt ~ race + smoke + race:smoke, data = lbw)
anova(model2)
drop1(model2, test="F")
summary(model2)

#Same model different parametrization

#model2B<-lm(bwt ~ race + smoke + race:smoke -1, data = lbw)
#summary(model2B)
#anova(model2)

#Model without interaction

model3 <- lm(bwt ~ race + smoke, data = lbw)
anova(model3)
drop1(model3, test="F")
summary(model3)

model3B <- lm(bwt ~ race + smoke -1, data = lbw)
anova(model3B)
drop1(model3B, test="F")
summary(model3B)
confint(model3B)

#Residuals an fitted values the same for any paramatrization of the same model, e.g. model3 and model3B.
par(mfrow=c(1,2)); par(mar= c(4,4,2,1.5))
plot(model3)


plot(model3, which=1:4)

#End
