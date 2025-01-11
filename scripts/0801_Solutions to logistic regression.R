
# Exercise slide 33: 

setwd("C:/<your data directory>")
crab.data<-read.table("datasets/crab.data.txt")

# 1):

plot(crab.data)

# 2)

analysis<-glm(y~width,family=binomial(link=logit),data=crab.data)

analysis2<-update(analysis,~.+ weight) 

drop1(analysis2,test="Chisq")

# both width and weight are insignificant.

analysis3<-update(analysis2,~.- weight) 
drop1(analysis3,test="Chisq")


analysis4<-update(analysis2,~.- width) 
drop1(analysis4,test="Chisq")


# however, if you remove one, the other will be significant.
# the two predictors are therefore to
# some extent exchangeable.

# Which one should you prefer?


drop1(analysis2,test="Chisq")

# the p-value of weight is much higher than the one for width. 
# following a principle of removing the 'most insignificant'
# covariate first, weight should be removed and width preferred.

Also:

AIC(analysis3)

AIC(analysis4)

# shows that analysis3 has the lowest Akaike Information, 
# so it should be preferred (ie. width should be preferred)

# the AIC can also be read off by typing

analysis3
analysis4


#3): 

analysis2<-update(analysis,~.+ as.factor(color)) 
drop1(analysis2,test="Chisq")
# p-value for color is 0.07>0.05 so insignificant,
# but close to significance

# which one stands out?

summary(analysis2)

# color level 5 has a much lower estimate and lower p-value than the 
#  other levels.


#4):


crab.data.2<-data.frame(crab.data,dark=1*(crab.data$color==5))

analysis2<-update(analysis,~.+dark,data=crab.data.2)
drop1(analysis2,test="Chisq")

# dark is significant, so the satellittes are not indifferent.

summary(analysis2)

# The estimate for dark is negative, so darker females have lesser chance 
# of a satellite. Thus, the satellite males prefer light-skinned females.


#Exercises slide 41:

# Exercise 1:


surgery<-read.table("datasets/surgery.txt",header=T)

head(surgery)

analysis.surgery<-glm(Y~D+T,family=binomial(link=logit),data=surgery)
drop1(analysis.surgery,test="Chisq")
summary(analysis.surgery)


# Adding squared terms:
analysis.surgery<-update(analysis.surgery,~.+I(D^2))
drop1(analysis.surgery,test="Chisq")

# Not significant
analysis.surgery<-update(analysis.surgery,~.-I(D^2))
drop1(analysis.surgery,test="Chisq")

analysis.surgery<-update(analysis.surgery,~.-T)
drop1(analysis.surgery,test="Chisq")

summary(analysis.surgery)

# The coefficient to D is positive.
# Thus, the surgery time significantly increases
# the probability of experiencing a sore throat.

# Odds ratio for duration:

exp(0.07038)

1.072916

# odds of a sore throat increases by 7.3% per minute.


#Exercise 2:

library(MASS); data(menarche)

head(menarche)

analysis.menarche<-glm(cbind(Menarche, Total-Menarche)~Age,
                       family=binomial(link=logit),
                       weights=Total,data=menarche)


drop1(analysis.menarche,test="Chisq")


summary(analysis.menarche)

# Age increases the frequency of menarche.
# Odds ratio:

exp(1.724127)

# 5.607623

# odds of Menarche increases by a factor 5.6 per year.

predict.menarche<-predict(analysis.menarche,se.fit = T)
predict.menarche2<-data.frame(fit=predict.menarche$fit,
                              lower=predict.menarche$fit-1.96*predict.menarche$se.fit,
                              upper=predict.menarche$fit+1.96*predict.menarche$se.fit)

plot(menarche$Age,invlogit(predict.menarche2$fit),
     type="l",lwd=2,las=1,xlab='Age',ylab='menarche freq.') 
lines(menarche$Age,menarche$Menarche/menarche$Total,type="p",pch=16, cex=1.5)

# The CIs are so small that it does not add anything useful to the figure.
# Thus these commands are not executed, but you could try them out:

#lines(menarche$Age,invlogit(predict.menarche2$upper),type="l",col="red",lty=2)
#lines(menarche$Age,invlogit(predict.menarche2$lower),type="l",col="red",lty=2)
