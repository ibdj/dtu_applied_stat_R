# exercise ### 
surgery <- read.delim("data/surgery.txt", sep = "")

head(surgery)
# Fit a logistic regression model using these predictors, interpret parameter estimates, and conduct inference about the effects

analysis<-glm(Y~D+T,family=binomial(link=logit),data=surgery)
analysis
summary(analysis)

# experienced a sore throat on waking up 
#Y (0=no, 1=yes), as a function of the 
#D= duration of the surgery in minutes; and the 
#T= type of device used to secure the airway (0 = laryngeal mask airway, 1= tracheal tube) ()

analysis2<-update(analysis,~.+T,data=surgery)
analysis2
summary(analysis2)

drop1(analysis2,test="Chisq")

plot(surgery)

plot(surgery$Y ~ surgery$D)

