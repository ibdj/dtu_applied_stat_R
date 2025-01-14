
crab <- read.delim("data/crap.data.txt", head = TRUE)
crab <- read.csv("data/crab.data.txt", sep="")

#1)   Plot the crab data again: plot(crab.data)

plot(crab)

#2) Deduce from the graph that another possible predictor for a satellite is the crabweight.  


#2.1. Use the update() function to add weight to the model as on slide 24. 

drop1(analysis,test="Chisq")

#2.2.How does that alter the model? If you should choose between width and weight, which onewould you choose?

analysis <- glm(y~width,family=binomial(link=logit),data=crab)
analysis2 <- update(analysis,~.+I(width^2)+weight)

drop1(analysis2,test="Chisq")


#Plot the crab data again: plot(crab.data)2) Deduce from the graph that another possible predictor for a satellite is the crabweight.  Use the update() function to    add weight to the model as on slide 24. How does that alter the model? If you should choose between width and weight,  which onewould you choose?3) A third possible predictor for satellites is the  color of the female. The color is a nominal covariate where higher value indicates darker skin, so it is added to the model as a factor:analysis2<-update(analysis,~.+ as.factor(color)) Check that color does not add significantly to the model. Which color label stands out the most?4) Create a new dataset that included an indicator for darkskinned females: crab.data.2<-data.frame(crab.data,dark=1*(crab.data$color==5))Add ‘dark’ to the model with the commandanalysis2<-update(analysis,~.+dark,data=crab.data.2)Do satellite males prefer light-skinned or dark-skinned females, or are theyindifferent?
  