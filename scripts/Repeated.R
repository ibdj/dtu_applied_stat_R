#setwd("C:/Users/ANST/Undervisning/phd kursus i basal statistik Jan2023/Repeated")

# slide 16
rats <- read.csv("data/rats.txt")

# make treatment and cage factors 
rats$treatm <- factor(rats$treatm)
rats$cage <- factor(rats$cage)

# make two versions of the time variable 
# - one quantitative and one factor
rats$monthQ <- rats$month
rats$month <- factor(rats$month)

summary(rats)
str(rats)

# slide 17:

head(rats,n=10)

# slide 18:

rats2<-as.data.frame(matrix(ncol=12,nrow=30))
names(rats2)<-c("treatm","cage",paste("lnc.",1:10,sep=""))
rats2$treatm<-tapply(rats$treatm,rats$cage,function(x){x[1]})
rats2$cage<-levels(rats$cage)
for(i in 1:length(levels(rats$cage))){
  rats2[i,-(1:2)]<-rats$lnc[rats$cage==levels(rats$cage)[i]]
  }

head(rats2)


# slide 19:
#Always start by plotting the data


plot(rats$monthQ,rats$lnc, xlab="Month", 
                 ylab="log(count)",type="n")
for(i in 1:dim(rats2)[1]){
  lines(1:10,rats2[i,-(1:2)],type="b",col=rats2$treatm[i]+1,
  pch=16,lwd=2,cex=2)
  }

legend("topright",legend=1:3,col=2:4,pch=16,lty=1,title="treatm")

# Slide 20:
#Generate the mean values of lnc by month and treat
Mean_data <- aggregate(rats$lnc, 
                       by=list(rats$month, rats$treatm), 
                       mean)
#Have a look
Mean_data

#assign names
names(Mean_data) <- c("month","treatm","Meanlnc")
#Have a look
Mean_data

#Plot the means

Grp1<-subset(Mean_data,treatm==1)
Grp2<-subset(Mean_data,treatm==2)
Grp3<-subset(Mean_data,treatm==3)

plot(as.numeric(Grp1$month), Grp1$Meanlnc, type = "b", pch=16,
     xlab = "Month",
     ylab = "Mean lnc", las = 1, col= 2)

#plot(rep(as.numeric(Grp1$month),3), Mean_data[,3],  pch=16,
#     xlab = "Month",
#     ylab = "Mean lnc", las = 1, col= 2,type="n")

#lines(as.numeric(Grp1$month), Grp1$Meanlnc, type = "b", col = 2, pch=16)
lines(as.numeric(Grp2$month), Grp2$Meanlnc, type = "b", col = 3, pch=16)
lines(as.numeric(Grp3$month), Grp3$Meanlnc, type = "b", col = 4, pch=16)

legend(locator(1),                   # we will place it with a mouse click
       legend = c("1","2","3"), 
       title = "Treat",
       lty = c(1,1,1), 
       col= 2:4)  



# Slide 24:

#use the by function to make 10 tests
byMonth <- by(rats, rats$monthQ, 
              function(x) anova(lm(lnc ~ treatm, data=x)))
#have a look
byMonth

#The largest effect at month 8
byMonth[[8]]

#Standard critical value alpha=0.05 from F distribution F(2,27)
qf(0.95, 2, 27)
#3.354131
#We observed 7.288 so significant

#If we want to correct with Bonferroni 
# critical value alpha=0.05/10 =0.005
qf(0.995, 2, 27)
#6.488511
#The observed 7.288 still significant


# Slide 28
###############################
# Analysis using summary measures
####################################

#First analysis
#log(Total count) over all 10 months

rats$count <- exp(rats$lnc)

DataTot <- aggregate(rats$count, 
                     by=list(rats$cage, rats$treatm), sum)
#have a look
head(DataTot)

names(DataTot) <- c("cage","treatm", "Tot")

DataTot$logTot <- log(DataTot$Tot)

head(DataTot)
# Slide 29:

reg1 <- lm(logTot ~ treatm, data = DataTot)
summary(reg1)
anova(reg1)
#The effect of treatment is bordeline significant

#Second analysis
#Mean lnc over all 10 months
DataMean <- aggregate(rats$lnc, 
                     by=list(rats$cage, rats$treatm), 
                     mean)
#have a look
DataMean

names(DataMean) <- c("cage","treatm", "Meanlnc")

reg2 <- lm(Meanlnc ~ treatm, data = DataMean)
summary(reg2)
anova(reg2)
#The effect of treatment is bordeline significant

# Slide 30:
#Third analysis
# Summary measure slopes for each cage
# Do the rats have the same rate of change for 
# each treatment 

# byCage is a regression with different slope and intercept
#for each cage and saves the coefficients
byCage <- coef(lm(lnc ~ -1 + cage + monthQ:cage, data = rats))
#have a look
byCage

slope1 <- data.frame(matrix(byCage, nrow=30, byrow=F))

names(slope1) <- c("Intercept", "Slope")
head(slope1)
#We also need info about cage and treat
SlopeData <- cbind(DataMean[ , 1:2], slope1) 
head(SlopeData)
names(SlopeData)[1:2] <- c("Cage", "treatm")
head(SlopeData)
# Slide 31:
######################################
#PLOTTING THE DIFFERENT SLOPES

#fitting the linear model by cage

fit <- unlist(by(rats, rats$cage,
          function(x) fitted.values(lm(lnc ~ monthQ, data=x))))
names(fit) <- NULL

#plotting the linear fit by cage
interaction.plot(rats$monthQ, rats$cage, fit,xlab="Month", ylab="lnc", 
  legend=F, col=as.numeric(SlopeData$treatm)+1,lty=1,lwd=2)

lines(rats$monthQ,rats$lnc, type="p",pch=16,cex=2)
legend(locator(1),                   # we will place it with a mouse click
       legend = c("1","2","3"), title = "Treat",
       lty = c(1,1,1),lwd=2,col= 2:4)  
#we assume a linear regression, but that might be so
#Did we make an impresise desribtion when using the linear regression
# secund order taylor approzimation??
# log transformation is for when variation changes; variance stability problem




# Slide 33:
reg3 <- lm(Slope ~ treatm, data = SlopeData)

# Slide 34:
anova(reg3)
#No difference in slopes for the three treatment groups


# Slide 35:
#fitting the linear model by treat
fit2 <- unlist(by(rats, rats$treatm,
          function(x) fitted.values(lm(lnc ~ monthQ, data=x))))

names(fit2) <- NULL
#plotting the linear fit by treatment
interaction.plot(rats$monthQ, rats$treatm, fit2,
                 xlab="Month", ylab="lnc", legend=F, col=2:4,lty=1,lwd=2,
                 ylim=c(8.5,10.5))
lines(rats$monthQ,rats$lnc, type="p",pch=16,cex=2)
legend(locator(1),                   # we will place it with a mouse click
       legend = c("1","2","3"), title = "Treat",
       lty = c(1,1,1),lwd=2,col= 2:4)  


# Slide 37:
#Perhaps a curve rather than a straight line

rats$monthQ2 <- rats$monthQ^2
fit3 <- unlist(by(rats, rats$cage,
          function(x) fitted.values(lm(lnc ~ monthQ+monthQ2, data=x))))
names(fit3) <- NULL

#plotting the squared fit by cage
interaction.plot(rats$monthQ, rats$cage, fit3,
                 xlab="Month", ylab="lnc", legend=F, 
                 col=as.numeric(SlopeData$treatm)+1,lty=1,lwd=2)
lines(rats$monthQ,rats$lnc, type="p",pch=16,cex=1.5)
legend(locator(1),                   # we will place it with a mouse click
       legend = c("1","2","3"), title = "Treat",
       lty = c(1,1,1),lwd=2,col= 2:4)  




# Group 3 looks a bit different and one cage is very different

# Slide 39:
#fitting the squared model by treat
fit4 <- unlist(by(rats, rats$treatm,
           function(x) fitted.values(lm(lnc ~ monthQ + monthQ2, data=x))))
names(fit4) <- NULL

interaction.plot(rats$monthQ, rats$cage, fit4,
                 xlab="Month", ylab="lnc", legend=F, col=2:4,lty=1,lwd=2,
                 ylim=c(8.5,10.5))
lines(rats$monthQ,rats$lnc, type="p",pch=16,cex=2)
legend(locator(1),                   # we will place it with a mouse click
       legend = c("1","2","3"), title = "Treat",
       lty = c(1,1,1),lwd=2,col= 2:4)  


# Slide 41:
###############################
# Analyse using summary measures
####################################

#Fourth analysis
# Summary measure curvature for each cage

byCage2 <- coef(lm(lnc ~ -1 + cage + monthQ:cage + 
                     monthQ2:cage, data = rats))
#have a look
byCage2

#I want a data frame, one row for each cage
curve <- data.frame(matrix(byCage2, nrow=30, byrow=F))

names(curve) <- c("Intercept", "monthQ", "monthQ2")
head(curve)

#We also need info about cage and treat
CurveData <- cbind(DataMean[ , 1:2], curve) 
head(CurveData)

# Slide 42:
reg4 <- lm(monthQ2 ~ treatm, data = CurveData)
summary(reg4)

# Slide 43:
anova(reg4)

#Significant difference in curve for the three treatment groups


# Slide 51:
##########################################
# Using a random effects model

library(nlme)
model1 <- lme(lnc ~ month + treatm + month:treatm,
              random = ~1 | cage, data = rats)
anova(model1)

# Slide 52:
#This time summary gives a lot of output!
#summary(model1)

#A table of fixed effects estimates
summary(model1)$tTable

# Slide 53:
#The estimates random effects
VarCorr(model1)

#intra class correlation
0.027478/(0.027478+0.037899)

#Model check
# We still have assumptions

#residual plot
plot(model1)

#qqplot for residuals
qqnorm(residuals(model1))
qqline(residuals(model1))


#qqplot for random effect
qqnorm(ranef(model1)$"(Intercept)")
qqline(ranef(model1)$"(Intercept)")




# Slide 55:

analysis<-lme(lnc~month+treatm+month:treatm,
    random=~1|cage,
    correlation=corGaus(form=~as.numeric(month)|cage,nugget=T),
    data=rats)


# Slide 57:
summary(analysis)

# Slide 62:

plot(Variogram(analysis,form=~monthQ|cage,data=rats))

# Slide 63:

analysis2<-lme(lnc~month+treatm+month:treatm, random=~1|cage, 
               correlation=corExp(form=~monthQ|cage,nugget=T),data=rats)
plot(Variogram(analysis2,form=~monthQ|cage,data=rats))

# Slide 64:

analysis3<-lme(lnc~month+treatm+month:treatm, random=~1|cage, 
               correlation=corLin(form=~monthQ|cage),data=rats)
plot(Variogram(analysis3,form=~monthQ|cage,data=rats))

# Slide 66:

summary(analysis2)


# Slide 72: 

histamin <- read.table("Data/histamin.txt", header=T, sep=",", dec=".")
histamin$dog <- factor(histamin$dog)
histamin$minQ <- histamin$min
histamin$min <- factor(histamin$min)
histamin$TRT <- factor(paste(histamin$treatm,":",histamin$level,sep=""))


# Slide 73:

plot(histamin$minQ,histamin$hist,xlab="minQ",ylab="hist",pch="")
for(i in 1:64){
  temp<-histamin[histamin$dog==i,]
  lines(temp$minQ,temp$hist,col=(1:4)[temp$TRT[1]],lwd=2,type="b",pch=16,cex=2)
  }
legend(locator(1),                   # we will place it with a mouse click
       legend = levels(histamin$TRT), title = "TRT",
       lty = c(1,1,1,1),lwd=2,col= 2:5,cex=2)  


