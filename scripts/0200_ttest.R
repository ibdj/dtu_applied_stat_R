setwd("C:/Users/ANST/Undervisning/Kurser/phd kursus i basal statistik Jun2024/Inferens og t-tests/T-test")

# Slide 4:
Mobile.phone <- read.delim("Data/Mobiltelefon.txt")


# Slide 5:
Mobile<-data.frame(Time=c(Mobile.phone$Cell.phone,Mobile.phone$Control),
                 Group=c(rep("Mobile",32),rep("Control",32)))

boxplot(Time~Group,data=Mobile,col=2:3,xlab='',ylab='reaction time (msec)')

#Slide 7:
par(mfrow=c(2,1))
hist(Mobile$Time[Mobile$Group=="Mobile"],xlab='Mobile',
     ylab='reaction time (msec)',
     probability=T,main='',xlim=c(0,1000),ylim=c(0,0.006),col="green")
hist(Mobile$Time[Mobile$Group=="Control"],xlab='Control',
     ylab='reaction time (msec)',
     probability=T,main='',xlim=c(0,1000),ylim=c(0,0.006),col="green")
par(mfrow=c(1,1))

# Slide 8:

par(mfrow=c(2,1))
plot(Mobile$Time[Mobile$Group=="Mobile"],xlab='Mobile',ylab='reaction time (msec)',
     main='',ylim=c(0,1000),col="red")
plot(Mobile$Time[Mobile$Group=="Control"],xlab='Control',ylab='reaction time (msec)',
     main='',ylim=c(0,1000),col="red")
par(mfrow=c(1,1))

# Slide 9:

by(Mobile$Time,Mobile$Group,summary)

by(Mobile$Time,Mobile$Group,sd)


# Slide 11:

par(mfrow=c(1,2))
plotdata<-Mobile$Time[Mobile$Group=="Mobile"]
qqnorm(plotdata,main="Mobile Phone")
lines((-3):3,mean(plotdata)+sd(plotdata)*(-3:3),type="l",col="red",lwd=3)
plotdata<-Mobile$Time[Mobile$Group=="Control"]
qqnorm(plotdata,main="Control")
lines((-3):3,mean(plotdata)+sd(plotdata)*(-3:3),type="l",col="red",lwd=3)


# slide 14:

Y<-Mobile$Time[Mobile$Group=="Mobile"]
max(Y)
2*(1-pnorm(max(Y)-mean(Y),sd=sd(Y))^length(Y))

Y<-Y[which(Y<900)]
2*(1-pnorm(960-mean(Y),sd=sd(Y))^length(Y))




# Slide 15:

par(mfrow=c(1,2))
plotdata<-Mobile$Time[Mobile$Group=="Mobile" & Mobile$Time<900]
qqnorm(plotdata,main="Mobile Phone")
lines((-3):3,mean(plotdata)+sd(plotdata)*(-3:3),type="l",col="red",lwd=3)
plotdata<-Mobile$Time[Mobile$Group=="Control"]
qqnorm(plotdata,main="Control")
lines((-3):3,mean(plotdata)+sd(plotdata)*(-3:3),type="l",col="red",lwd=3)



# slide 16:

Y1<-Mobile$Time[Mobile$Group=="Mobile" & Mobile$Time<900]
Y2<-Mobile$Time[Mobile$Group=="Control"]

m1<-mean(Y1)
m2<-mean(Y2)
n1<-length(Y1)
n2<-length(Y2)

s12<-var(Y1)
s22<-var(Y2)
nu<-(s12/n1+s22/n2)^2/((s12/n1)^2/(n1-1)+(s22/n2)^2/(n2-1))


# Slide 17:

nu
# 60.69052


# t-test statistic:
T<-(m1-m2)/sqrt(s12/n1+s22/n2)

# Welch t-test:
2*(1-pt(T,df=nu))
# p=0.014

# Slide 18:

# difference between groups:

m1-m2
#39.50302

# variation:

sqrt(s12/n1+s22/n2)
# 15.66674

# Slide 19:

# ci lower:

m1-m2-qt(0.975,df=nu)*sqrt(s12/n1+s22/n2)

# ci upper:

m1-m2+qt(0.975,df=nu)*sqrt(s12/n1+s22/n2)

# with normal noise (1.96) lower:

m1-m2-1.96*sqrt(s12/n1+s22/n2)

# with normal noise (1.96) upper:

m1-m2+1.96*sqrt(s12/n1+s22/n2)

# Slide 20:

t.test(Y1,Y2)


# Slide 22:


s2<-((n1-1)*s12+(n2-1)*s22)/(n1+n2-2)
s2

T<-(m1-m2)/sqrt(s2/n1+s2/n2)
T

# slide 24:
t.test(Y1,Y2,var.equal=TRUE)


# Slide 27:

Y1<-Mobile$Time[Mobile$Group=="Mobile"]
Y2<-Mobile$Time[Mobile$Group=="Control"]

m1<-mean(Y1)
m2<-mean(Y2)
n1<-length(Y1)
n2<-length(Y2)

s12<-var(Y1)
s22<-var(Y2)

T<-(m1-m2)/sqrt(s12/n1+s22/n2)

# Test statistic
T

# Estimated group difference
m1-m2

# Slide 30:

set.seed(9825)
my.reaction.times<-Mobile$Time
my.t.statistics<-numeric(50000)
for(i in 1:50000){
  index<-sample(1:64,32)
  Y1.temp<-my.reaction.times[index]
  Y2.temp<-my.reaction.times[-index]
  my.t.statistics[i]<-t.test(Y1.temp,Y2.temp)$statistic 
  }
my.p.value<-length(my.t.statistics[abs(my.t.statistics)>T])/50000

my.p.value


# Slide 31:

plot(cumsum(abs(my.t.statistics)>T)/(1:50000),type="l",
     ylab="Permutation test p-value")
lines(c(-10000,52000),rep(my.p.value,2),type="l",lty=2,lwd=3,col="blue")

# slide 32 exersice
# Access the builtin data set mtcars with the command data(mtcars)
    data(mtcars)
    str(mtcars)
# Plot the Miles per Gallon for the two groups (am=0 or 1).
    mtcars$am <- factor(mtcars$am)
    boxplot(mpg ~ am, data = mtcars, las = 1,
            ylab = "Mile pr gallon", col = 2:3)
# Formulate the relevant hypothesis to test, and the alternative.
    # H1 cars with automatic transmission runs more miles pr gallon than cars with manual transmission
    # H0 alternative: that there is no difference between at and mt with regards to mpg

# Are the underlying assumptions for the t-test fullled?
    hist(mtcars$mpg)
    qqnorm(mtcars$mpg)
    lines((-3):3,((-3):3)*sd(mtcars$mpg)+mean(mtcars$mpg),
          type="l",col="red",lwd=2)
    
    mtcars_am<-mtcars$mpg[mtcars$am=="1"]
    mtcars_mt<-mtcars$mpg[mtcars$am=="0"]
    
    hist(mtcars_am)
    # Normality as described - how could this be violated?
    # Independence: all observations are independent - how could this be violated?
    # Representativity: students represent a random sample - how could this be violated?

# What is the estimated difference in mpg, and the corresponding 95% condence interval?
# What can we conclude about H0?


# Slide 33:
glucose12<-read.table("Data/Glucose12.txt",header=T,sep="\t")
head(glucose12)



# Slide 35:

glucose<-data.frame(subject=rep(glucose12$subject,2),
                    glucose=c(glucose12$Glucose1,glucose12$Glucose2),
                    method=rep(1:2,each=dim(glucose12)[1]))

par(mfrow=c(1,2))
boxplot(glucose~method,data=glucose,col=2:3,xlab="Method", 
        main="Sub-optimal Plot")
plot(glucose$method,glucose$glucose,pch='',xlab="Method",
     ylab='Glucose level',main="Better plot",axes=F)
axis(1,at=1:2,labels=1:2)
axis(2)
box()
for(i in 1:dim(glucose)[1]){
  lines(1:2,c(glucose12$Glucose1[i],glucose12$Glucose2[i]),col=i,lty=i,lwd=2)
  }
par(mfrow=c(1,1))





# Slide 38:

plot(glucose12$Glucose1-glucose12$Glucose2,
     ylab='Difference in Glucose level')

# Slide 39:

qqnorm(glucose12$Glucose1-glucose12$Glucose2,main="")
lines((-3):3,mean(glucose12$Glucose1)-mean(glucose12$Glucose2)+
+ sd(glucose12$Glucose1-glucose12$Glucose2)*((-3):3),
      type="l",lwd=3,col="red")


# Slide 40:
# continuing without the outlier:
glucose12.new<-glucose12[glucose12$Glucose1-glucose12$Glucose2> -1,]
glucose12.new$D<-glucose12.new$Glucose1-glucose12.new$Glucose2

par(mfrow=c(1,2))
plot(glucose12.new$D,
     ylab='Difference in Glucose level')
qqnorm(glucose12.new$D,main="")
lines((-3):3,mean(glucose12.new$D)+
      sd(glucose12.new$D)*((-3):3),
      type="l",lwd=3,col="red")
par(mfrow=c(1,1))

# Slide 41:

#mean:
muhat<-mean(glucose12.new$D); muhat

# sd:
sdhat<-sd(glucose12.new$D); sdhat

#SEM:
SEM<-sdhat/sqrt(72); SEM

# Slide 42:

# lower:
muhat-qt(0.975,df=72)*SEM

# upper 
muhat+qt(0.975,df=72)*SEM


# normal errors:

muhat-1.96*SEM; muhat+1.96*SEM


# Slide 44:

# test statistic:
T<-(muhat-0)/SEM; T

# p-value:

2*(1-pt(T,df=71))

# slide 45:

t.test(glucose12.new$D)

# slide 46:

t.test(glucose12.new$Glucose1,glucose12.new$Glucose2,paired=TRUE)

# slide 48 exersice mobile phone 2

# Load the data Mobile_Matched.txt.
    mobilematched<-read.delim("Data/Mobile_Matched.txt",header=T)
# Make relevants plots of the data, and 
    par(mfrow=c(1,2))
    hist(mobilematched$Control)
    hist(mobilematched$Mobile)
    
    boxplot(mobilematched$Control)
    boxplot(mobilematched$Mobile)

    plot(mobilematched$Control)
    plot(mobilematched$Mobile)
# formulate the hypotheses to test the method difference.
    # there is a difference between having using your mobile or not
# Evaluate the model control, and perform the test.
    
    t.test(mobilematched$Control,mobilematched$Mobile,paired=TRUE)



