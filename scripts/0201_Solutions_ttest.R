setwd("C:/Users/ANST/Undervisning/phd kursus i basal statistik Jan2025/Inferens og t-tests/T-test")

# MILES PER GALLON
#am=0 automatic
#am=1 manual

data(mtcars)
# PLOT
boxplot(mpg ~ am, data=mtcars, las = 1, ylab = "Miles per gallon",
        xaxt="n", col=4:5)
axis(1, at=c(1,2), label=c("Automatic","Manual"))
# IT LOOKS LIKE THE AUTOMATIC CARS RUN LESS MILES PER GALLON

# H0: MPG ARE THE SAME IN THE TWO GROUPS
# H1: MPG ARE DIFFERENT IN THE TWO GROUPS

# PLOT FOR NORMALITY
par(mfrow=c(1,2))

#AUTOMATIC AND MANUAL
plotdata<-mtcars$mpg[which(mtcars$am==0)]
qqnorm(plotdata, main = "Automatic")
lines((-3):3,mean(plotdata)+sd(plotdata)*((-3):3),type="l",col="red",lwd=3)

# note: qqline is a fast option that draws a line through 
# the 1st and 3rd quartile. It is NOT the optimal line, but a fast 
# approximative alternative. Try to overlay the qqline on the qqplot 
# with the following command:
# qqline(mtcars$mpg[which(mtcars$am==0)],col="green",lwd=3)

plotdata<-mtcars$mpg[which(mtcars$am==1)]
qqnorm(plotdata, main = "Manual")
lines((-3):3,mean(plotdata)+sd(plotdata)*((-3):3),type="l",col="red",lwd=3)
par(mfrow=c(1,1))

#THE TWO QQ PLOTS APPEAR OK

t.test(mpg ~ am, data=mtcars)

# WE GET THE ESTIMATED DIFFERENCE -7.24 WITH 95% CI -11.28 to -3.21
# SO ON AVERAGE AUTOMATIC CARS RUN 7 mpg LESS THAN MANUAL PER GALLON
# THE DIFFERENCE IS SIGNIFICANTLY DIFFERENT FROM 0, AS 0 IS NOT IN 
# THE CI AND WE GET p=0.001 <0.05.

#################################
# PAIRED STUDY OF MOBILE PHONE USE

MM <- read.delim("Data/Mobile_Matched.txt")

MM$dif <- MM$Mobile - MM$Control
MM$av <- (MM$Mobile + MM$Control)/2

# Bland-Altman plot:

plot(MM$av,MM$dif,xlab='Average',ylab='Difference')


# BOX PLOT
boxplot(MM$dif, horizontal=TRUE, col=5)
rug(MM$dif)

par(mfrow=c(1,2))
qqnorm((MM$Mobile-mean(MM$Mobile))/sd(MM$Mobile))
lines((-3):3,(-3):3,type="l",col="red", lwd=3)
qqnorm((MM$Control-mean(MM$Control))/sd(MM$Control))
lines((-3):3,(-3):3,type="l",col="red", lwd=3)
par(mfrow=c(1,1))

# ONE PERSON IS VERY SLOW. BUT FROM THE DIFFERENCE PLOT IT IS 
# NOT AN OUTLIER

# scatter plot of differences

plot(MM$dif, xlab = "Average", ylab = "Difference", las=1)
# NO APPARENT PATTERNS IN THE VARIATION


#NORMAL DISTRIBUTION FOR DIFFERENCES
qqnorm((MM$dif-mean(MM$dif))/sd(MM$dif))
lines((-3):3,(-3):3,type="l",col="red", lwd=3)

#THE NORMAL DISTRIBUTION IS NOT PERFECT BUT OK
# MAINLY A PROBLEM IF WE WANT TO CALCULATE NORMAL RANGE

# H0: THE REACTION TIMES ARE THE SAME
# HA: THE REACTION TIMES DIFFER

t.test(MM$Mobile, MM$Control, paired=TRUE)
#WE GET A SIGNIFICANT DIFFERENCE 
# ESTIMATED DIFF 50.3 WITH 95% CI (31.3 to 69.3)

# IF WE DO NOT THINK THE NORMAL DISTRIBUTION IS OK
# WE COULD TRY LOG-TRANSFORMATION

MM$logM <- log2(MM$Mobile)
MM$logC <- log2(MM$Control)
MM$Dlog <- MM$logM - MM$logC
MM$Avlog  <- (MM$logM + MM$logC)/2

# Bland-Altman plot:

plot(MM$Avlog,MM$Dlog,xlab='Average',ylab='Difference')



# SCATTERPLOT PLOT FOR LOG DATA
plot(MM$Dlog, xlab = "Average (log)", ylab = "Difference (log)", las=1)

#NORMAL DISTRIBUTION
qqnorm(MM$Dlog)
lines((-3):3,mean(MM$Dlog)+sd(MM$Dlog)*((-3):3),col="red",lwd=3,type="l")

# ONLY SLIGHTLY BETTER - BUT BETTER

# H0: THE REACTION TIMES ARE THE SAME
# HA: THE REACTION TIMES DIFFER

t.test(MM$logM, MM$logC, paired=TRUE)
#WE GET A SIGNIFICANT DIFFERENCE ON THE LOG SCALE
# ESTIMATED DIFF 0.125 WITH 95% CI 0.079 to 0.171

# WE WANT TO GET BACK TO ORIGINAL SCALE - TAKE ANTILOG
# 2^(log2M-log2C)=2^log2(M/C)=M/C
# DIFFERENCES ON LOG SCALE BECOME RATIOS ON ORIGINAL SCALE

# WE ESTIMATE THAT MOBILE HAS 2^0.125=1.09 times LONGER REACTION
# COMPARED TO CONTROL - 9% more.
# THE CONFIDENCE INTERVAL IS (2^0.079; 2^0.171)=(1.06; 1.13)
# compare with 50.3/mean(MM$Control)=0.09 
# veeery close to the same level
