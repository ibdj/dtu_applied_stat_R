#CHANGE WORKING DIRECTORY TO WHERE YOU HAVE YOUR DATA
setwd("Your favorite directory>")
#CHECK HOW IT WENT
getwd()

# slide 7:
#GET THE DATA
colour_dat <- read.table("data/Colour.txt",header=TRUE, dec=".")

# display them

# slide 8:

colourTab <- xtabs(Count ~ Boy + Colour_blind, data = colour_dat)
ftable(colourTab) # print table


#ROW TOTALS
margin.table(colourTab, 1)

#Row percentages
prop.table(colourTab,1)

#COLUMN TOTALS
margin.table(colourTab, 2)

# Slide 12:

library(epitools)

#EXACT CONFIDENCE INTERVALS
#colourTab[,2] ARE THE NUMBER OF COLOUR BLIND
#margin.table(colourTab,1) THE ROW SUMS

binom.approx(colourTab[,2], margin.table(colourTab,1))

# x n.Boy n.Freq proportion.Boy proportion.Freq lower.Boy   lower.Freq upper.Boy upper.Freq conf.level
# 0 1     0    120              0     0.008333333         0 -0.007931503         0 0.02459817       0.95
# 1 6     1    150              1     0.040000000         1  0.008640576         1 0.07135942       0.95

#negative freq does noget make any sense. 

# Slide 13:

#use this fore rare events
binom.exact(colourTab[,2], margin.table(colourTab,1))

# > binom.exact(colourTab[,2], margin.table(colourTab,1))
# x n.Boy n.Freq proportion.Boy proportion.Freq        lower      upper conf.level
# 0 1     0    120              0     0.008333333 0.0002109595 0.04555551       0.95
# 1 6     1    150              1     0.040000000 0.0148185211 0.08502781       0.95


# Slide 19:
library(epitools)
epitools::oddsratio(colourTab, method="wald")

# Slide 20:

epitools::riskratio(colourTab)

# Slide 23:

# CHI 2 TEST AND EXPECTED VALUES
chisq.test(colourTab, correct=FALSE) 

# Slide 24:

expected(colourTab)

# Slide 25:

epitools::oddsratio(colourTab,method="wald")

#############################################
# KIDNEY STONE EXAMPLE
#############################################

# Slide 27:
kidney <- read.table("data/kidney.txt",header=TRUE, dec=".")
kidney

# slide 28:
mytable <- xtabs(Count ~ Treatment + Success1 + Stone, data=kidney)

# print table
ftable(mytable) 

# Slide 29:
library(epitools)
#TABLE AGGREGATING OVER STONE
Treat_Succ<-margin.table(mytable,1:2)


#WHAT ARE THE ODDS OF SUCCESS?
oddsratio(Treat_Succ, method="wald")

#THE ODDS OF SUCCESS FOR TREATMENT B ARE 1.34 TIMES THE ODDS FOR A

# Slide 30:
#TABLE OF SMALL STONES
Small<-mytable[,,2]

# a 2 in the end for regular ordering; only the samll stones

oddsratio(Small, method="wald")

# Slide 31:
#TABLE OF LARGE STONES
Large<-mytable[,,1]
oddsratio(Large, method="wald")


# Slide 33:

#TABLE AGGREGATING OVER TREATMENT
Stone_Succ<-margin.table(mytable,2:3)
oddsratio(Stone_Succ, method="wald")
#OR 2.9 for success with small compared to large

# Slide 34:

#TABLE AGGREGATING OVER SUCCESS
Stone_Treat<-margin.table(mytable,c(3,1))
oddsratio(Stone_Treat, method="wald")
#SMALL STONES HAVE BEEN TREATED WITH B

###############################################
# RxC TABLES
##############################################

# Slide 36:

caffeine <- read.table("data/caffeine2.txt",header=TRUE, dec=".")
mytable <- xtabs(Count ~ Marital + Caffeine, data=caffeine)
mytable

#ROW TOTALS
margin.table(mytable,1)
#COLUMN TOTALS
margin.table(mytable,2)
#TOTAL
margin.table(mytable)

# Slide 39:
#CHI 2 TEST FOR INDEPENDENCE BETWEEN CAFFEINE AND MARITAL STATUS
chisq.test(mytable, correct=FALSE) 
#SIGNIFICANT

expected(mytable)

# Slide 40:
#DESCRIBE THE DIFFERENCES:

#PLOT THE TABLE USING A MOSAIC PLOT
library(vcd)
mosaic(mytable, shade=TRUE, legend=TRUE) 

