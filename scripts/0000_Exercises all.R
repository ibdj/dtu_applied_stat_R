# PROTEIN CONSUMPTION

# SET THE PATH TO THE FOLDER WHERE YOU KEEP YOUR DATA
setwd("C:/Users/ANST/Undervisning/phd kursus i basal statistik Jan2025/Introdag")

# convert to csv first. Then:
Protein <- read.csv2("Data/Protein.csv")

#LOOK AT YOUR DATA

head(Protein)
tail(Protein)
names(Protein)

View(Protein)

#LOOK AT PROTEIN CONSUMPTION IN DENMARK
Protein[Protein$Country=="Denmark", ]
Protein[6, ]

# What is the protein comsumption in Denmark?
# Row 1 is country name, we don't want to sum that one

sum(Protein[6,-1])

# GIVE THE ROWS THE COUNTRY NAMES
row.names(Protein) <- Protein$Country
Protein["Denmark", ]

#LOOK AT THE PROTEIN CONSUMPTION FROM RED MEAT
Protein[ , "RedMeat"]

# LOOK AT PROTEIN CONSUMPTION IN DENMARK, NORWAY AND
# SWEDEN FROM RED MEAT WHITE MEAT AND EGGS

Protein[c(6, 15, 20) , c("RedMeat", "WhiteMeat", "Eggs")]

# OR

Protein[c("Denmark", "Norway", "Sweden") , c("RedMeat", "WhiteMeat", "Eggs")]

# not utilizing that we have renamed the rows:

Protein[Protein$Country %in% c("Denmark","Norway","Sweden"),
        c("RedMeat","WhiteMeat","Eggs")]

# RENAME THE VARIABLES RedMeat and WhiteMeat to
# Red and White

names(Protein)[c(2,3)] <- c("Red", "White")

names(Protein)

# SAVE THE NEW VERSION OF THE PROTEIN DATA AS A TAB
# DELIMITED TEXTFILE Protein2.txt

write.table(Protein, file = "Protein2.txt",
            sep = "\t", na = ".")

###########################
# CDC DATA
##########################

cdc <- read.csv("Data/cdc.csv")

head(cdc)

#WHAT IS THE STRUCTURE
str(cdc)

#CHANGE SMOKE100 TO A FACTOR
cdc$smoke100 <- as.factor(cdc$smoke100)

#SUMMARY
summary(cdc)

#MEAN AGE BY GENERAL HEALTH
tapply(cdc$age, cdc$genhlth, mean)

hist(cdc$age, main = "", xlab="Age" )

boxplot(cdc$age, xlab = "Age")






#CALCULATING NEW VARIABLES

cdc$hcm <- cdc$height*2.54
cdc$wkg <- cdc$weight/2.2046
cdc$wdesirekg <- cdc$wtdesire/2.2046


#GROUP THE WEIGHT VARIABLE
summary(cdc$wkg)
cdc$weight_grp <- cut(cdc$wkg,
                      breaks=c(30,63.5,76.21,88.46,227) )
summary(cdc$weight_grp)

# SORT BY GENDER AND AGE
cdc_sort <- cdc[order(cdc$gender,cdc$age), ]

#AVERAGE WEIGHT

cdc$AvWeight <- rowMeans(cdc[ ,11:12])

#MEAN WEIGHT BY GENERAL HEALTH


by(cdc[ ,11:12], cdc$genhlth,colMeans)


################################
#Merge WITH CDC DATA
#THE CDC DATA IS OUR MAIN DATA SET (x). WE WANT TO HAVE ALL
#THE SUBJECTS FROM CDC

cdc_plus <- merge(cdc2, cdc_exer, by= "id", all.x = TRUE )

# COUNT MISSING VALUES OF exerany

sum(is.na(cdc_plus$exerany))

colSums(is.na(cdc_plus))

#LIST INDICES  OF SUBJECTS WITHOUT exerany
cdc_plus$id[is.na(cdc_plus$exerany)]

#Make a new dataframe with average age
# for gender and general health

AgData <- aggregate(cdc_plus$age,
                    by=list(Gender=cdc_plus$gender,
                            Health=cdc_plus$genhlth), mean)
AgData



cdc <- read.csv("cdc.csv")

#CALCULATING NEW VARIABLES

cdc$hcm <- cdc$height*2.54
cdc$wkg <- cdc$weight/2.2046
cdc$wdesirekg <- cdc$wtdesire/2.2046

# HISTOGRAM OF WEIGHT IN kg

hist(cdc$wkg)

hist(cdc$wkg, xlab = "Weight (kg)", las = 1, main = " ",
     col = "grey", density = 40)

hist(cdc$wkg, xlab = "Weight (kg)", las = 1, main = " ",
     col = c(rep(1,5),2,rep(1,5)), density = 40,lwd=2)

hist(cdc$wkg, xlab = "Weight (kg)", las = 1, main = " ",
     col = 1:11,lwd=2)

# BOX PLOT FOR WEIGHT

boxplot(cdc$wkg, xlab = "Weight (kg)", las = 1, col = 6,horizontal = T)
rug(cdc$wkg)

boxplot(wkg ~ genhlth, data = cdc , ylab = "Weight (kg)", las = 1,
        col = 2:6, xlab = "General health")

# MAKE A NEW VARIABLE
# ASSIGNING LEVELS
cdc$genhlth.num[cdc$genhlth == "poor"] <- 1
cdc$genhlth.num[cdc$genhlth == "fair"] <- 2
cdc$genhlth.num[cdc$genhlth == "good"] <- 3
cdc$genhlth.num[cdc$genhlth == "very good"] <- 4
cdc$genhlth.num[cdc$genhlth == "excellent"] <- 5


boxplot(wkg ~ genhlth.num, data = cdc , ylab = "Weight (kg)", las = 1,
        col = 2:6, xlab = "General health", xaxt = "n")
axis(1 , at = 1:5,
     labels=c('Poor','Fair','Good','Very good','Excellent'))
#SLIGHTLY DECREASING MEANS









# BASIC SCATTER PLOT
plot(cdc$wkg, cdc$wdesirekg, xlab = "Actual weight (kg)",
     ylab = "Desired weight", las = 1, pch = 19)
abline(lm(cdc$wdesirekg ~ cdc$wkg), lty = 1, col = 3)
#ADD SMOTHED
lines(lowess(cdc$wkg, cdc$wdesirekg), lty = 1, col = 2)

#SCATTER PLOT IN EACH GROUP
cdc1 <- subset(cdc, genhlth == "poor")
cdc2 <- subset(cdc, genhlth == "fair")
cdc3 <- subset(cdc, genhlth == "good")
cdc4 <- subset(cdc, genhlth == "very good")
cdc5 <- subset(cdc, genhlth == "excellent")

par(mfrow = c(2,3))
plot(cdc1$wkg, cdc1$wdesirekg, xlab = "Actual weight (kg)",
     ylab = "Desired weight", las = 1, pch = 19, ylim = c(50, 110),
     main = "General health poor")
abline(lm(cdc1$wdesirekg ~ cdc1$wkg), lty = 1, col = 3)
#ADD SMOTHED
lines(lowess(cdc1$wkg, cdc1$wdesirekg), lty = 1, col = 2)

plot(cdc2$wkg, cdc2$wdesirekg, xlab = "Actual weight (kg)",
     ylab = "Desired weight", las = 1, pch = 19,
     main = "General health fair")
abline(lm(cdc2$wdesirekg ~ cdc2$wkg), lty = 1, col = 3)
#ADD SMOTHED
lines(lowess(cdc2$wkg, cdc2$wdesirekg), lty = 1, col = 2)

plot(cdc3$wkg, cdc3$wdesirekg, xlab = "Actual weight (kg)",
     ylab = "Desired weight", las = 1, pch = 19,
     main = "General health good")
abline(lm(cdc3$wdesirekg ~ cdc3$wkg), lty = 1, col = 3)
#ADD SMOTHED
lines(lowess(cdc3$wkg, cdc3$wdesirekg), lty = 1, col = 2)

plot(cdc4$wkg, cdc4$wdesirekg, xlab = "Actual weight (kg)",
     ylab = "Desired weight", las = 1, pch = 19,
     main = "General health very good")
abline(lm(cdc4$wdesirekg ~ cdc4$wkg), lty = 1, col = 3)
#ADD SMOTHED
lines(lowess(cdc4$wkg, cdc4$wdesirekg), lty = 1, col = 2)

plot(cdc5$wkg, cdc5$wdesirekg, xlab = "Actual weight (kg)",
     ylab = "Desired weight", las = 1, pch = 19,
     main = "General health excellent")
abline(lm(cdc5$wdesirekg ~ cdc5$wkg), lty = 1, col = 3)
#ADD SMOTHED
lines(lowess(cdc5$wkg, cdc5$wdesirekg), lty = 1, col = 2)
par(mfrow = c(1,1))

# ENHANCED SCATTERPLOT BY GENERAL HEALTH

scatterplot(wdesirekg ~ wkg | genhlth, data = cdc,
            xlab = "Actual weight (kg)",
            ylab = "Desired weight", las = 1,
            legend=list(coords="topright"),
            id =list(method="identify"),
            boxplots = "xy",col=2:6)

#WITHOUT REGRESSION LINE:

scatterplot(wdesirekg ~ wkg | genhlth, data = cdc,
            xlab = "Actual weight (kg)",
            ylab = "Desired weight", las = 1,
            legend=list(coords="topright"),
            id =list(method="identify"),
            regLine=FALSE,
            boxplots = "xy",col=2:6)


# SCATTERPLOT MATRIX

Protein <- read.delim("Protein.txt")
names(Protein)

scatterplotMatrix( ~RedMeat + WhiteMeat + Eggs + Milk +
                     Fish,
                    data = Protein,
                   smooth=list(col.smooth=2,col.spread=3,
                              lty.smooth=1,lwd.smooth=3),
                   regLine=FALSE)


# IT SEEMS THAT COUNTRIES WITH A HIGH INTAKE OF PROTEIN FROM
# MEAT ALSO HAVE HIGH INTAKE FROM EGGS. FISH IS A BIT DIFFERENT
scatterplotMatrix( ~ RedMeat + WhiteMeat + Eggs + Milk +
                     Fish, diagonal = list(method="boxplot"),
                   smooth=list(lty.smooth=1),
                   regLine=FALSE, data = Protein)


