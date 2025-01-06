# read the data ####

cdc <- read_csv("data/cdc.csv")
str(cdc)

# look at the data ####

names(cdc)
summary(cdc)
str(cdc)
head(cdc)

# small transformation ####

cdc$gender <- factor(cdc$gender)

str(cdc)


# basic stats ####

tapply(cdc$height, cdc$gender, mean)

# data structure description ####

#1 a data frame of 207 observations/rows of data and 9 variables
#2 numerical data on the hight, weight, age of tests persons
#3 character of factor variables of general health, if they smoke, if they have a health plan and gender.

# change smoke100 to a factor ####

cdc$smoke100 <- factor(cdc$smoke100)

cdc$genhlth <- factor(cdc$genhlth)
cdc$hlthplan <- factor(cdc$hlthplan)

# summarrize the variables in the dataset ####

summary(cdc)

# calculate the mean age by general health ####

tapply(cdc$age, cdc$genhlth, mean)

# histogram of the age ####

hist(cdc$age, main = "age", xlab = "age")

# box plot of age of the participants ####

boxplot(cdc$age, xlab = "age")

hist(cdc$weight_kg, , las = 1, main = "Weight of participants (kg)", col = 3, density = 20, xlab = "Bins of weight (kg)")

boxplot(cdc$weight_kg, xlab = "Weight (Kg)")

boxplot(data = cdc, weight_kg ~ genhlth , xlab = "Weight (Kg)", col = 1:5)

head(cdc)

health_mapping <- c("poor" = 1, "fair" = 2, "good" = 3, "very good" = 4, "excellent" = 5)
cdc$genhlth_num <- health_mapping[cdc$genhlth]
