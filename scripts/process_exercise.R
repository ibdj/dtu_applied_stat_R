# process exercise ####

#1. Plot the variables and make a graphical assessment. Which variables could be helpful in explaining process loss?

plot(process[,-1])

process <- read.delim("data/process.txt")
names(process)

library(car)
scatterplotMatrix(~ loss + airflow + watertemp + acidconc,
                  diagonal = list(method="boxplot"),data = process)

# all seem to have some relationship

#2. Using simple linear regression, assess whether air flow, water temperature and acid concentration have an influence on process loss.

reg01 <- lm(loss ~ airflow, data = process)
summary(reg01)

reg02 <- lm(loss ~ watertemp, data = process)
summary(reg02)

reg03 <- lm(loss ~ acidconc, data = process)
summary(reg03)

#3 Now use a multiple linear regression to assess the effects of air flow, water temperature and acid concentration on process loss.

reg1 <- lm(loss ~ airflow + watertemp + acidconc, data = process)
summary(reg1)

# Call:
#   lm(formula = loss ~ airflow + watertemp + acidconc, data = process)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.2377 -1.7117 -0.4551  2.3614  5.6978 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -39.9197    11.8960  -3.356  0.00375 ** 
#   airflow       0.7156     0.1349   5.307  5.8e-05 ***
#   watertemp     1.2953     0.3680   3.520  0.00263 ** 
#   acidconc     -0.1521     0.1563  -0.973  0.34405    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.243 on 17 degrees of freedom
# Multiple R-squared:  0.9136,	Adjusted R-squared:  0.8983 
# F-statistic:  59.9 on 3 and 17 DF,  p-value: 3.016e-09

par(mfrow=c(2, 2))
plot(reg1, which=1:4)
