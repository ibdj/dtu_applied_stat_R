pcb <- read.table("data/troutpcb.txt", header=TRUE, sep="\t")

head(pcb)
str(pcb)
summary(pcb)

plot(PCB ~ Age, data=pcb)

#4 Fit the linear regression model
fm <- lm(PCB ~ Age, data=pcb)
abline(fm)
# What is your impression of the model fit — do you think you have found a reasonable model?
  # Doesnt look like a super good fit. Variation increasing with the age of the trouts.
coef(fm)
summary(fm)
#5. Perform model diagnostics and plot the residuals
par(mfrow=c(2, 2)) ## split plotting region in 4
plot(fm, which=1:4)

#Residuals vs. fitted
  # not a random point cloud; trumpet shape; log transformation log10
#Q-Q residuals
  # s-shaped
#cooks distance
  # a few very high influences

#6. Take the logarithm of PCB concentration and save the variable in the data frame
pcb$log10PCB <- log10(pcb$PCB)
## Even better R code:
## pcb <- within(pcb, log10PCB <- log10(PCB))

#7. Illustrate log PCB versus age, fit the model using log PCB and check the residuals again

par(mfrow=c(1, 1))
plot(log10PCB ~ Age, data=pcb)
fm2 <- lm(log10PCB ~ Age, data=pcb)
abline(fm2)

par(mfrow=c(2, 2))
plot(fm2, which=1:4)

#Residuals vs. fitted
  # MORE of random point cloud (but not fully?) NO trumpet shape, but there is a curve, sqaure term could fix that
#Q-Q residuals
  # LESS s-shaped 
#cooks distance
# only one 'high' influence; 28

coef(fm2)
summary(fm2)

B <- coef(summary(fm2))
df <- data.frame(Estimate = round(10^B[, 1], 2),
                 Lower = round(10^confint(fm2)[1, ], 2),
                 Upper = round(10^confint(fm2)[2, ], 2),
                 "p-value" = format.pval(B[, 4], digits=3, eps=1e-3))
df

10^coef(fm2)
#multiplicable model; increases in percent


