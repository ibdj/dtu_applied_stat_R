# exercise prostate ##

# Read in the data prostate.txt

prostata <- read.delim("data/prostata.txt", sep = "")
names(prostata)
str(prostata)
#prostata$svi <- factor(prostata$svi, levels = unique(prostata$svi))
prostata$svi <- factor(prostata$svi)
prostata$gleason <- factor(prostata$gleason)
str(prostata)
# make descriptive plots of plot the lpsa, lcavol and svi.

    #lpsa
plot(prostata$lpsa ~prostata$lcavol,col=c("green","red")[prostata$svi])
    #lcavol
plot(prostata$lpsa ~prostata$lcavol)
    #svi
plot(prostata$lpsa ~prostata$svi)

# Fit an ANCOVA with lpsa as outcome and lcavol as continuous and svi as categorical covariates, with different slopes for svi=1 and svi=0

Model1 <- lm(lpsa ~ svi + lcavol, data = prostata)

# • Do a model check.

par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(Model1, which = 1:4)
par(mfrow = c(1,1))

Model2 <- lm(lpsa ~ svi + lcavol + svi:lcavol, data = prostata)

summary(Model2)
# • Reduce the initial model until there only are significant covariates left. Estimate the parameters with 95% confidence intervals.
# • Plot the estimated regression lines from the final model.
# • If time look at the rest of the covariates and fit a general linear model, which covariates are statistically significant for the level of prostate-specific antigen?