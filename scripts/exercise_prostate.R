# exercise prostate ##

# Read in the data prostate.txt

prostate <- read.delim("data/p")

# make descriptive plots of plot the lpsa, lcavol and svi.
    #lpsa
    #lcavol
    #svi

# Fit an ANCOVA with lpsa as outcome and lcavol as continuous and svi as categorical covariates, with different slopes for svi=1 and svi=0
# • Do a model check.
#• Reduce the initial model until there only are significant covariates left. Estimate the parameters with 95% confidence intervals.
# • Plot the estimated regression lines from the final model.
# • If time look at the rest of the covariates and fit a general linear model, which covariates are statistically significant for the level of prostate-specific antigen?