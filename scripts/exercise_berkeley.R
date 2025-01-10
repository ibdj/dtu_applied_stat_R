# exercise Berkeley #####

#• Read in the data admission.txt into a three-way table.
colour_dat <- read.table("data/Colour.txt",header=TRUE, dec=".")
#• Calculate the OR and RR (with 95% confidence limits) for men being admitted compared to women not taking department into account.
#• Is the probability for being admitted the same for men and women?
#• Do all departments have the same probability of admitting? Illustrate the results with proportions and a plot.
#• Do men and women apply to the same departments? Illustrate the results with proportions and a plot.
#• Why is department a potential confounder for the effect of gender on being admitted?
#• Calculate the OR and RR for men vs. women admitted for each department separately.