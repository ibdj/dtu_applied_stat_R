# fish exercise ####

fish <- read.delim2("data/fishgrazer.txt", sep = "")

fish$treat <- as.factor(fish$treat)
fish$block <- as.factor(fish$block)

str(fish)

#1 Make appropriate plots to investigate whether treatments or blocks have any influence on the percentage of regenerated seaweed

par(mfrow = c(1, 1))
boxplot(cover ~ treat*block, data=fish, xlab = 'Block and Treatment',
        ylab = 'Cover', las = 1, col = 2:4)

boxplot(cover ~ treat*block, data=fish, xlab = 'Block and Treatment',
        ylab = 'Cover', las = 1, col = 2:4, 
        cex.lab = 1, cex.axis = 0.5)

#2 Fit a two-way analysis of variance model
plot(fish)
plot(cover~block, data = fish)
plot(cover~treat, data = fish)
model2 <- lm(cover ~ block + treat + block:treat, data = fish)
summary(model2)
plot(model2)

#3 Check the underlying assumptions of your previous model If the assumptions were not fulfilled then improve your model with a transformation

  # Independent observations
    ## No test, but knowledge of the the experiment
    ## No connection between the block or the treatments?
  # Variance homogeneity
    ## Residuals versus tted values, look for trumpet shape.
    
    library(car)
    leveneTest(model2)
    
    library(MESS)
    wallyplot(model2)
    
    qqwrap <- function(x, y, ...) {qqnorm(y,main="",...)
      abline(a=0, b=1)}
    wallyplot(model2, FUN=qqwrap)
    
interaction.plot(fish$block, fish$treat, fish$cover, type=c("b"), ylab = "Cover", las=1, trace.label = "Treat", xlab = "Block")
  # Normally distributed observations.
    
# anova for compairing model: when the one model is nested with in the other model (e.g. by colabsing some variables in one category)
  