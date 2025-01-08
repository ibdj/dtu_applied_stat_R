# fish exercise ####

fish <- read.delim2("data/fishgrazer.txt", sep = "")

fish$treat <- as.factor(fish$treat)
fish$block <- as.factor(fish$block)

str(fish)

#1 Make appropriate plots to investigate whether treatments or blocks have any influence on the percentage of regenerated seaweed

boxplot(cover ~ treat*block, data=fish, xlab = 'Block and Treatment',
        ylab = 'Cover', las = 1, col = 2:4)

#2 Fit a two-way analysis of variance model

model2 <- lm(cover ~ block + treat + block:treat, data = fish)
summary(model2)


  