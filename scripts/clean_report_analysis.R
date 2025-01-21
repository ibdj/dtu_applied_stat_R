#### clean report data ####

soil_moisture_groups <- wide_data2_$soil_moisture_group

plot_data <- wide_data2_[2:78]

plot_data <- as.data.frame(lapply(plot_data, function(x) {
  if (is.list(x)) unlist(x) else x
}))

str(plot_data)
dim(plot_data)

par(mfrow = c(2,2), cex.axis = 0.8, cex.lab = 0.8)
for (i in 2:77) {boxplot(plot_data[,i] ~ soil_moisture_groups, col = 2:4, 
                        main=paste(names(plot_data)[i]))
  }
par(mfrow = c(1,1))

summary(wide_data2_)

### all data plotting all the data against eachother ####
par(mar = c(1, 1, 1, 1))
x11(width = 10, height = 8)
plot(wide_data2_)

for (i in 2:77) {plot(plot_data[,i] ~ plot_data[,i], col = 2, 
                         main=paste(names(plot_data)[i]))
  }
  
# Save pairwise plots as PNG files
for (i in 2:77) {
  for (j in 2:77) {
    if (i != j) {
      png(filename = paste0("plot_", names(plot_data)[i], "_vs_", names(plot_data)[j], ".png"))
      plot(
        plot_data[, i] ~ plot_data[, j],
        col = 2,
        main = paste(names(plot_data)[i], "vs", names(plot_data)[j]),
        xlab = names(plot_data)[j],
        ylab = names(plot_data)[i]
      )
      dev.off()
    }
  }
}


pdf("pairwise_plots.pdf")  # Open a PDF device

# Loop through each pair of species
num_species <- ncol(plot_data)
for (i in 2:num_species) {
  for (j in 2:num_species) {
    if (i != j) { # Avoid self-comparison
      plot(
        plot_data[, i] ~ plot_data[, j],
        col = 2,
        main = paste(names(plot_data)[i], "vs", names(plot_data)[j]),
        xlab = names(plot_data)[j],
        ylab = names(plot_data)[i]
      )
    }
  }
}

dev.off()  # Close the PDF device

### all data PCA #####
pca.1<- PCA(scale(pca_stat[ ,-c(1,76)]))
labeling <- as.data.frame(pca.1$loadings)

### all data score plot ####
scoreplot(mp.PC, cex.axis = 0.7, pch = 16, lwd=1, col = soil_moisture_groups)
text(mp.PC$scores[,1], mp.PC$scores[,2], pos=2, cex=0.7, labels=plot_names)
legend("bottomright",levels(soil_moisture_groups), col=1:3,pch=16)

### all data ggbiplot to view scores with soil moisture ####

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(prcomp(scale(pca_stat)),groups=soil_moisture_groups,ellipse=FALSE)
