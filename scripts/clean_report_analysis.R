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
legend("bottomright",levels(soil_moisture_groups), col=1:3,pch=16)
text(mp.PC$scores[,1], mp.PC$scores[,2], pos=3, cex=0.8, labels=plot_names)

### all data ggbiplot to view scores with soil moisture ####

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(prcomp(scale(pca_stat)),groups=soil_moisture_groups,ellipse=TRUE, var.axes = F, alpha = 0.8)
text(mp.PC$scores[,1], mp.PC$scores[,2], pos=2, cex=0.7, labels=plot_names)
  #labs(color="Soil moisture level")


### all data diagnostics plot I ####

mp.PCA<- princomp(pca_stat, cor = TRUE)
res<-pcaDiagplot(pca_stat, mp.PC, a=37)
### all data diagnostics plot II ####

par(mfrow=c(1,1))
plot(res$SDist, res$ODist, type="n")
text(res$SDist, res$ODist, labels=as.character(1:178))
abline(v = 5, col = "red", lty = 2)  # Vertical line
abline(h = 3, col = "red", lty = 2)  # Horizontal line

# if the plot should be equal size, xlim = c(0, 10), ylim = c(0, 10)

# Create a blank plot with a 10x10 axis scale
plot(res$SDist, res$ODist, xlim = c(0, 10), ylim = c(0, 10), 
     xlab = "Score Distance (SD)", ylab = "Orthogonal Distance (OD)", 
     main = "Score Distance vs Orthogonal Distance", type = "n")

# Add vertical and horizontal lines at x = 5 and y = 5
abline(v = 5, col = "red", lty = 2)  # Vertical line
abline(h = 5, col = "red", lty = 2)  # Horizontal line



### select data filter ####
select_data <- wide_data2_[!(wide_data2_$plot_name %in% c("MP024","MP034", "MP041", "MP051", "mp086", "Mp088")), ]

select_data_pca <- select_data[, !(names(select_data) %in% c("phleum_commutatum","plot_name","soil_moisture_group","soil_moisture_group"))]

select_data_pca <- select_data_pca[, colSums(select_data_pca) != 0]

select_data <- as.data.frame(lapply(select_data, function(x) {
  if (is.list(x)) unlist(x) else x
}))



select_data_pca <- as.data.frame(lapply(select_data_pca, function(x) {
  if (is.list(x)) unlist(x) else x
}))
### select data plotting all the data against eachother ####
par(mar = c(1, 1, 1, 1))
x11(width = 10, height = 8)
plot(select_data)

for (i in 2:77) {plot(plot_data[,i] ~ plot_data[,i], col = 2, 
                      main=paste(names(plot_data)[i]))
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

### select data PCA #####

select_pca<- PCA(scale(select_data_pca))
labeling <- as.data.frame(select_pca$loadings)

### select data basic summary II ####

summary(select_pca)

head(select_pca$loadings,n=3)

### select data basic summary II ####

### select data score plot ####

select_soil_moisture_groups <- select_data$soil_moisture_group
select_plot_names <- select_data$plot_name

scoreplot(select_pca, cex.axis = 0.7, pch = 16, lwd=1, col = select_soil_moisture_groups)
legend("bottomright",levels(select_soil_moisture_groups), col=1:3,pch=16)
text(select_pca$scores[,1], select_pca$scores[,2], pos=3, cex=0.8, labels=select_plot_names)

### select data ggbiplot to view scores with soil moisture ####

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(prcomp(scale(select_data_pca)),groups=select_soil_moisture_groups,ellipse=TRUE, var.axes = F, alpha = 0.8)
text(select_pca$scores[,1], select_pca$scores[,2], pos=2, cex=0.7, labels=select_plot_names)
#labs(color="Soil moisture level")


### select data diagnostics plot I ####

select_pca_res<- princomp(select_data_pca, cor = TRUE)
res<-pcaDiagplot(select_data_pca, select_pca_res, a=37)
### select data diagnostics plot II ####

par(mfrow=c(1,1))
plot(res$SDist, res$ODist, type="n")
text(res$SDist, res$ODist, labels=as.character(1:178))
abline(v = 5, col = "red", lty = 2)  # Vertical line
abline(h = 3, col = "red", lty = 2)  # Horizontal line

# if the plot should be equal size, xlim = c(0, 10), ylim = c(0, 10)

# Create a blank plot with a 10x10 axis scale
plot(res$SDist, res$ODist, xlim = c(0, 10), ylim = c(0, 10), 
     xlab = "Score Distance (SD)", ylab = "Orthogonal Distance (OD)", 
     main = "Score Distance vs Orthogonal Distance", type = "n")

# Add vertical and horizontal lines at x = 5 and y = 5
abline(v = 5, col = "red", lty = 2)  # Vertical line
abline(h = 5, col = "red", lty = 2)  # Horizontal line
