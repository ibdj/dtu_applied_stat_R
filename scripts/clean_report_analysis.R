#### clean report data ####

#### importing data ####

stat_mappingplants <- readRDS("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/MappingPlants/admin/courses/2025 01 Applied statistics and R/dtu_applied_stat_R/data/stat_mappingplants.rds")

stat_mappingplants$taxon <- as.factor(stat_mappingplants$taxon)

wide_data <- stat_mappingplants[-c(590, 288), ] |> 
  filter(rowid < 102) |> 
  pivot_wider(id_cols = plot_name, names_from = taxon, values_from = bb_num) |> 
  clean_names()

wide_data2 <- wide_data |> 
  mutate(across(-1, ~ as.numeric(as.character(.)))) |> 
  mutate(across(-1, ~ replace_na(., 0))) |> 
  filter(plot_name != "")

plot_attributes <- stat_mappingplants |> 
  group_by(plot_name) |> 
  summarise(soil_moisture = mean(mean_soil_moisture) ) |> 
  filter(plot_name != "")

wide_data2$plot_name <- as.factor(wide_data2$plot_name)
str(wide_data2)

plot_attributes$plot_name <- as.factor(plot_attributes$plot_name)

wide_data2_ <- wide_data2 |> 
  left_join(plot_attributes, by = "plot_name")

quantile(wide_data2_$soil_moisture,probs=seq(0,1,by=0.33333),type=2)

wide_data2_$soil_moisture_group <- cut(wide_data2_$soil_moisture,
                                       breaks = c(-Inf,15.07, 25.18, Inf), labels = c("low", "medium", "high"))


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

############################################################################################################

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
labs(color="Soil moisture level")

p <- ggbiplot(prcomp(scale(pca_stat)), 
              groups = soil_moisture_groups, 
              ellipse = TRUE, 
              var.axes = FALSE, 
              alpha = 0.8)

# Add a customized legend title
p <- p + labs(color = "Soil moisture level")  # Change 'color' to match your aesthetic

# Display the plot
print(p)

### all data diagnostics plot I ####

mp.PCA<- princomp(pca_stat, cor = TRUE)
res<-pcaDiagplot(pca_stat, mp.PC, a=37)
### all data diagnostics plot II ####

par(mfrow=c(1,1))
plot(res$SDist, res$ODist, type="n")
text(res$SDist, res$ODist, labels=as.character(1:nrow(as.data.frame(res$ODist))), cex = 0.6)
abline(v = ((max(res$SDist) + min(res$SDist)) / 2), col = "red", lty = 2)  # Vertical line
abline(h = ((max(res$ODist) + min(res$ODist)) / 2), col = "red", lty = 2)  # Horizontal line




############################################################################################################
### select data filter ####
select_data <- wide_data2_[!(wide_data2_$plot_name %in% c("MP024","MP034", "MP041", "MP051", "mp086", "Mp088")), ]
# , "Mp089","mp087" could olso be considered potential outliers

select_data_pca <- select_data[, !(names(select_data) %in% c("phleum_commutatum","plot_name","soil_moisture_group","soil_moisture_group"))]

select_data_pca <- select_data_pca[, colSums(select_data_pca) != 0]

select_data <- as.data.frame(lapply(select_data, function(x) {
  if (is.list(x)) unlist(x) else x
}))



select_data_pca <- as.data.frame(lapply(select_data_pca, function(x) {
  if (is.list(x)) unlist(x) else x
}))
### select data plotting all the data against each other ####


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
library(remotes)
#install_github("rwehrens/ChemometricsWithR")
library(ChemometricsWithR)


select_pca<- PCA(scale(select_data_pca))
labeling <- as.data.frame(select_pca$loadings)

### select data basic summary I ####

summary(select_pca)

head(select_pca$loadings,n=3)

select_loadings <- select_pca$loadings

### select data basic summary II ####

select_X<-var(scale(select_data_pca))
select_T<-eigen(select_X)$vectors
sum(is.na(select_X))

### select data basic summary III ####

head(select_T,n=3)
select_Lambda<-t(select_T)%*%select_X%*%select_T
round(select_Lambda, digits=3)

### select data basic summary IIII ####

select_T[,1]
round(select_T[,1],digits=2) #Thus the most varying combination of the scaled data is this
sum(diag(select_Lambda))

### select data skreee plot ####
#par(cex.axis = 0.8, cex.lab = 0.8)
plot(100*(sum(diag(select_Lambda))-cumsum(diag(select_Lambda)))/sum(diag(select_Lambda)),type="b",
     main="Percentage Variance Unexplained",
     xlab='Number of eigenvectors included',
     ylab='Percentage of total variance',
     cex.axis = 0.5) # Reduce size of axis text)
#abline(h = 20, col = "darkgreen") 
abline(h = 10, col = "darkgreen") 
abline(v = 36, col = "darkgreen") 
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width

### select data score plot ####

library(pls)
select_soil_moisture_groups <- select_data$soil_moisture_group
select_plot_names <- select_data$plot_name

scoreplot(select_pca, cex.axis = 0.8, pch = 16, lwd=1, col = select_soil_moisture_groups)
legend("bottomright",levels(select_soil_moisture_groups), col=1:3,pch=16)
text(select_pca$scores[,1], select_pca$scores[,2], pos=3, cex=0.6, labels=select_plot_names)

### select data ggbiplot to view scores with soil moisture ####

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
par(mfrow=c(1,1))
ggbiplot(prcomp(scale(select_data_pca)),groups=select_soil_moisture_groups,ellipse=TRUE, var.axes = F, alpha = 0.8)
labs(color="Soil moisture level")
text(select_pca$scores[,1], select_pca$scores[,2], pos=2, cex=0.7, labels=select_plot_names)

x <- ggbiplot(prcomp(scale(select_data_pca)),groups=select_soil_moisture_groups,ellipse=TRUE, alpha = 0.8, var.axes = FALSE)
x <- x + text(select_pca$scores[,1], select_pca$scores[,2], pos=2, cex=0.7, labels=select_plot_names)
#labs(color="Soil moisture level")
#,  to remove the loadings
x <- x + labs(color = "Soil moisture level")  # Change 'color' to match your aesthetic


### select data diagnostics plot I ####
library(chemometrics)
select_pca_res<- princomp(select_data_pca, cor = TRUE)
select_res<-pcaDiagplot(select_data_pca, select_pca_res, a=36)
### select data diagnostics plot II ####

par(mfrow=c(1,1))
plot(select_res$SDist, select_res$ODist, type="n")
text(select_res$SDist, select_res$ODist, labels=as.character(1:nrow(as.data.frame(select_res$SDist))), cex = 0.6)
abline(v = ((max(select_res$SDist) + min(select_res$SDist)) / 2), col = "red", lty = 2)  # Vertical line
abline(h = ((max(select_res$ODist) + min(select_res$ODist)) / 2), col = "red", lty = 2)  # Horizontal line



############################################################################################################

# PLANT FUNCTIONAL GROUPS

#### importing and filtering data ####
library(tidyverse)
Book1 <- read_delim("data/Book1.csv", delim = ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

plant_func <- Book1

plant_func$func_group <- plant_func$X2 
plant_func$taxon <- plant_func$X1 

plant_func_stat <- stat_mappingplants |> 
  left_join(plant_func, by = "taxon")

plant_func_stat$bb_num <- as.numeric(plant_func_stat$bb_num)
plant_func_stat$func_group <- as.factor(plant_func_stat$func_group)


list_check <- plant_func_stat |> 
  group_by(taxon, func_group) |> 
  dplyr::summarise(sum_bb = sum(bb_num, na.rm = TRUE))
view(list_check)

plant_func_groups_sum <- plant_func_stat |> 
  group_by(plot_name, func_group, soil_moisture_group) |> 
  dplyr::summarise(sum_bb = sum(bb_num, na.rm = TRUE)) |> 
  filter(plot_name != "")
  
  
str(plant_func_groups_sum)
plant_func_groups_sum <- as.data.frame(lapply(plant_func_groups_sum, function(x) {
  if (is.list(x)) unlist(x) else x
}))

str(plant_func_groups_sum)

func_data_wide <- plant_func_groups_sum |> 
  pivot_wider(names_from = func_group, values_from = sum_bb, values_fill = 0)

func_data_wide$rowsum <- rowSums(func_data_wide[, 2:ncol(func_data_wide)])

func_data_wide <- func_data_wide[, colSums(func_data_wide) != 0]

func_data_wide <- func_data_wide |> 
  filter(rowsum != 0)

#### func plant PCA ####
library(remotes)
#install_github("rwehrens/ChemometricsWithR")
library(ChemometricsWithR)

func_pca_data <- func_data_wide[,2:ncol(func_data_wide-1)]
func_pca_data <- func_pca_data[, colSums(func_pca_data) != 0]
func_pca_data <- func_pca_data[rowSums(func_pca_data) != 0,]


func_pca<- PCA(scale(func_pca_data))
labeling <- as.data.frame(func_pca$loadings)

### func plant basic summary I ####

summary(func_pca)

head(func_pca$loadings,n=3)

select_loadings <- func_pca$loadings

### func plant basic summary II ####

func_X<-var(scale(func_data_wide[,2:ncol(func_data_wide)]))
func_T<-eigen(func_X)$vectors
sum(is.na(func_X))

### func plant basic summary III ####

head(func_T,n=3)
func_Lambda<-t(func_T)%*%func_X%*%func_T
round(func_Lambda, digits=3)

### func plant basic summary IIII ####

func_T[,1]
round(func_T[,1],digits=2) #Thus the most varying combination of the scaled data is this
sum(diag(func_Lambda))

### func plant skreee plot ####
#par(cex.axis = 0.8, cex.lab = 0.8)
plot(100*(sum(diag(func_Lambda))-cumsum(diag(func_Lambda)))/sum(diag(func_Lambda)),type="b",
     main="Percentage Variance Unexplained",
     xlab='Number of eigenvectors included',
     ylab='Percentage of total variance',
     cex.axis = 0.5) # Reduce size of axis text)
#abline(h = 20, col = "darkgreen") 
abline(h = 10, col = "darkgreen") 
abline(v = 36, col = "darkgreen") 
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width
### func plant score plot ####

library(pls)
select_soil_moisture_groups <- select_data$soil_moisture_group
func_plot_names <- func_data_wide$plot_name
par(mfrow=c(1,1))
scoreplot(func_pca, cex.axis = 0.8, pch = 16, lwd=1, col = soil_moisture_groups)
#legend("bottomright",levels(select_soil_moisture_groups), col=1:3,pch=16)
text(func_pca$scores[,1], func_pca$scores[,2], pos=3, cex=0.6, labels=func_plot_names)

### func plant diagnostics plot I ####
library(chemometrics)
func_pca_res<- princomp(func_pca_data, cor = TRUE)
func_res<-pcaDiagplot(func_pca_data, func_pca_res, a=9)

### func plant diagnostics plot II ####

par(mfrow=c(1,1))
plot(func_res$SDist, func_res$ODist, type="n")
text(func_res$SDist, func_res$ODist, labels=as.character(1:nrow(as.data.frame(func_res$SDist))), cex = 0.6)
abline(v = ((max(func_res$SDist) + min(func_res$SDist)) / 2), col = "red", lty = 2)  # Vertical line
abline(h = ((max(func_res$ODist) + min(func_res$ODist)) / 2), col = "red", lty = 2)  # Horizontal line