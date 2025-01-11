# 01 01 import ####

protein <- read_excel("data/Protein.xlsx")
protein <- read.csv2("data/Protein.csv", sep = ",")

# 01 02 Look at the data ####

#view all data
View(Protein)

# see header
head(protein)

# see tail 
tail(protein)

# see data types
str(protein)

# see the names of the data
names(protein)

# change row name 

row.names(protein) <- protein$Country

# 01 03 Protein consumption in Denmark ####

consumption_dk_row <- rowSums(protein["Denmark", -1], na.rm = TRUE)

# 01 04 Denmark Norway Sweden ####

protein[c("Denmark","Norway","Sweden"), c("RedMeat","WhiteMeat")]

# 01 xx #### 

write.table(protein)

#### cdc continued ####

# convert the height in to centimeters
cdc$height_cm <- cdc$height * 2.54

# convert weight in to kg
cdc$weight_kg <- cdc$weight * 0.45359237

# convert desired weight in to kg

cdc$wtdesire_kg <- cdc$wtdesire * 0.45359237

# factor of 4 equally sized groups based on wight ####

summary(cdc$weight_kg)

# Create weight categories
df$weight_category <- cut(cdc$weight_kg,
                          breaks = quantile(cdc$weight_kg, probs = seq(0, 1, length.out = num_bins + 1)),
                          include.lowest = TRUE,
                          labels = c("Low", "Medium", "High"))


