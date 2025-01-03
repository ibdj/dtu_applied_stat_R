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



