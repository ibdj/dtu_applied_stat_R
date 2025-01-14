# exersice 

#1 Get the data lbw.txt. We would like to compare birth weight for different weight of mothers'.

  lbw <- read.delim("data/lbw.txt")
  names(lbw)
#2 Create a new variable Weight_group grouping maternal weight four equally sized groups.
  # LWT is the weight of the mother on pound

  summary(lbw$lwt)
  
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 36.36   50.00   55.00   59.01   63.64  113.64 
  
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 80.0   110.0   121.0   129.8   140.0   250.0 

  # Define breakpoints for the groups
  breakpoints <- c(-Inf, 110, 121, 140, Inf)
  
  lbw$lwt_category <- cut(lbw$lwt, breaks = breakpoints, labels = c("Low", "Medium-Low", "Medium-High", "High"))

  summary(lbw$lwt_category)  
  
  
#3 Illustrate the birth weight in the four weight groups with a figure.
  boxplot(data = lbw, )
  
#4 Is the mean birth weight the same in the four maternal weight groups?
#5 Remember to check the model assumptions.
#6 Use pairwise tests adjusting for multiplicity to see where there might be differences.