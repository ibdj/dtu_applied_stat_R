# exercise Berkeley #####

#• Read in the data admission.txt into a three-way table.
          admission <- read.table("data/admission.txt",header=TRUE, dec=".")
          
          head(admission)
          
          # Department Sex Admitted Count
          # 1          A   F       No    19
          # 2          A   F      Yes    89
          # 3          A   M       No   313
          # 4          A   M      Yes   512
          # 5          B   F       No     8
          # 6          B   F      Yes    17
          
          admission2_2 <- xtabs(Count ~ Sex + Admitted, data = admission)
          ftable(admission2_2)
          admission2_2 <- margin.table(colourTab, 1)
          margin.table(admission2_2, 1:2)

#• Calculate the OR and RR (with 95% confidence limits) for men being admitted compared to women not taking department into account.
          
          prop.table(admission2_2,1)
          'margin.table(admission2_2, 2)
          
          library(vcd)
          mosaic(admission2_2, shade = TRUE, legend = TRUE)

          library(epitools)
          epitools::oddsratio(admission2_2, method = "wald")
          
          # $data
          # Admitted
          # Sex       No  Yes Total
          # F     1278  557  1835
          # M     1493 1198  2691
          # Total 2771 1755  4526
          # 
          # $measure
          # odds ratio with 95% C.I.
          # Sex estimate    lower    upper
          # F  1.00000       NA       NA
          # M  1.84108 1.624377 2.086693
          # 
          # $p.value
          # two-sided
          # Sex midp.exact fisher.exact chi.square
          # F         NA           NA         NA
          # M          0 4.835903e-22 7.8136e-22
          # 
          # $correction
          # [1] FALSE
          
#• Is the probability for being admitted the same for men and women?
          
          No:
          
          prop.table(admission2_2,1)
          # Admitted
          # Sex        No       Yes
          # F 0.6964578 0.3035422
          # M 0.5548123 0.4451877
          
#• Do all departments have the same probability of admitting? Illustrate the results with proportions and a plot.
          admission2_3 <- xtabs(Count ~ Department + Sex + Admitted, data = admission)
          ftable(admission2_3)
          prop.table(admission2_3,1)
          
           epitools::oddsratio(admission2_3, method = "wald")
          
#• Do men and women apply to the same departments? Illustrate the results with proportions and a plot.
          mosaic(admission2_3, shade = TRUE, legend = TRUE)
          
          
          admission2_4 <- xtabs(Count ~ Sex + Admitted + Department, data = admission)
          mosaic(admission2_4, shade = TRUE, legend = TRUE)
          
#• Why is department a potential confounder for the effect of gender on being admitted?
          
#• Calculate the OR and RR for men vs. women admitted for each department separately.