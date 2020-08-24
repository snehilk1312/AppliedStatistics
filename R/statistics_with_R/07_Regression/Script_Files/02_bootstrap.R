library(car)
library(QuantPsyc)
library(boot)
library(dplyr) # data mainpulation
library(cowplot)
library(ggplot2)


df2<- read.delim('/home/atrides/Desktop/R/statistics_with_R/07_Regression/Data_Files/album.dat', header=TRUE)

head(df2)

bootReg<- function(formula, data, i){
  d<- data[i, ]
  fit<- lm(formula, data=d)
  return(coef(fit))
}

bootResults<- boot(statistic = bootReg, formula=sales~adverts+airplay+attract, data=df2, R=2000)


print(boot.ci(bootResults,type='bca', index=2))

print(boot.ci(bootResults,type='bca', index=3))

print(boot.ci(bootResults,type='bca', index=4))
