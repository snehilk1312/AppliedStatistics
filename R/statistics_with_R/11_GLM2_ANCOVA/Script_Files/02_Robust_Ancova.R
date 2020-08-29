library(car)
library(pastecs)
library(WRS)
library(multcomp)
library(compute.es)
library(effects)
library(ggplot2)
library(cowplot)
library(dplyr)

df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/11_GLM2_ANCOVA/Data_Files/CloakofInvisibility.dat', header=TRUE)

head(df)

df$cloak<- factor(df$cloak, levels=c(1, 2), labels=c('No Cloak', 'Cloak'))

plot1<- ggplot(df, aes(cloak, mischief1))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,14))
plot2<- ggplot(df, aes(cloak, mischief2))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,14))

plot_grid(plot1, plot2, ncol=2, nrow=1)


# for robust ancova , we have to get data in right format, 
Cloak<- filter(df, cloak=="Cloak")
No_Cloak<- filter(df, cloak=="No Cloak")

covgrp1<- Cloak$mischief1
covgrp2<- No_Cloak$mischief1
dvgrp1<- Cloak$mischief2
dvgrp2<- No_Cloak$mischief2

# creating robust models
m1_robust<- ancova(covgrp1, dvgrp1, covgrp2 , dvgrp2, tr=.2)
m2_robust<- ancboot(covgrp1, dvgrp1, covgrp2 , dvgrp2, nboot=2000)

m1_robust

m2_robust

