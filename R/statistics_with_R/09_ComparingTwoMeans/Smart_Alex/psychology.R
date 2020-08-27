library(reshape)
library(WRS)
library(ggplot2)
library(pastecs)
library(dplyr)
df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/09_ComparingTwoMeans/Data_Files/Penis.dat', header = TRUE)

df$book<-factor(df$book, levels = c(1,2), labels=c("Women are from Bras, Men are from Penis", "Marie Claire"))

bar<- ggplot(df, aes(book, happy))
bar<- bar+
  stat_summary(fun=mean, geom="bar", fill="white", colour='black')+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar', width=0.2)

bar

t_stat<-t.test(happy~book, data=df)
t_stat


# Effect Size
t<- t_stat$statistic[[1]]
dof<- (10-1)+(10-1)
r<- sqrt(t^2/(t^2+dof))
r

shapiro.test(filter(df, book=='Women are from Bras, Men are from Penis')$happy)
shapiro.test(filter(df, book=='Marie Claire')$happy)
# data is normal so no need to do robust tests
