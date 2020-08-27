library(ggplot2)
library(WRS)
library(pastecs)
library(reshape)

df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/09_ComparingTwoMeans/Data_Files/Field&Hole.dat', header = TRUE)

head(df)

df$id<- gl(500, 1,500, labels=c(1:500))

df_<- melt(df,id.vars = 'id', measure.vars = c('women', 'statbook') )

bar<- ggplot(df_, aes(variable, value))
bar<- bar+
  stat_summary(fun=mean, geom='bar', fill='white', colour='black')+
  stat_summary(fun.data=mean_cl_normal, geom='errorbar', width=0.2)+
  labs(x='book', y='value')
bar

t_stat<-t.test(df$women, df$statbook, paired=TRUE)
t_stat

t<- t_stat$statistic[[1]]
dof<- 499
r<- sqrt(t^2/(t^2+dof))
r


# robust test
yuend(df$women, df$statbook)
