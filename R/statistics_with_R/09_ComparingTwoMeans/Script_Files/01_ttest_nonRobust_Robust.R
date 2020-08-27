library(ggplot2)
library(WRS)
library(pastecs)
library(reshape)

df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/09_ComparingTwoMeans/Data_Files/SpiderLong.dat', header = TRUE)

head(df , 15)

# barplot for independent measure design , the differnce will be with errorbar when 
# we will compare it to repeated measure design
bar<- ggplot(df, aes(Group, Anxiety))
bar<- bar+
  stat_summary(fun=mean, geom='bar', fill='white', colour='black')+
  stat_summary(fun.data=mean_cl_normal, geom='errorbar', width=0.2)+
  labs(x='Groups', y='Anxiety')+
  ggtitle('Groups vs Anxiety')
bar


df_wide<- read.delim('/home/atrides/Desktop/R/statistics_with_R/09_ComparingTwoMeans/Data_Files/SpiderWide.dat', header = TRUE)
df_wide

grand_mean = mean(df$Anxiety)
grand_mean

mean_pic<- mean(df_wide$picture)
mean_pic

mean_real<- mean(df_wide$real)
mean_real

df_wide$mean<- (df_wide$picture+df_wide$real)/2
head(df_wide)

df_wide$adj<- grand_mean-df_wide$mean
df_wide$picture_adj<- df_wide$picture+df_wide$adj
df_wide$real_adj<- df_wide$real+df_wide$adj

df_wide$id<- gl(12, 1,12, labels=c(1:12))
names(df_wide)

df_long<- melt(df_wide, id.vars="id", measure.vars=c('picture_adj', 'real_adj'))
df_long

bar2<- ggplot(df_long, aes(variable, value))
bar2<- bar2+
  stat_summary(fun=mean, geom='bar', fill='white', colour='black')+
  stat_summary(fun.data=mean_cl_normal, geom='errorbar', width=0.2)+
  labs(x='Groups', y='Anxiety')+
  ggtitle('Groups vs Anxiety')
bar2

# Doing independent t-test


# when data is in long format
ttest_long<- t.test(Anxiety~Group , data=df, paired=FALSE)
ttest_long
# when data is in wide format
ttest_wide<- t.test(df_wide$picture, df_wide$real, paired=FALSE)
ttest_wide


# Robust Methods for independent ttest
# we need to have data in wide format for these robusts tests
robust_t1<- WRS::yuen(df_wide$picture, df_wide$real, tr=.2 , alpha=.05)
robust_t1


robust_t2<- WRS::yuenbt(df_wide$picture, df_wide$real, tr=.2, nboot = 599, alpha=0.05, side=F)
robust_t2


robust_t3<- WRS::pb2gen(df_wide$picture, df_wide$real, nboot=2000)
robust_t3


# Effect Size, r<- sqrt(t**2/(t**2+df))
r<- sqrt((ttest_wide$statistic[[1]])^2/((ttest_wide$statistic[[1]])^2+(24-2)))
r

# which is a medium effect size



# Doing Dependent ttest

ttest_dep<- t.test(df_wide$picture, df_wide$real, paired = TRUE)
ttest_dep

# Robust method for dependent ttest
yuend(df_wide$picture, df_wide$real)


ydbt(df_wide$picture, df_wide$real, nboot=2000)


bootdpci(df_wide$picture, df_wide$real, nboot=2000, est=tmean)


# effect size, df=11
r<- sqrt(((ttest_dep$statistic[[1]])^2)/((ttest_dep$statistic[[1]])^2+11))
r
