library(car)
library(ggplot2)
library(pastecs)
library(psych)

data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/05/Data_Files/DownloadFestival1.dat',header=TRUE)
head(data, 10)

# Day 1
day1<-ggplot(data, aes(day1))
day1<-day1+geom_histogram(aes(y=..density..),colour='black',fill='white')+labs(x='hygiene score day1', y='density')
day1

# adding a perfect normal distribution along with histogram + the histogram density plot
day1<-day1+stat_function(fun=dnorm, args = list(mean=mean(data$day1,na.rm = TRUE), sd=sd(data$day1,na.rm=TRUE)),colour='black',size=1)+
  geom_density(color='red')

day1


# Day2, normal vs real
day2<-ggplot(data, aes(day2))
day2<-day2+geom_histogram(aes(y=..density..),colour='black',fill='white')+labs(x='hygiene score day 2' , y='density')
day2<- day2+stat_function(fun=dnorm, args=list(mean=mean(data$day2, na.rm=TRUE), sd=sd(data$day2, na.rm = TRUE)), colour='black', size=1)
day2<- day2+geom_density(color='red')
day2


# Day3, normal vs real
day3<-ggplot(data, aes(day3))
day3<-day3+geom_histogram(aes(y=..density..),colour='black',fill='white')+labs(x='hygiene score day 3' , y='density')
day3<- day3+stat_function(fun=dnorm, args=list(mean=mean(data$day3, na.rm=TRUE), sd=sd(data$day3, na.rm = TRUE)), colour='black', size=1)
day3<- day3+geom_density(color='red')
day3


# Drawing Q-Plots
# https://stats.stackexchange.com/questions/348438/qq-plot-and-x-y-line

# Day1
qqplot.day1<-ggplot(data, aes(sample=day1))
qqplot.day1<-qqplot.day1+stat_qq()
qqplot.day1+ggtitle('day 1 qq plot')

# Day2
qqplot.day2<-ggplot(data, aes(sample=day2))
qqplot.day2<-qqplot.day2+stat_qq()
qqplot.day2+ggtitle('day 2 qq plot')

# Day3
qqplot.day3<-ggplot(data, aes(sample=day3))
qqplot.day3<-qqplot.day3+stat_qq()
qqplot.day3+ ggtitle('day 3 qq plot')



#  Quantifying normality with number
describe(data$day1)

describe(data$day2)

describe(data$day3)


# more stats at once
describe(cbind(data$day1,data$day2,data$day3))

# another function for describing
stat.desc(cbind(data$day1,data$day2,data$day3),norm=TRUE,basic=FALSE)


# another way to do the same things
describe(data[,c('day1','day2','day3')])
stat.desc(data[,c('day1','day2','day3')], basic = FALSE,norm=TRUE)