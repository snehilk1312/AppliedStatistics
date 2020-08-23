library(ggplot2)
library(car)
library(pastecs)
library(psych)
library(cowplot)
library(gridExtra)
data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/05/Data_Files/RExam.dat',header=TRUE)
head(data, 15)


data$uni <-factor(data$uni,levels = c(0:1), labels = c('DunceTown','Sussex'))

is.factor(data$uni)

# exam marks histogram
examplot<-ggplot(data, aes(exam))
examplot<-examplot+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
examplot<-examplot+
  stat_function(fun=dnorm, args=list(mean=mean(data$exam,na.rm = TRUE),sd=sd(data$exam,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')



# computer literacy 
complot<-ggplot(data, aes(computer))
complot<-complot+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
complot<-complot+
  stat_function(fun=dnorm, args=list(mean=mean(data$computer,na.rm = TRUE),sd=sd(data$computer,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')



# lectures attended
lectureplot<-ggplot(data, aes(lectures))
lectureplot<-lectureplot+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
lectureplot<-lectureplot+
  stat_function(fun=dnorm, args=list(mean=mean(data$lectures,na.rm = TRUE),sd=sd(data$lectures,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')



# numeracy
numeracyplot<-ggplot(data, aes(numeracy))
numeracyplot<-numeracyplot+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
numeracyplot<-numeracyplot+
  stat_function(fun=dnorm, args=list(mean=mean(data$numeracy,na.rm = TRUE),sd=sd(data$numeracy,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')
numeracyplot

# using cowplot library, combined multiple plot
plot_grid(examplot, complot, lectureplot, numeracyplot, labels = "AUTO")

# using gridExtra library, compined multiple plots
grid.arrange(examplot, complot,lectureplot,numeracyplot, ncol=2,nrow=2)


stat.desc(data[,cbind('exam', 'computer','lectures','numeracy')], norm=TRUE,basic=FALSE)



# Running the analysis on different groups, i.e as pandas groupby

by(data[,cbind('exam', 'computer', 'lectures','numeracy')], data$uni, describe)

# or also, 
by(data[,cbind('exam', 'computer', 'lectures','numeracy')], data$uni, stat.desc, basic=FALSE, norm=TRUE)

# dividing data as per uni
dunceData<-subset(data, data$uni=='DunceTown')
Sussex<-subset(data, data$uni=='Sussex')


# DunceTown PLot
# exam marks histogram
examplot1<-ggplot(dunceData, aes(exam))
examplot1<-examplot1+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
examplot1<-examplot1+
  stat_function(fun=dnorm, args=list(mean=mean(dunceData$exam,na.rm = TRUE),sd=sd(dunceData$exam,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')


# numeracy
numeracyplot1<-ggplot(dunceData, aes(numeracy))
numeracyplot1<-numeracyplot1+geom_histogram(colour='black',bins=15,fill='white',aes(y=..density..))
numeracyplot1<-numeracyplot1+
  stat_function(fun=dnorm, args=list(mean=mean(dunceData$numeracy,na.rm = TRUE),sd=sd(dunceData$numeracy,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')


# Sussex PLot
# exam marks histogram
examplot2<-ggplot(Sussex, aes(exam))
examplot2<-examplot2+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
examplot2<-examplot2+
  stat_function(fun=dnorm, args=list(mean=mean(Sussex$exam,na.rm = TRUE),sd=sd(Sussex$exam,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')


# numeracy
numeracyplot2<-ggplot(Sussex, aes(numeracy))
numeracyplot2<-numeracyplot2+geom_histogram(colour='black',bins=15,fill='white',aes(y=..density..))
numeracyplot2<-numeracyplot2+
  stat_function(fun=dnorm, args=list(mean=mean(Sussex$numeracy,na.rm = TRUE),sd=sd(Sussex$numeracy,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')

# plotting above things
grid.arrange(examplot1,numeracyplot1,examplot2,numeracyplot2,ncol=2, nrow=2, top='Joint Plots of numeracy and exam score of ( Duncetown vs Sussex )')



# DunceTown PLot
# computer marks histogram
computerplot1<-ggplot(dunceData, aes(computer))
computerplot1<-computerplot1+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
computerplot1<-computerplot1+
  stat_function(fun=dnorm, args=list(mean=mean(dunceData$computer,na.rm = TRUE),sd=sd(dunceData$computer,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')


# lectures
lecplot1<-ggplot(dunceData, aes(lectures))
lecplot1<-lecplot1+geom_histogram(colour='black',bins=15,fill='white',aes(y=..density..))
lecplot1<-lecplot1+
  stat_function(fun=dnorm, args=list(mean=mean(dunceData$lectures,na.rm = TRUE),sd=sd(dunceData$lectures,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')


# Sussex PLot
# exam marks histogram
computerplot2<-ggplot(Sussex, aes(computer))
computerplot2<-computerplot2+geom_histogram(colour='black',bins=30,fill='white',aes(y=..density..))
computerplot2<-computerplot2+
  stat_function(fun=dnorm, args=list(mean=mean(Sussex$computer,na.rm = TRUE),sd=sd(Sussex$computer,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')


# lectures
lecplot2<-ggplot(Sussex, aes(lectures))
lecplot2<-lecplot2+geom_histogram(colour='black',bins=15,fill='white',aes(y=..density..))
lecplot2<-lecplot2+
  stat_function(fun=dnorm, args=list(mean=mean(Sussex$lectures,na.rm = TRUE),sd=sd(Sussex$lectures,na.rm = TRUE)), colour='black', size=1)+
  geom_density(colour='red')

# plotting above things
grid.arrange(computerplot1,lecplot1,computerplot2,lecplot2,ncol=2, nrow=2, top='Joint Plots of numeracy and exam score of ( Duncetown vs Sussex )')



# Doing Statistical tests for normality assumptions

# Shapiro-wilk test whole Data
shapiro.test(data$exam) # non -normal
shapiro.test(data$computer) # normal
shapiro.test(data$lectures) # normal
shapiro.test(data$numeracy) # non-normal


# Shapiro-wilk test Duncetown Data
shapiro.test(dunceData$exam) # normal
shapiro.test(dunceData$computer) # normal
shapiro.test(dunceData$lectures) # normal
shapiro.test(dunceData$numeracy) # non-normal


# Shapiro-wilk test Sussex Data
shapiro.test(Sussex$exam) # normal
shapiro.test(Sussex$computer) # non-normal
shapiro.test(Sussex$lectures) # normal
shapiro.test(Sussex$numeracy) # non-normal


# qqplots
qqplot_exam<- ggplot(data, aes(sample=exam))
qqplot_exam<-qqplot_exam+stat_qq()
qqplot_exam<-qqplot_exam+ggtitle('qqplot Exam')


qqplot_numeracy<- ggplot(data, aes(sample=numeracy))
qqplot_numeracy<-qqplot_numeracy+stat_qq()
qqplot_numeracy<-qqplot_numeracy+ggtitle('qqplot numeracy')

grid.arrange(qqplot_exam,qqplot_numeracy,ncol=2)





# Doing Statistical tests for Homogeneity of variance assumption
# leveneTest() from car package

leveneTest(data$exam, data$uni, center = median)  # assumptions met
leveneTest(data$numeracy, data$uni, center = median)  # assumptions not  met

