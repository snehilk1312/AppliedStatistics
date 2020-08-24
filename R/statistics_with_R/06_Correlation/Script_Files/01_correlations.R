library(boot)
library(ggm)
library(Hmisc)
library(ggplot2)
library(polycor)



# we have cor() , cor.test() which are part of base R system, while  rcorr() which is a part of Hmisc package.
data<-read.delim('/home/atrides/Desktop/R/statistics_with_R/06/Data_Files/Exam Anxiety.dat',header=TRUE)

head(data)
is.factor(data$Gender)

data$Gender<-factor(data$Gender)
data$GenderX<-as.numeric(data$Gender)

is.factor(data$Gender)


# Using cor()
# pearson with complete.obs
cor(data[,cbind('Code','Revise', 'Exam','Anxiety','GenderX')], use='complete.obs', method='pearson')

# kendall with complete.obs
cor(data[,cbind('Code','Revise', 'Exam','Anxiety','GenderX')], use='complete.obs', method='kendall')

# kendall with pairwise.complete.obs
cor(data[,cbind('Code','Revise', 'Exam','Anxiety','GenderX')], use='pairwise.complete.obs', method='kendall')


# using rcorr(), from Hmisc . This function does not work on dataframes, convert it into a matrix
# Note: rcorr() works only on matrix and gives, pearson and spearman only and use pairwise exclusion of missing values
dm <- data.matrix(data[,cbind('Code','Revise', 'Exam','Anxiety', 'Gender')])

rcorr(dm, type=c('pearson'))


# using cor.test(), only does one pair of variables at a time
cor.test(data$Exam, data$Anxiety, alternative = 'two.sided', method='pearson', conf.level = 0.95)


# Note:
# use pearson r for parametric 
# use spearman rho for non-parametric
# use tendall tau for non-parametric and small datasets




# Now  , if data is non-normal , we would use bootstrapping correaltion 

library(boot)
data<-read.delim('/home/atrides/Desktop/R/statistics_with_R/06/Data_Files/The Biggest Liar.dat',header=TRUE)

bootTau<-function(data, i)cor(data$Position[i], data$Creativity[i], use='complete.obs', method='kendall')
boot_kendall<-boot(data, bootTau, 2000)
boot_kendall

# 95% confidence interval
boot.ci(boot_kendall)
