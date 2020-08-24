# writingfunctions in R
# to get mean

meanOfVariable <- function(variables){
  mean <-sum(variables)/length(variables)
  cat("Mean: ", mean)
}

meanOfVariable(c(1,2,3,4))



data<-read.csv('/home/atrides/Desktop/R/statistics_with_R/06/Data_Files/pbcorr.csv', header=TRUE)

head(data)


# point-biserial correaltion
cor.test(data$time, data$gender, method = 'pearson')


# biserial correlation
catFrequencies<-table(data$gender)
proportions(catFrequencies)
r_pb = (0.378*sqrt(0.533*0.467))/.3977
r_pb
# or use function
polyserial(data$time, data$gender)


# Partial Correlation , using ggm package
data<-read.delim('/home/atrides/Desktop/R/statistics_with_R/06/Data_Files/Exam Anxiety.dat',header=TRUE)
data<-data[, c('Exam', 'Anxiety', 'Revise')]

pc<-pcor(c('Exam', 'Anxiety' , 'Revise'), var(data))  # first two variable passed inside c are the req variables and all other 
# variables in c() are control variables
pc
pc^2

# pcor siginificance 
pcor.test(pc, 1, 103)  # (pcor.object, no.of control variables, sample size)
