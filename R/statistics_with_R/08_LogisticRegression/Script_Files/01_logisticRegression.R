library(car)
library(mlogit)


df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/08_LogisticRegression/Data_Files/eel.dat', header=TRUE)

# listing all columns in data frame
names(df)

# checking whether the passed columns ae factor or not
is.factor(df$Cured)
is.factor(df$Intervention)

# converting column to a factor 
df$Cured<- as.factor(df$Cured)
df$Intervention<- as.factor(df$Intervention)


# Default factors were not suitable. So refactoring the revels
df$Cured<- relevel(df$Cured, "Not Cured")
df$Intervention<- relevel(df$Intervention, "No Treatment")

# fitting the model
# newModel<-glm(outcome ~ predictor(s), data = dataFrame, family = name of a distribution, na.action = an action)
m01<- glm(Cured~Intervention, data=df, family = binomial())
m02<- glm(Cured~Intervention+Duration, data=df, family = binomial())
m00<- glm(Cured~1, data=df, family = binomial())


# printing summary
summary(m00)
summary(m01)
summary(m02)


# Accessing some other statistics of our Logmodel
m01$null.deviance
m01$deviance
m01$coefficients


# to see what all statistics are there , we  could do as follows
names(m01)


# getting some critical statistics, model chi square and its significance
modelChi<- m01$null.deviance - m01$deviance
modelChi

chidf<- m01$df.null-m01$df.residual
chidf

# feeding model chi square and its degree of freedom to calculate the p value
chisq.prob<- 1-pchisq(modelChi , chidf)
chisq.prob

# Note: we reject the null model that our model 'm01' is not better than just chance to predict outcome


# Now we will calculate various different R and R^2

R<- sqrt((3.074^2-2*1)/m01$null.deviance)
R

pseudoRsquared<- function(m){
  dev<- m$deviance
  nulldev<- m$null.deviance
  n<- length(m$fitted.values)
  R2_hl<- 1-dev/nulldev
  R2_cs<- 1-exp(-(nulldev-dev)/n)
  R2_n<-  R2_cs/(1-(exp(-(nulldev/n))))
  cat("Pseudo R^2 for logistic regression: \n")
  cat("Hosmer and Lemeshow R^2: ", round(R2_hl, 3), "\n")
  cat("Cox and Snell R^2: ", round(R2_cs ,3), "\n")
  cat("Nagelkerke R^2: ", round(R2_n, 3),"\n")
}

pseudoRsquared(m01)



# odds Ratio
exp(m01$coefficients)

# confidence interval of these odds, as it doesn't cross 1 , so it says as intervention is done odds of
# being cured increases
exp(confint(m01))



 
# Model 2 , Intervention and Duration as predictor
summary(m02)
modelChi<- m01$deviance - m02$deviance
modelChi

chidf<- m01$df.residual - m02$df.residual
chidf

chisq.prob<- 1 - pchisq(modelChi, chidf)
chisq.prob

# from above chisq.prob , we can conclude that model 2 is not such an improvement over model 1

# also doing anova
anova(m01, m02)


# Doing casewise diagnostics
df$predicted.probablities<- fitted(m01)
df$standarized.residuals<- rstandard(m01)
df$studentized.residuals<- rstudent(m01)
df$dfbeta<- dfbeta(m01)
df$dffits<- dffits(m01)
df$leverage<- hatvalues(m01)

head(df)
# by seeing the residuals we can see that  none of the case to be seem an outlier
head(df[order(-df$standarized.residuals),]$standarized.residuals, 10)

# all cases have DFBetas less than 1, and leverage statistics are very close to the calculated expected value of 0.018.
# All in all, this means that there are no influential cases having an effect on the model. 
# The studentized residuals all have values of less than ±2 and so there seems to be very little here to concern us.




# Another Example
data<- read.delim('/home/atrides/Desktop/R/statistics_with_R/08_LogisticRegression/Data_Files/penalty.dat', header=TRUE)
head(data)

# checking if Scored is a factor or not
is.factor(data$Scored)

# it  is not , so
data$Scored<- as.factor(data$Scored)

names(data)
m01<- glm(Scored~PSWQ+Previous, data=data, family=binomial())
m02<- glm(Scored~PSWQ+Previous+Anxious, data=data, family=binomial())

anova(m01, m02)

summary(m01)
summary(m02)


modelChi1<- m01$null.deviance - m01$deviance
modelChi1

chidf1<- m01$df.null - m01$df.residual
chidf1

chisq.prob1<- 1- pchisq(modelChi1, chidf1)
chisq.prob1

# the chisquare probability 'chisq.prob1' value is less than 0.05 which tells that this
# model was quite an improvement over a null model(i.e just chance)


# Now we will see whether model 2 is any improvement over model 1
modelChi2<- m01$deviance - m02$deviance
modelChi2

chidf2<- m01$df.residual - m02$df.residual
chidf2

chisq.prob2<- 1-pchisq(modelChi2, chidf2)
chisq.prob2

# the chisquare probability 'chisq.prob2' value is greater than 0.05 , which tells that this
# model(i.e m02) was a improvement over m01  , just by chance.

# dataframe of studentized residuals
df_resid<- rstudent(m01)
# printing the head, i.e top 10 residuals
head(df_resid[order(-df_resid)], 10)

# now , we will head to model m02, for assumption checking

# Testing for multicollinearity

# vif
vif(m02)

# tolerance
1/vif(m02)

# from the output of  vif and tolerance , we can deduce that there is a high multicollinearity in our model 

# checking correlation between different independent variables
cor(data[, cbind('PSWQ', 'Anxious', 'Previous')])
# from the above table , the correlation b/w Anxious and Previous is very high,  thus leading to high multicollinearity


# Testing for linearity of logit
data$logPSWQ<- data$PSWQ * log(data$PSWQ)
data$logAnxious<- data$Anxious * log(data$Anxious)
data$logPrevious<- data$Previous * log(data$Previous)

head(data)
m03<- glm(Scored~PSWQ+logPSWQ+Anxious+logAnxious+Previous+logPrevious, data=data, family=binomial())
summary(m03)
# From the summary output , if any interaction term has significance less than 0.05 , it will mean that assumption
# of linearity has been violated. In our output we can conclude that the assumption of linearity has been met as all 
# interaction term is non-significant


