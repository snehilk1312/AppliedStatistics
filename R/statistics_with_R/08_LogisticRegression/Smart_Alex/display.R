library(car)
library(mlogit)


df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/08_LogisticRegression/Data_Files/Display.dat', header=TRUE)

head(df,15)

# columns in dataframe
names(df)

is.factor(df$fb)
is.factor(df$display)


df$fb<- factor(df$fb, levels=c("No", "Yes"), ordered = FALSE)
df$display<- factor(df$display)

m00<- glm(display~1, data=df, family=binomial())
m01<- glm(display~fb, data=df, family=binomial())
m02<- glm(display~age+fb, data=df, family = binomial())
m03<- glm(display~age+fb+fb:age, data=df, family = binomial())

summary(m01)
summary(m02)

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
pseudoRsquared(m02)
pseudoRsquared(m03)

modelChi<- m01$null.deviance-m01$deviance
modelChi

chidf<- m01$df.null-m01$df.residual
chidf

chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

# so , model 1 is a significant improvement over null model  , with p<0.05
anova(m00, m01, m02,m03)

chisq.prob<- 1-pchisq(2.2850, 1)
chisq.prob

# so model 2 is a not significant improvement over model 1, with p>0.05
summary(m01)


# Doing casewise diagnostics
df$predicted.probablities<- fitted(m01)
df$standarized.residuals<- rstandard(m01)
df$studentized.residuals<- rstudent(m01)
df$dfbeta<- dfbeta(m01)
df$dffits<- dffits(m01)
df$leverage<- hatvalues(m01)

head(df)

head(df[order(-df$studentized.residuals),]$studentized.residuals, 10)
head(df[order(df$studentized.residuals),]$studentized.residuals, 10)

# so, none of the standarized residuals is a cause for concern 

# for odd ratio
exp(m01$coefficients)

# from seeing the outcome we can say that the child with fb = "Yes" have 15.8125 times higher odds
# of display="Yes", than those with fb="No"


