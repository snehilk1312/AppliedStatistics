library(car)
library(mlogit)


df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/08_LogisticRegression/Data_Files/condom.dat', header=TRUE)

head(df, 15)

names(df)

unique(df$use)
unique(df$gender)
unique(df$previous)

df$use<- factor(df$use, levels=c('Unprotected', 'Condom Used'))
df$gender<- factor(df$gender, levels=c("Male", "Female"))
df$previous<- factor(df$previous, levels = c("No Condom", "Condom used", "First Time with partner"))

is.factor(df$use)
is.factor(df$gender)
is.factor(df$previous)

m00<- glm(use~1, data=df, family=binomial())
m01<- glm(use~gender+perceive+safety, data=df, family=binomial())
m02<- update(m01, .~.+previous+selfcon+sexexp)

anova(m00, m01, m02)

chisq.prob<- 1-pchisq(17.799, 4)
chisq.prob

# so, m02 is significant improvement over m01. Also , it seems that in model 2 , dummy coding is being done by itself. 
# by seeing the df of residuals.

summary(m02)

exp(m02$coefficients)

# also , checking assumptions
car::vif(m02)

df$predicted<- m02$fitted.values
df[c(12, 53, 75),]
