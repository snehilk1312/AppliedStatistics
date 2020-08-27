library(car)
library(mlogit)


df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/08_LogisticRegression/Data_Files/Chat-Up Lines.dat', header=TRUE)

head(df)

# checking whether Success and Gender are factor vectors

is.factor(df$Success)
is.factor(df$Gender)

# converting Success and Gender into factor vectors
df$Success<- as.factor(df$Success)

# dont use ordered=TRUE in case of df$Gender, as it is just a nominal categorical variable
# use ordered=TRUE in case of ordinal categorical variables only
df$Gender<-  factor(df$Gender, levels = c('Male', 'Female'))

# to count the values of each factor
summary(df$Gender)
summary(df$Success)

mlchat<- mlogit.data(df, choice='Success', shape='wide')
head(mlchat , 15)

m01 <- mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex +  Funny:Gender, data = mlchat, reflevel = "No response/Walk Off")
summary(m01)

# to see what can we access of m01 object
names(m01)

data.frame(exp(m01$coefficients))


# Checking Some assumptions

# Assumption of Multicollinearity

# Collinearity is a property of the design matrix. Variance inflation factors 
# are calculated based on linear regressions of the predictors against all other predictors. That means you can just 
# fit with glm instead of mlogit and pass that fit to vif to get the vif
m02 <- glm(Success ~Good_Mate + Funny + Gender + Sex, data = df, family=binomial())
car::vif(m02)

# also correlation value will show that there is no problem of multicollinearity.So, assumption of 
# multicollinearity has been followed.
cor(df[, cbind('Good_Mate', 'Funny', 'Sex')])


# Assumption of Linearity
names(mlchat)
# creating log transformed variables for continuous  variables
mlchat$logFunny<- log(mlchat$Funny)*mlchat$Funny
mlchat$logSex<- log(mlchat$Sex)*mlchat$Sex
mlchat$logGood_Mate<- log(mlchat$Good_Mate)*mlchat$Good_Mate

rough_model <- mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex +  Funny:Gender + logFunny + logSex + logGood_Mate, data = mlchat, reflevel = "No response/Walk Off")
summary(rough_model)

# by seeing the log interaction term, its pretty clear that many values has significance p<0.05, 
# hence the assumption of linearity of logit has been violated

