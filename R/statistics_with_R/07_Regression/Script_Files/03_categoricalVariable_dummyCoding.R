library(car)
library(QuantPsyc)
library(boot)
library(dplyr) # data mainpulation
library(cowplot)
library(DAAG)

df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/07_Regression/Data_Files/GlastonburyFestivalRegression.dat', header=TRUE)
df<- na.omit(df)
head(df)

is.factor(df$music)
df$music<- as.factor(df$music)

is.factor(df$music)

contrasts(df$music)<- contr.treatment(4,base=4)

attr(df$music, "contrasts")

# setting contrasts manually, preferable

crustyVSnma<- c(1,0,0,0)
indieVSnma<-  c(0,1,0,0)
metalVSnma<-  c(0,0,1,0)

contrasts(df$music)<- cbind(crustyVSnma, indieVSnma, metalVSnma)

attr(df$music, 'contrasts')

# Doing Regression
fest<- lm(change~music, data=df)
summary(fest)

# what does these coeffiecients represent
# this actually represents the difference in the change in hygiene scores if a person has no musical affiliation, 
# compared to someone who is a crusty, indie, metal, Rescpectively


# see https://stackoverflow.com/questions/3505701/grouping-functions-tapply-by-aggregate-and-the-apply-family
# note: tapply - For when you want to apply a function to subsets of a vector and the subsets
# are defined by some other vector, usually a factor.
print(round(tapply(df$change, df$music, mean, na.rm=TRUE), 3))

cat("AIC_model: ", AIC(fest),"\nBIC_model: ",  BIC(fest))

anova(fest)


# outliers and influential cases

df$residuals <-resid(fest)
df$standarized.residuals <- rstandard(fest)
df$studentized.residuals <- rstudent(fest)
df$cooks <- cooks.distance(fest)
df$dfbeta <- dfbeta(fest)
df$dffit <- dffits(fest)
df$leverage <- hatvalues(fest)
df$covratio <- covratio(fest)
df$fitted.values<- fitted.values(fest)


large_resid <- dplyr::filter(df, standarized.residuals>2 | standarized.residuals< -2)
large_resid


# now lets see cooks distance , leverage , covariance ratio for 'these' cases
k = 3 #number of predictors
n = 123 #number of objervations

average_leverage = (k+1)/n
average_leverage


cvr_low<- 1-3*average_leverage
cvr_high<- 1+3*average_leverage

large_resid$cov_ideal_low <- cvr_low
large_resid$cov_ideal_high <- cvr_high

large_resid
# no serious influencers or outliers


# Assumptions Check
car::durbinWatsonTest(fest)  # assumption of independent errors

# here car::vif(fest) will not be used 
# Not with vif() in the car package, which wants to compute generalized variance inflation factors (GVIFs) 
# for multi-df terms in the model. Single-df VIFs are pretty simple, so you could just write your own function. 
# Alternatively, there are other packages on CRAN, such as DAAG, that compute VIFs, so you might try one of these. 
DAAG::vif(fest)     # assumptions of multicollinearity

# https://r.789695.n4.nabble.com/Variance-Inflation-Factor-VIC-with-a-matrix-td4643734.html


df$fitted.values<- fitted.values(fest)
df$std_fitted.values<- (fitted.values(fest)-mean(fitted.values(fest)))/sd(fitted.values(fest))
resid_plot<- ggplot(df, aes(standarized.residuals,std_fitted.values))
resid_plot<- resid_plot+geom_point()+geom_smooth(formula='y~x',method = "lm",alpha=0.1)



resid_qq<- ggplot(df, aes(sample=studentized.residuals))
resid_qq<- resid_qq+stat_qq()+ggtitle('QQ-Plot')
resid_qq


histresid<- ggplot(df, aes(studentized.residuals))
histresid<- histresid+geom_histogram(aes(y=..density..),colour='black', fill='white')+
  ggtitle('Histogram')+
  stat_function(fun = dnorm, args = list(mean=0, sd=sd(df$studentized.residuals, na.rm = TRUE)), colour='red')


plot_grid(resid_plot, resid_qq, histresid,ncol=2, nrow=2 )

# this assumption was also met


