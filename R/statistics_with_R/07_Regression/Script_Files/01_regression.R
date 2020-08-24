library(car)
library(QuantPsyc)
library(boot)
library(dplyr) # data mainpulation
library(cowplot)
library(ggplot2)

df1<- read.delim('/home/atrides/Desktop/R/statistics_with_R/07_Regression/Data_Files/Album Sales 1.dat', header=TRUE)


albumsales1<-lm(formula=sales~adverts, data=df1)


# Interpreting a simple regression
summary(albumsales1)


df2<- read.delim('/home/atrides/Desktop/R/statistics_with_R/07_Regression/Data_Files/Album Sales 2.dat', header=TRUE)

head(df2)

albumsales2<-lm(sales~adverts, data=df2)
albumsales3<-lm(sales~adverts+airplay+attract, data=df2) # or use update(albumsales2, .~.+attract+airplay)


summary(albumsales2)
summary(albumsales3)

# model parameters
print(lm.beta(albumsales3))  # standarized b balues
print(confint(albumsales3))


# comparing Models
anova(albumsales2, albumsales3)

cat("AIC_model2: ", AIC(albumsales2),"\nBIC_model2: ",  BIC(albumsales2))
cat("AIC_model3: ", AIC(albumsales3),"\nBIC_model3: ",  BIC(albumsales3))


# outliers and influential cases

df2$residuals <-resid(albumsales3)
df2$standarized.residuals <- rstandard(albumsales3)
df2$studentized.residuals <- rstudent(albumsales3)
df2$cooks <- cooks.distance(albumsales3)
df2$dfbeta <- dfbeta(albumsales3)
df2$dffit <- dffits(albumsales3)
df2$leverage <- hatvalues(albumsales3)
df2$covratio <- covratio(albumsales3)


# saving this data
write.table(df2,'albumSalesWithDiagnosticsData.dat', sep='\t', row.names = FALSE)

large_resid <- dplyr::filter(df2, standarized.residuals>2 | standarized.residuals< -2)
# these cases are to be analyzed coz they have somewhat large residuals
large_resid


# now lets see cooks distance , leverage , covariance ratio for 'these' cases
k = 3 #number of predictors
n = 200 #number of objervations

average_leverage = (k+1)/n
average_leverage

cvr_low<- 1-3*average_leverage
cvr_high<- 1+3*average_leverage

large_resid$cov_ideal_low <- cvr_low
large_resid$cov_ideal_high <- cvr_high

large_resid

# from this large residual model we conclude that
# Most of our 12 potential outliers have CVR values within or just outside the boundaries.
# none of them has a Cook’s distance greater than 1,  so none of the cases is having an undue influence on the model.


# So , Note:

# i) Look at standardized residuals and check that no more than 5% of cases have absolute values above 2, 
#    and that no more than about 1% have absolute values above 2.5. Any case with a value above about 3 could be an outlier.

# ii)Look at the values of Cook’s distance: any value above 1 indicates a case that might be influencing the model.

# iii)Calculate the average leverage (the number of predictors plus 1, divided by the sample size) 
#      and then look for values greater than twice or three times this average value

# iv)Calculate the upper and lower limit of acceptable values for the covariance ratio, CVR.
#        The upper limit is 1 plus three times the average leverage, whereas 
#        the lower limit is 1 minus three times the average leverage. 
#        Cases that have a CVR falling outside these limits may be problematic


# -----------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------


# Testing various assumptions

# i) Assumptions of Independent Errors
car::durbinWatsonTest(albumsales3)  # hence assumption is valid here

# ii) Assumption of no multicollinearity
vif_<- car::vif(albumsales3)
vif_
mean(vif_)
# the assumption of multicollinearity is followed too

# iii) Assumption about the Residuals
df2$fitted.values<- fitted.values(albumsales3)
df2$std_fitted.values<- (fitted.values(albumsales3)-mean(fitted.values(albumsales3)))/sd(fitted.values(albumsales3))
resid_plot<- ggplot(df2, aes(standarized.residuals,std_fitted.values))
resid_plot<- resid_plot+geom_point()+geom_smooth(formula='y~x',method = "lm",alpha=0.1)



resid_qq<- ggplot(df2, aes(sample=studentized.residuals))
resid_qq<- resid_qq+stat_qq()+ggtitle('QQ-Plot')
resid_qq


histresid<- ggplot(df2, aes(studentized.residuals))
histresid<- histresid+geom_histogram(aes(y=..density..),colour='black', fill='white')+
  ggtitle('Histogram')+
  stat_function(fun = dnorm, args = list(mean=0, sd=sd(df2$studentized.residuals, na.rm = TRUE)), colour='red')


plot_grid(resid_plot, resid_qq, histresid,ncol=2, nrow=2 )

# this assumption was also met


