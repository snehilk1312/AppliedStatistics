library(ggplot2) # for graphs
library(WRS) # for robust stats
library(car) # for levene tese
library(multcomp) # for posthoc test
library(compute.es) # for effect sizes
library(pastecs) # for descriptive stats
library(dplyr)
library(reshape2)

df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/10_GLM1_ANOVA/Data_Files/Dummy.dat', header=TRUE)

m01<- lm(libido~dummy1+dummy2, data=df)
summary(m01)


df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/10_GLM1_ANOVA/Data_Files/Viagra.dat', header=TRUE)

df

lineplot<- ggplot(df, aes(dose, libido))
lineplot<- lineplot+
  stat_summary(fun=mean,geom='point',colour='blue')+
  stat_summary(fun=mean, geom='line',colour='blue')+
  stat_summary(fun.data = mean_cl_boot,geom='errorbar', width=0.2)
lineplot

by(df$libido, df$dose, stat.desc)

is.factor(df$dose)
df$dose<- as.factor(df$dose)

# doing levene test
leveneTest(df$libido, df$dose)

# since the above p value is not significant , i.e, p>.05, we can say that homogeneity of variance is maintained.
# also checking manually

sd(filter(df,dose==1 )$libido)
sd(filter(df,dose==2 )$libido)
sd(filter(df,dose==3 )$libido)


# Boxplot
box<- ggplot(df, aes(dose, libido))
box<- box+geom_boxplot()
box

m01<- lm(libido~dose, data=df)
summary(m01)

anova_table<- aov(libido~dose, data=df)
summary(anova_table)

# shows qqplot and some other plots of standarized residuals. Good for checking assumption
plot(anova_table)
# The plot we have shows points that are equally spread for the three groups, which implies that 
# variances are similar across groups (which was also the conclusion reached by Levene’s test

# from qqplot it can be seen that the assumption of normality of errors has been violated,
# so we may have to do robust anova later.


# Doing Welch anova in the case if homogeniety of variance  is violated(our data here dont need this test)
oneway.test(libido~dose, data=df)



# Robust Anova

# first we need our data in wide format
df$id<- c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
df<- df[, cbind('dose','libido','id')]
df<- reshape(df, idvar = "id", timevar = "dose", direction = "wide")

names(df)<- c('id','placebo', 'low', 'high')
df<- df[, cbind('placebo', 'low', 'high')]
write.table(df, file='df_robustAnova.csv',sep=',' )

# now since the data is in wide format , we can use WRS package to do robust tests

# 1st robust test
t1way(df,tr=0.1)

# 2nd robust test
med1way(df)

# 3rd robust test
t1waybt(df)


# all three robust test show there is no significant effect of dose on libido



# Planned Comparison
summary.lm(anova_table)

df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/10_GLM1_ANOVA/Data_Files/Viagra.dat', header=TRUE)
df$dose<- as.factor(df$dose)


contrast1<- c(-2, 1, 1)
contrast2<- c(0, -1, 1)
contrasts(df$dose)<- cbind(contrast1, contrast2)

aov2<- aov(libido~dose, data=df)
summary.lm(aov2)


# Trend Analysis
contrasts(df$dose)<- contr.poly(3)
aov_trend<- aov(libido~dose, data=df)
summary.lm(aov_trend)



# Post-hoc tests
pairwise.t.test(df$libido , df$dose, paired=FALSE , p.adjust.method = "bonferroni")
pairwise.t.test(df$libido , df$dose, paired=FALSE , p.adjust.method = "holm")
pairwise.t.test(df$libido , df$dose, paired=FALSE , p.adjust.method = "BH")

# posthoc test using multcomp package
# doing tukey test
posthoc_1<- glht(anova_table, linfct=mcp(dose="Tukey"))
print(summary(posthoc_1))

print(confint(posthoc_1))

# doing dunnett's test
posthoc_2<- glht(anova_table, linfct=mcp(dose='Dunnett'), base=1)
print(summary(posthoc_2))
print(confint(posthoc_2))



# Robust Posthoc test
data<- read.csv('/home/atrides/Desktop/R/statistics_with_R/10_GLM1_ANOVA/Data_Files/df_robustAnova.csv', header=TRUE)
data<- data[, cbind('placebo', 'low', 'high')]
data


# when we use lincon, confidence interval are adjusted to family wise error rate
# but the p-values are not adjusted to same
lincon(data, tr=0.2)
mcppb20(data)


# Calculating the effect size
summary(anova_table)
# R^2<- SSm/SSt
SSm<- 20.13
SSt<- 20.13+23.60  
R2<- SSm/SSt
R2    # called  eta squared, η2, in context of anova 

R<- sqrt(R2)
R

# a more complex measure is omega-squared , which adjust for the fact that we are doing 
# finding effect size for population and not just our sample

MSr<- 1.967
df_m<- 2
omega2<- (SSm-df_m*MSr)/(SSt+MSr)
omega2

omega<- sqrt(omega2)
omega
# treat omega as unbiased r


# effect sizes for different pair of groups
eff1<- mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5)
cat("cohen's d: ", eff1$d, "hedges g: ",eff1$g, "correlation coeffecient: ",eff1$r)

eff2<- mes(2.2, 5, 1.30384, 1.5811388, 5, 5)
cat("cohen's d: ", eff2$d, "hedges g: ",eff2$g, "correlation coeffecient: ",eff2$r)

eff3<- mes(3.2, 5, 1.30384, 1.5811388, 5, 5)
cat("cohen's d: ", eff3$d, "hedges g: ",eff3$g, "correlation coeffecient: ",eff3$r)


# Checking effects of contrasts
rcontrast<- function(t, dof){
  r<- sqrt(t^2/(t^2+dof))
  cat("r: ", r)

}

# for 1st contrast
rcontrast(2.474, 12)

# for 2nd contrast
rcontrast(2.029, 12)


# both effects were quite large

