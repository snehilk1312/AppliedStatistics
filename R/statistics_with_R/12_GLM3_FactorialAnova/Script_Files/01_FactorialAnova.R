library(car)
library(compute.es)
library(multcomp)
library(WRS)
library(reshape)
library(pastecs)
library(ggplot2)
library(dplyr)
library(cowplot)


df<- read.csv('/home/atrides/Desktop/R/statistics_with_R/12_GLM3_FactorialAnova/Data_Files/goggles.csv')

head(df)

df$gender<- factor(df$gender)
df$alcohol<- factor(df$alcohol, levels=c("None", "2 Pints", "4 Pints"))

# also, we can plot this
inter_plot<- ggplot()
inter_plot
q1 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df, gender=="Female"), aes(alcohol,attractiveness, group="Female", colour=gender))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df, gender=="Female"), aes(alcohol,attractiveness), colour="red", width=0.2)
q2 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df, gender=="Male"), aes(alcohol,attractiveness, group="Male",colour=gender))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df, gender=="Male"), aes(alcohol,attractiveness),colour="#20B2AA", width=0.2)

inter_plot<- inter_plot +q1$layers[[1]]+q1$layers[[2]]+q2$layers[[1]]+q2$layers[[2]]
inter_plot


# box plots
female_box<- ggplot(filter(df, gender=="Female"), aes(alcohol, attractiveness))+
  geom_boxplot()+
  ggtitle('Female')
male_box<- ggplot(filter(df, gender=="Male"), aes(alcohol, attractiveness))+
  geom_boxplot()+
  ggtitle('Male')

plot_grid(female_box, male_box, nrow=1, ncol=2)


# Some descriptive statistics
by(df$attractiveness, df$gender, stat.desc)
by(df$attractiveness, df$alcohol, stat.desc)
# descriptive statistics using interaction
by(df$attractiveness, list( df$alcohol, df$gender), stat.desc)


# levene test on interaction of variables
leveneTest(df$attractiveness, interaction(df$gender, df$alcohol), center=median)

m00<- lm(attractiveness~gender*alcohol, data=df)
summary(m00)

# Planned Contrast
contrasts(df$alcohol)<-cbind(c(-2, 1, 1), c(0, -1, 1))
contrasts(df$gender)<-c(-1, 1)

m01<- aov(attractiveness~gender*alcohol, data=df)
Anova(m01, type="III")

summary.lm(m01)

# main effect of gender
gender_box<- ggplot(df, aes(gender, attractiveness))+
  geom_boxplot()+
  ggtitle("Main effect of Gender")
gender_box

# main effect of alcohol
alcohol_box<- ggplot(df, aes(alcohol, attractiveness))+
  geom_boxplot()+
  ggtitle("Main effect of Alcohol")
alcohol_box



# Effect analysis in R
df$simple<- gl(6,8)
df$simple<- factor(df$simple, levels = c(1:6) , labels = c("F_None", "F_2", "F_4", "M_None", "M_2", "M_4"))

alcEffect1<-c(-2, 1, 1, -2, 1, 1)
alcEffect2<-c(0, -1, 1, 0, -1, 1)
gender_none<-c(-1, 0, 0, 1, 0, 0)
gender_twoPint<-c(0, -1, 0, 0, 1, 0)
gender_fourPint<-c(0, 0, -1, 0, 0, 1)

simpleEff<-cbind(alcEffect1, alcEffect2, gender_none, gender_twoPint, gender_fourPint)

contrasts(df$simple)<- simpleEff

# Creating an anova model for effect analysis
model_effectAnalysis<- aov(attractiveness~simple, data=df)
summary.lm(model_effectAnalysis)



# Post-Hoc tests

# using pairwise.t.test()
pairwise.t.test(df$attractiveness, df$simple, p.adjust.method = "bonferroni")


# some plots
plot(m00)

# one more interaction graph
bar<-ggplot(df, aes(gender,attractiveness,fill=alcohol))
bar<- bar+stat_summary(fun=mean, geom='bar',position ='dodge')+ 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)

bar_<-ggplot(df, aes(alcohol,attractiveness,fill=gender))
bar_<- bar_+stat_summary(fun=mean, geom='bar',position ='dodge')+ 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)
plot_grid(bar, bar_, nrow=1, ncol=2)

# Robust Factorial Anova

df<- df[, cbind('gender', 'alcohol', 'attractiveness')]

df$row<- rep(1:8, 6)

Melted<-melt(df, id = c("row", "gender", "alcohol"), measured = c("attractiveness"))
head(Melted)

df_wide<- cast(Melted, row~gender+alcohol)
df_wide$row<- NULL

# Conducting Robust anova

# t2way(levels of factor A, levels of factor B, data, tr = .2, alpha = .05)
t2way(2,3,df_wide)


# pbad2way(levels of factor A, levels of factor B, data, est = mom, nboot = 2000)
pbad2way(2,3, df_wide)


# Robust Post-hoc tests
mcp2atm(2, 3,df_wide)


mcp2a(2,3,df_wide)


# Effect Size
omega_factorial<-function(n, a, b, SSa, SSb, SSab, SSr){    
  MSa<-SSa/(a-1)    
  MSb<-SSb/(b-1)    
  MSab<-SSab/((a-1)*(b-1))    
  MSr<-SSr/(a*b*(n-1))    
  varA<-((a-1)*(MSa-MSr))/(n*a*b)    
  varB<-((b-1)*(MSb-MSr))/(n*a*b)    
  varAB<-((a-1)*(b-1)*(MSab-MSr))/(n*a*b)    
  varTotal<-varA + varB + varAB + MSr    
  print(paste("Omega-Squared A: ", varA/varTotal))    
  print(paste("Omega-Squared B: ", varB/varTotal))    
  print(paste("Omega-Squared AB: ", varAB/varTotal)) 
}

omega_factorial(8, 2, 3, 169, 3332, 1978, 3488)


# Effect sizes of gender at different level of alcohol

# mes(mean_males, mean_females, sd_males, sd_females, n_males, n_females)
mes(66.875, 60.625, 10.3293963, 4.95515604, 8, 8)
mes(66.875, 62.5, 12.5178444, 6.5465367, 8, 8)
mes(35.625, 57.5, 10.8356225, 7.0710678, 8, 8)
