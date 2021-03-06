library(ez)
library(nlme)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(reshape)
library(dplyr)
df<- read.delim("/home/atrides/Desktop/R/statistics_with_R/13_GLM4_RepeatedMeasuresDesigns/Data_Files/BeerGogglesLighting.dat")

df_long<- melt(df, id.vars = 'Participant', measure.vars = c("dim0",'bright0', 'dim2','bright2','dim4', 
                                                             'bright4', 'dim6','bright6' ), 
               variable_name = "light")

df_long$light<- factor(df_long$light)
df_long$Participant<- factor(df_long$Participant)

df_long$l<- gl(2, 26, 208, labels = c('dim', 'bright'))
df_long$a<- gl(4, 52, 208, labels = c(0,2,4,6))

head(df_long)

# Bar Chart
bar<- ggplot(df_long, aes(light, value, fill=l))+
  stat_summary(fun=mean, geom = "bar", colour="black")+
  stat_summary(fun.data = mean_cl_normal, geom="pointrange")+
  labs(y="Rating")
bar

# see the effect of interaction effect
inter_plot<- ggplot()
q1 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df_long, a==0), aes(l,value, group=1, colour=a))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df_long,a==0), aes(l,value),colour='orange',  width=0.2)
q2 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df_long, a==2), aes(l,value, group=2, colour=a))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df_long,a==2), aes(l,value),colour='green',  width=0.2)
q3 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df_long, a==4), aes(l,value, group=3, colour=a))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df_long,a==4), aes(l,value), colour="blue", width=0.2)
q4 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df_long, a==6), aes(l,value, group=4, colour=a))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df_long,a==6), aes(l,value),colour="purple",  width=0.2)

inter_plot<- inter_plot +q1$layers[[1]]+q1$layers[[2]]+q2$layers[[1]]+q2$layers[[2]]+
  q3$layers[[1]]+q3$layers[[2]]+q4$layers[[1]]+q4$layers[[2]]
inter_plot

# interaction seems to be significant

# setting contrasts
NonevsAlcohol<-c(4, -1, -1, -1) 
MaxvsLess<-c(0, -1, -1, 2) 
TwovsFour<-c(0, 1, -1,0) 
DimvsLight<-c(1, -1) 

contrasts(df_long$a)<- cbind(NonevsAlcohol,MaxvsLess,  TwovsFour)
contrasts(df_long$l)<- cbind(DimvsLight)

ezModel<- ezANOVA(df_long, dv=.(value), wid=.(Participant), within = .(a,l), type = 3, detailed = TRUE)
ezModel

# Since l variable has 2 levels only, no prblm of sphericity


# post hoc tests
pairwise.t.test(df_long$value, df_long$light, paired = TRUE, p.adjust.method = 'bonferroni')



# Using multilevel models
baseline<- lme(value~1,random = ~1|Participant/a/l, method="ML", data=df_long)
m02<- update(baseline, .~.+a+l+a:l)
anova(baseline, m02)

summary(m02)


# Effect Sizes
# getting effect size of various contrasts used in our model
rcontrast<- function(t, dof){
  eff<- sqrt(t^2/((t^2)+dof))
  cat("r contrast: ", eff)
}

rcontrast( 10.21387, 75) 
rcontrast(-12.22235, 75) 
rcontrast(9.94787, 75) 
rcontrast(5.27368, 100) 
rcontrast(-4.31284, 100) 
rcontrast(4.78794, 100) 
rcontrast(-5.03915, 100)
