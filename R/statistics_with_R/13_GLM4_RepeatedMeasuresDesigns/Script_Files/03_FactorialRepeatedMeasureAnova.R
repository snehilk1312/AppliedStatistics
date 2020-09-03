library(ez)
library(nlme)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(reshape)
library(dplyr)

df<- read.delim("/home/atrides/Desktop/R/statistics_with_R/13_GLM4_RepeatedMeasuresDesigns/Data_Files/Attitude.dat", header=TRUE)

head(df)

df_long<- melt(df, id.vars = "participant", measure.vars = c('beerpos','beerneg','beerneut','winepos',
                                                             'wineneg','wineneut','waterpos','waterneg','waterneu'), 
               variable_name = "drinkWperception")

df_long$drinkWperception<- factor(df_long$drinkWperception)
df_long$participant<- factor(df_long$participant)

df_long$drink<- gl(3, 60, 180, labels=c("beer", "wine", "water"))
df_long$atd<- gl(3, 20, 180, labels=c("pos", "neg", "neu"))
# Bar Chart
bar<- ggplot(df_long, aes(drinkWperception, value, fill=drink))+
  stat_summary(fun=mean, geom = "bar", colour="black")+
  stat_summary(fun.data = mean_cl_normal, geom="pointrange")+
  labs(y="Rating")+
  ggtitle("Drink Perception ")
bar

# box plot
box<- ggplot(df_long, aes(drinkWperception, value, fill=drink))+
  geom_boxplot()+
  labs(y="Rating" )+
  ggtitle("Drink Perception ")
box

by(df_long$value,df_long$drinkWperception, stat.desc)


# Setting Contrasts

AlcoholvsWater<-c(1, 1, -2)
BeervsWine<-c(-1, 1, 0)
contrasts(df_long$drink)<-cbind(AlcoholvsWater, BeervsWine)



NegativevsOther<-c(1, -2, 1)
PositivevsNeutral<-c(-1, 0, 1)
contrasts(df_long$atd)<-cbind(NegativevsOther, PositivevsNeutral)


attitudeModel<-ezANOVA(data = df_long, dv = .(value), wid = .(participant),
                       within = .(atd, drink), type = 3, detailed = TRUE)
attitudeModel


by(df_long$value, df_long$drink, stat.desc)
bar1<- ggplot(df_long, aes(drink, value))+
  stat_summary(fun=mean, geom = "bar", colour="black", fill="white")+
  stat_summary(fun.data = mean_cl_normal, geom="pointrange")+
  labs(y="Rating")+
  ggtitle("Drink")
bar1


by(df_long$value, df_long$atd, stat.desc)
bar2<- ggplot(df_long, aes(atd, value))+
  stat_summary(fun=mean, geom = "bar", colour="black", fill="white")+
  stat_summary(fun.data = mean_cl_normal, geom="pointrange")+
  labs(y="Rating")+
  ggtitle("Attitude")
bar2


# see the effect of interaction effect
inter_plot<- ggplot()
q1 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df_long, atd=="pos"), aes(drink,value, group="beer", colour=atd))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df_long,atd=="pos"), aes(drink,value), colour="blue",  width=0.2)
q2 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df_long,  atd=="neg"), aes(drink,value, group="wine", colour=atd))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df_long,atd=="neg"), aes(drink,value), colour="red",  width=0.2)
q3 <- ggplot() +
  stat_summary(fun=mean, geom="line", data=filter(df_long,  atd=="neu"), aes(drink,value, group="water", colour=atd))+
  stat_summary(fun.data =mean_cl_normal, geom="errorbar", data=filter(df_long,atd=="neu"), aes(drink,value), colour="green",  width=0.2)

inter_plot<- inter_plot +q1$layers[[1]]+q1$layers[[2]]+q2$layers[[1]]+q2$layers[[2]]+q3$layers[[1]]+q3$layers[[2]]
inter_plot
# interaction plot shows there is a significant interaction present 

postHoc1<- pairwise.t.test(df_long$value, df_long$drinkWperception, paired = TRUE, p.adjust.method = "bonferroni")
postHoc1


#  Factorial repeated-measures designs as a GLM

baseline<- lme(value~1, random = ~1|participant/drink/atd , data=df_long, method="ML")
drinkmodel<- update(baseline, .~.+drink+atd+drink:atd)

anova(baseline, drinkmodel)

summary(drinkmodel)


#   Robust factorial repeated-measures ANOVA 
#   no function for now


# Effect Size
# Getting omega squared was tough enough for one way repeated measure anova
# we will use generalized eta-squared(ges) from ezAnova model

cat("ges for main effect of attitude: ", 0.5753191)
cat("ges for main effect of drink: ", 0.1158687)
cat("ges for interaction effect: ", 0.1411741)


# effect size for planned contrasts
rcontrast<- function(t, dof){
  r<- sqrt((t^2)/((t^2)+dof))
  cat("r : ", r)
}

rcontrast(3.18, 38) # alcohol vs. water
rcontrast(-1.47, 38) # beer vs. wine
rcontrast(17.26, 114) # negative vs. other
rcontrast(-9.81, 114) # positive vs. neutral
rcontrast(0.69, 114) # alcohol vs. water with negative vs. other 
rcontrast(6.77, 114) # beer vs. wine with negative vs. other imagery
rcontrast(0.93, 114) # alcohol vs. water with positive vs. neutral 
rcontrast(-0.80, 114) # beer vs. wine with positive vs. neutral 

