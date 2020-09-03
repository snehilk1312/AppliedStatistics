library(ez)
library(nlme)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(reshape)

df<- read.delim("/home/atrides/Desktop/R/statistics_with_R/13_GLM4_RepeatedMeasuresDesigns/Data_Files/TutorMarks.dat")

head(df)

df_long<- melt(df, id.vars = "Essay", measure.vars = c('tutor1', 'tutor2', 'tutor3','tutor4'), variable_name = "tutor")

df_long

df_long$Essay<- factor(df_long$Essay)
df_long$tutor<- factor(df_long$tutor)

# Bar Chart
bar<- ggplot(df_long, aes(tutor, value))+
  stat_summary(fun=mean, geom = "bar", colour="black", fill="white")+
  stat_summary(fun.data = mean_cl_normal, geom="pointrange")+
  labs(y="Marks")+
  ggtitle("tutors")
bar

by(df_long$value, df_long$tutor, stat.desc)


goodVSbad<- c(1,1,1,-3)
firstVS2_3<- c(2,-1,-1,0)
secondVSthird<- c(0,1,-1,0)

contrasts(df_long$tutor)<- cbind(goodVSbad, firstVS2_3, secondVSthird)


# doing normal anova
m01<- ezANOVA(df_long, dv=.(value), wid = .(Essay),within = .(tutor), detailed=TRUE, type = 3)

m01


# PostHoc test
pairwise.t.test(df_long$value, df_long$tutor, paired = TRUE, p.adjust.method = "bonferroni")

# the conclusion drawn by post hoc tests are somwhat wrong , due to violation of sphericity assumption

cor(df[, cbind('tutor1', 'tutor2', 'tutor3', 'tutor4')])


# Using multilevel model, better approach
baseline<- lme(value~1, data=df_long, random = ~1|Essay/tutor, method = "ML")
tutormodel<- lme(value~1+tutor, data=df_long, random = ~1|Essay/tutor, method = "ML")

anova(baseline, tutormodel)

summary(tutormodel)


postHoc2<- glht(tutormodel, linfct=mcp(tutor="Tukey"))
summary(postHoc2)
confint(postHoc2)


# Effect Sizes
# getting effect size of various contrasts used in our model
rcontrast<- function(t, dof){
  eff<- sqrt(t^2/((t^2)+dof))
  cat("r contrast: ", eff)
}

rcontrast(3.34182, 21)
rcontrast(1.48533,21)
rcontrast(-0.31184,21)



# Robust anova
df<- df[, -c(1)]
rmanova(df)

rmanovab(df, nboot = 2000)

rmmcp(df)


pairdepb(df, nboot = 2000)
