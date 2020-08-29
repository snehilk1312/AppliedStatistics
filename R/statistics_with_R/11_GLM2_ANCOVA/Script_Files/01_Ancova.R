library(car)
library(pastecs)
library(WRS)
library(multcomp)
library(compute.es)
library(effects)
library(ggplot2)
library(dplyr)

df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/11_GLM2_ANCOVA/Data_Files/ViagraCovariate.dat', header=TRUE)
head(df)

# summary basic means 
by(df$libido ,df$dose, mean)
by(df$partnerLibido ,df$dose, mean)

is.factor(df$dose)
df$dose<- factor(df$dose, levels=c(1,2,3))

# boxplots
box<- ggplot(df, aes(dose, libido))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 10))
box

# checking homogeneity of variances
leveneTest(df$libido, df$dose)
# also , could use Hartley F max test in addition , done in python notebook :)


# Checking assumption 1, independence of covariate and experimental manipulator
aov1<- aov(partnerLibido~dose, data=df)
summary(aov1)

# from summary we can see the relationship between groups and covariate is non-significant
# hence, our assumption is followed

m01<- aov(libido~dose+partnerLibido, data=df)
Anova(m01, type='III')  # defaults to type="II"


# Planned Contrasts
con1<- c(-2,1, 1)
con2<- c(0, 1,-1)
contrasts(df$dose)<- cbind(con1, con2)

contrast_model<- aov(libido~ dose+partnerLibido, data=df)
Anova(contrast_model, type="III")
summary.lm(contrast_model)


# adjusting for the effect of covariate
adjustedMeans<- effect("dose", m01)
summary(adjustedMeans)

adjustedMeans$se


# Interpreting the covariate
dotplot<- ggplot(df, aes(partnerLibido, libido))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_continuous(breaks=pretty(df$libido,n=5))

dotplot


# post hoc test in Ancova
# we can use only the glht() function; the pairwise.t.test() function will not test the adjusted means.

postHocs<- glht(m01, linfct=mcp(dose='Tukey'))
summary(postHocs)
confint(postHocs)


# plots in ancova
plot(m01)


# Final Remarks
model_justAnova<- aov(libido~dose, data=df)
summary(model_justAnova)

# so , if we hadn't taken covariate in our calculation , the resulting 
# summary would be incorrect and misleading


# Checking assumption of homegeniety of regression slopes
hoRS<- aov(libido~ dose*partnerLibido, data=df)
summary(hoRS)
# since the interaction term is significant , the assumption is broken

# also, we can plot this
hoRS_plot<- ggplot(df, aes(libido,partnerLibido))+
  geom_point(color='black')
  
hoRS_plot
q1 <- ggplot() +
  geom_smooth(data = filter(df, dose==1), aes(libido,partnerLibido,  color = "blue"), method = "lm")
q2 <- ggplot() +
  geom_smooth(data = filter(df, dose==2), aes(libido,partnerLibido,  color = "orange"), method = "lm")
q3 <- ggplot() +
  geom_smooth(data = filter(df, dose==3), aes(libido,partnerLibido,  color = "green"), method = "lm")

hoRS_plot<- hoRS_plot+q1$layers[[1]]+q2$layers[[1]]+q3$layers[[1]]
hoRS_plot



# Effect Sizes

# partial R2
Anova(m01)

partial_R2_dose<- 25.185/(25.185+79.047)
partial_R2_partner<- 15.076/(15.076+79.047)

partial_R2_dose
partial_R2_dose


# R_Contrast
r_contrast<- function(t, dof){
  cat("r : ",sqrt(t^2/(t^2+dof)))
  
}

summary.lm(contrast_model)

r_contrast(2.785, 26)
r_contrast(-0.541, 26)
r_contrast(2.227, 26)


# an alternative of getting effect size of contrasts,
# is to get all pairwise effect sizes
summary(adjustedMeans)
n<- c(9,8,13)
adjustedMeans$sd<- adjustedMeans$se*sqrt(n)

adjustedMeans$sd

# placebo-low
mes(2.92, 4.71, 1.79, 1.46, 9,8)

# high-low
mes(5.15, 4.71, 2.11, 1.46, 13,8)

# high-placebo
mes(5.15, 2.92, 2.11, 1.79, 13,9)

