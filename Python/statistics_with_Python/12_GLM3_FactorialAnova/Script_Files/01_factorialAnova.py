#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as st
import statsmodels.formula.api as smf
import statsmodels.api as sm
import pingouin as pg
get_ipython().run_line_magic('matplotlib', 'inline')


# In[3]:


df = pd.read_csv('../Data_Files/goggles.csv')
df.head()


# In[6]:


df['genderX'] = df['gender'].replace({'Male':1, 'Female':2})
df['alcoholX'] = df['alcohol'].replace({'None':1, '2 Pints':2,'4 Pints':3})


# In[10]:


df.groupby(['gender', 'alcohol']).describe()['attractiveness']


# In[12]:


from statsmodels.graphics.factorplots import interaction_plot
fig = interaction_plot(df.alcoholX, df.gender, df.attractiveness,
             colors=['red','blue'], markers=['D','^'], ms=10)


# In[26]:


_ = sns.lineplot(x='alcohol', y='attractiveness', hue='gender', err_style="bars",sort=False,data=df,style='gender',markers=['D','^'])


# In[28]:


plt.figure(figsize=(8,6))
_ = sns.boxplot(x='alcohol', y='attractiveness', hue='gender', data=df)


# In[41]:


# main effect of alcohol
_ = sns.boxplot(x='alcoholX', y='attractiveness', data=df)


# In[30]:


# main effect of gender
_ = sns.boxplot(x='genderX', y='attractiveness', data=df)


# ## levene test on interaction of variables

# In[32]:


unique_list = [i for i in range(1,7)]
unique_list


# In[33]:


df['interaction'] = 0
for i in range(6):
    for j in range(8):
        df.at[8*i+j,'interaction'] = unique_list[i]


# In[34]:


df.head()


# In[35]:


# Levene test on interaction variables
pg.homoscedasticity(df, dv='attractiveness',group='interaction')


# #### # A non-significant result like the one we have here, W(5, 42) = 1.425, p = .235, is indicative of the assumption being met.

# In[36]:


m01 = smf.ols('attractiveness~C(genderX)*C(alcoholX)', data=df).fit()
m01.summary()


# #### # https://www.statsmodels.org/devel/examples/notebooks/generated/contrasts.html#examples-notebooks-generated-contrasts--page-root

# ### Planned Contrast

# In[37]:


con1 = [-2,1,1]
con2 = [0,-1,1]
contrast = np.vstack((con1, con2))
contrast_alc = contrast.T
contrast_alc


# In[38]:


contrast_gen = np.array([[1,-1]])
contrast_gen =contrast_gen.reshape(2,1)
contrast_gen


# In[50]:


contrast_model = smf.ols('attractiveness~C(genderX,contrast_gen)*C(alcoholX, contrast_alc)', data=df).fit()
contrast_model.summary()


# ## Simple Effect Analysis

# In[42]:


from IPython.display import Image
Image('/home/atrides/Downloads/simpleEffectAnalysis.png')


# In[46]:


Image('/home/atrides/Downloads/contrast_table.png')


# In[43]:


contrast1 = [-2, 1, 1, -2, 1, 1]
contrast2 = [0, -1, 1, 0, -1, 1]
contrast3 = [ 1, 0, 0, -1, 0, 0]
contrast4 = [ 0, 1, 0, 0, -1, 0]
contrast5 = [ 0, 0, -1, 0, 0, 1]


# In[47]:


final_contrast = np.vstack((contrast1, contrast2, contrast3, contrast4, contrast5))
final_contrast = final_contrast.T
final_contrast  # according to levels, i.e 0F, 2F, 4F, 0M, 2M, 4M


# In[51]:


effectAnalysis_model = smf.ols('attractiveness~C(interaction,final_contrast)', data=df).fit()
effectAnalysis_model.summary()


# #### # The resulting output contains the parameter estimates for the five contrasts. Looking at the significance values for each simple effect, it appears that there was no significant difference between men and women when they drank no alcohol, p = .177, or when they drank 2 pints, p = .34, but there was a very significant difference, p < .001, when 4 pints were consumed (which, judging from the interaction graph, reflects the fact that the mean for men is considerably lower than for women)

# ## Post-hoc Tests

# In[52]:


from statsmodels.sandbox.stats.multicomp import MultiComparison


# In[55]:


multicomp = MultiComparison(df['attractiveness'], df['interaction'])   # testfunc


# In[56]:


# Bonferroni
com = multicomp.allpairtest(st.ttest_ind, method='bonf')
print(com[0])


# In[58]:


prediction = pd.DataFrame(m01.fittedvalues)
prediction.columns = ['predicted'] 
prediction['standarized_prediction']  = (prediction['predicted']-prediction['predicted'].mean())/prediction['predicted'].std()
prediction.head()


# In[60]:


import statsmodels.stats.outliers_influence as sms


# In[61]:


summary_frame = sms.OLSInfluence(m01).summary_frame()
summary_frame = pd.merge(summary_frame, prediction, how = 'inner', left_index = True, right_index = True)


# ### Some Plots

# In[62]:


_ = sns.scatterplot(y = 'standard_resid', x='standarized_prediction', data = summary_frame)
_ = plt.axhline(y=0)


# In[63]:


_ = pg.qqplot(summary_frame['standard_resid'], confidence=False)


# #### # The plot we have does show funnelling (the spread of scores is wider at some points than at others), which implies that the residuals might be heteroscedastic (a bad thing). The second plot (on the right) is a Q-Q plot , which tells us about the normality of residuals in the model, from plot we can say our assumptions of normally distributed residuals have been true

# In[65]:


# one more interaction graph
plt.figure(figsize=(12,6))
plt.subplot(1,2,1)
_ = sns.barplot(x='alcohol', y='attractiveness', data=df, hue='gender')
plt.subplot(1,2,2)
_ = sns.barplot(x='gender', y='attractiveness', data=df, hue='alcohol')


# ## Robust Factorial Anova

# In[68]:


df1 = df[df['interaction']==1]
df2 = df[df['interaction']==2]
df3 = df[df['interaction']==3]
df4 = df[df['interaction']==4]
df5 = df[df['interaction']==5]
df6 = df[df['interaction']==6]


# In[69]:


st.kruskal(df1['attractiveness'], df2['attractiveness'], df3['attractiveness'],df4['attractiveness'],df5['attractiveness'],df6['attractiveness'])


# In[70]:


sm.stats.anova_lm(m01,typ=3,robust="hc1")


# In[71]:


sm.stats.anova_lm(m01,typ=3,robust="hc3")


# ## Robust post-hocs test

# In[76]:


# using scikit-posthocs
import scikit_posthocs as sp
sp.posthoc_wilcoxon(df, val_col = 'attractiveness', group_col='interaction')


# ## Some basic understanding of sum of squares

# In[79]:


df.head()


# In[80]:


df['attractiveness'].describe()


# In[81]:


x_grand =  df['attractiveness'].mean()
x_grand


# In[82]:


SSt = df['attractiveness'].std()**2 * (47)
SSt


# In[84]:


SSm = 0
for i in range(1,7):
    a = eval(f'df{i}')
    g_mean = a['attractiveness'].mean()
    SSm = SSm+8*(g_mean-x_grand)**2
SSm


# In[85]:


SSr = SSt - SSm
SSr


# In[86]:


df_m = df[df['gender']=='Male']
df_f = df[df['gender']=='Female']


# In[88]:


## SS_gender
SSa = 0
gender_l = ['m', 'f']
for i in range(2):
    a = eval(f'df_{gender_l[i]}')
    g_mean = a['attractiveness'].mean()
    SSa = SSa+24*(g_mean-x_grand)**2
SSa


# In[89]:


alcohol_list = ['no', '2pint', '4pint']
alcohol_no = df[df['alcoholX']==1]
alcohol_2pint = df[df['alcoholX']==2]
alcohol_4pint = df[df['alcoholX']==3]


# In[90]:


## SS_alcohol
SSb = 0
for i in range(3):
    a = eval(f'alcohol_{alcohol_list[i]}')
    g_mean = a['attractiveness'].mean()
    SSb = SSb+16*(g_mean-x_grand)**2
SSb


# In[91]:


SSab = SSm - SSa - SSb
SSab


# In[92]:


SSr = SSt-SSm
SSr


# ### calculating degree of freedom

# In[93]:


n = 48
k = 6


# In[94]:


df_SSt = n-1
df_SSt


# In[95]:


df_SSm = k-1
df_SSm


# In[96]:


df_SSa = 2-1
df_SSa


# In[97]:


df_SSb = 3-1
df_SSb


# In[98]:


df_SSab = df_SSa*df_SSb  # or use df_SSab = df_SSm - df_SSa-df_SSb
df_SSab


# In[99]:


df_SSr = n-k
df_SSr


# ### Calculating mean sum of squares

# In[100]:


MSm = SSm/df_SSm
MSm


# In[101]:


MSa = SSa/df_SSa
MSa


# In[102]:


MSb = SSb/df_SSb
MSb


# In[103]:


MSab = SSab/df_SSab
MSab


# In[106]:


MSr = SSr/df_SSr
MSr


# ### Calculating F-ratios

# In[105]:


F_a = MSa/MSr
F_a


# In[107]:


F_b = MSb/MSr
F_b


# In[108]:


F_ab = MSab/MSr
F_ab


# ## Effect Size

# In[109]:


sm.stats.anova_lm(m01,typ=3)


# In[110]:


# a is the number of levels of the first independent variable
# b is the number of levels of the second independent variable 
# n is the number of people per condition.
def Omega_factorial(n,a,b, MSa, MSb, MSab, MSr):
    varA = ((a-1)*(MSa-MSr))/(n*a*b)
    varB = ((b-1)*(MSb-MSr))/(n*a*b)
    varAB = ((a-1)*(b-1)*(MSab-MSr))/(n*a*b)
    varTotal = varA+varB+varAB+MSr
    print("Omega-Squared A: ", varA/varTotal)
    print("Omega-Squared B: ", varB/varTotal)
    print("Omega-Squared AB: ", varAB/varTotal)


# In[111]:


Omega_factorial(8, 2, 3, MSa, MSb, MSab, MSr)


# In[112]:


def s_within(s1,s2,n1,n2):
    return np.sqrt((((n1-1)*(s1**2))+((n2-1)*(s2**2)))/(n1+n2-2))

def mes(m1, m2, s1, s2, n1, n2):
    s_ = s_within(s1,s2,n1,n2)
    d = (m1-m2)/s_   # cohen's D
    d_var = (n1+n2)/(n1*n2) + (d**2)/(2*(n1+n2))    # variance of d 
    
    df = n1+n2-2
    J = 1-3/(4*df-1)   # correction factor
    
    g = J*d          #   Hedges' g
    
    g_var = (J**2)*d_var     #   variance of g
    
    a = (n1+n2)**2/(n1*n2)    # a corrects for inbalance in n1 & n2
    
    r = d/np.sqrt(d**2+a)  # correlation coeffecient
    
    r_var = (a**2 * d_var)/((d**2 + a)**3)     #   variance of r
    
    
    print(f'Cohen\'s d: {d}, var.d: {d_var}')
    print(f'Hedge\'s g: {g}, var.g: {g_var}')
    print(f'Correlation coeffecient: {r}, var.r: {r_var}')


# #### # Several times it is perhaps more useful to quantify focused differences (i.e., between two things) than overall effect, Compute the differences between means for one independent variable at different levels of the other independent variable.

# ## Effect sizes of gender at different level of alcohol

# In[113]:


# none - male vs female
mes(66.875, 60.625, 10.3293963, 4.95515604, 8, 8)


# In[114]:


# 2 pints - male vs female
mes(66.875, 62.5, 12.5178444, 6.5465367, 8, 8)


# In[115]:


# 4 pints - male vs female
mes(35.625, 57.5, 10.8356225, 7.0710678, 8, 8)


# #### # The difference in attractiveness scores between males and females who drank no alcohol is a medium effect (the means are under a standard deviation different), d = 0.77, r = .36; the difference between males and females who drank 2 pints is a fairly small effect (there is less than half a standard deviation difference between the group means), d = 0.44, r = .21; finally, the difference between males and females who drank 4 pints is a very large effect (the means are more than 2 standard deviation apart), d = −2.39, r = −.77

# In[116]:


Image('/home/atrides/Downloads/report1.png')


# In[117]:


Image('/home/atrides/Downloads/report2.png')


# In[ ]:




