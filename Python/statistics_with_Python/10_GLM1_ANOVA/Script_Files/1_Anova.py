#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as st
import statsmodels.api as sm
import pingouin as pg
get_ipython().run_line_magic('matplotlib', 'inline')


# In[2]:


df = pd.read_csv("/home/atrides/Desktop/R/statistics_with_Python/10_GLM1_ANOVA/Data_Files/Viagra.dat", sep='\t')
df.head()


# In[3]:


# lineplot
_ = sns.lineplot(x='dose', y='libido', data = df, err_style='bars')


# In[4]:


# describing statistical properties
df.groupby('dose')['libido'].describe()


# In[5]:


# ideal value for normal distribution , skewness = 0 , excess of kurtosis(here, same as kurtosis) = 0
for i in sorted(df['dose'].unique()):
    print(f'For group with dose = {i} : ')
    print(st.describe(df[df['dose']==i].libido), '\n')


# In[6]:


df_dose1 = df[df['dose']==1]
df_dose2 = df[df['dose']==2]
df_dose3 = df[df['dose']==3]


# In[7]:


# Levene's Test for Homogeneity of Variance (center = median(by default))
stat, p = st.levene(df_dose1['libido'], df_dose2['libido'], df_dose3['libido'])
print(stat, p)


# #### # since the above p value is not significant , i.e, p>.05, we can say that homogeneity of variance is maintained. just for the sake of visualization I am going to draw the boxplot .

# In[8]:


_ = sns.boxplot(x='dose', y='libido', data=df)


# In[9]:


from statsmodels.formula.api import ols
m01 = ols('libido~C(dose)', data=df).fit()
m01.summary()


# In[10]:


anova_table = sm.stats.anova_lm(m01)
anova_table


# #### # making some plots for assumption checking

# In[11]:


prediction = pd.DataFrame(m01.fittedvalues)
prediction.columns = ['predicted'] 
prediction['standarized_prediction']  = (prediction['predicted']-prediction['predicted'].mean())/prediction['predicted'].std()
prediction.head()


# In[12]:


import statsmodels.stats.outliers_influence as sms
summary_frame = sms.OLSInfluence(m01).summary_frame()
summary_frame = pd.merge(summary_frame, prediction, how = 'inner', left_index = True, right_index = True)
summary_frame .head()


# In[13]:


_ = sns.scatterplot(y = 'standard_resid', x='standarized_prediction', data = summary_frame)
_ = plt.axhline(y=0)


# #### # This graph can be used for testing homogeneity of variance. We encountered this kind of plot previously; essentially, if it has a funnel shape then we’re in trouble. The plot we have shows points that are equally spread for the three groups, which implies that variances are similar across groups (which was also the conclusion reached by Levene’s test).

# In[14]:


_ = pg.qqplot(summary_frame['standard_resid'], confidence=False)


# #### # The second plot is a Q-Q plot , which tells us something about the normality of residuals in the model. We want our residuals to be normally distributed, which means that the dots on the graph should cling  to the diagonal line. Ours look like they have had a bit of an argument with the diagonal line, which suggests that we may not be able to assume normality of errors and should perhaps use a robust version of ANOVA instead.

# In[15]:


# Doing Welch anova in the case if homogeniety of variance  is violated(our data here dont need this test)
aov = pg.welch_anova(dv='libido', between='dose', data=df)
aov


# ## Robust ANOVA (for independent samples)

# In[16]:


st.kruskal(df_dose1['libido'], df_dose2['libido'], df_dose3['libido'])


# # Planned Comparison

# #### https://www.statsmodels.org/devel/examples/notebooks/generated/contrasts.html#examples-notebooks-generated-contrasts--page-root 

# In[17]:


contrast1 = [-2,1,1]
contrast2 = [0,-1,1]


# In[18]:


contrast = np.vstack((contrast1, contrast2))
contrast = contrast.T
print(contrast)


# In[19]:


m02 = ols("libido ~ C(dose, contrast)", data=df).fit()
m02.summary()


# # Trend Analysis

# In[20]:


from patsy.contrasts import Poly
levels = df.dose.unique().tolist()
contrast = Poly().code_without_intercept(levels)
print(contrast.matrix)


# In[21]:


m03 = ols("libido ~ C(dose, Poly)", data=df).fit()
m03.summary()


# # Post-hoc tests

# In[22]:


from statsmodels.sandbox.stats.multicomp import MultiComparison


# In[23]:


multicomp = MultiComparison(df['libido'], df['dose'])   # testfunc


# #### # https://pythonhealthcare.org/2018/04/13/55-statistics-multi-comparison-with-tukeys-test-and-the-holm-bonferroni-method/
# #### # https://www.statsmodels.org/stable/generated/statsmodels.sandbox.stats.multicomp.MultiComparison.html

# In[24]:


# Bonferroni
com = multicomp.allpairtest(st.ttest_ind, method='bonf')
print(com[0])


# In[25]:


# Holm
comp = multicomp.allpairtest(st.ttest_ind, method='Holm')
print (comp[0])


# In[26]:


# Tukey HSD
co = multicomp.tukeyhsd(alpha=0.05)
co.summary()


# ### another library for post-hoc tests
# #### # https://scikit-posthocs.readthedocs.io/en/latest/posthocs_api/

# In[27]:


import scikit_posthocs as sk_ph


# In[28]:


sk_ph.posthoc_tukey_hsd(df['libido'], df['dose'])


# In[29]:


sk_ph.posthoc_tukey(df, val_col = 'libido', group_col='dose')


# ## Robust Post-hoc test

# In[30]:


sk_ph.posthoc_wilcoxon(df, val_col = 'libido', group_col='dose')


# #### # from above table it seems that groups (1,3) and (2,3) are significant.
# 

# In[31]:


anova_table


# In[32]:


SSt = 43.73
SSm = 20.133
SSr = 23.6


# In[33]:


R_squared = SSm/SSt    # here R_squared is called eta_squared
R_squared


# In[34]:


effect_size = np.sqrt(R_squared)   #R
effect_size


# #### # a more complex measure is omega-squared , which adjust for the fact that we are doing finding effect size for population and not just our sample

# In[35]:


MSr = 1.967
df_m = 2
ω2 = (SSm-df_m*MSr)/(SSt+MSr)
ω2


# In[36]:


ω = np.sqrt(ω2)       # ω, more accurate measure of R, treat ω as unbiased R
ω


# In[37]:


# effect sizes for different pair of groups
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


# In[38]:


mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5)


# In[39]:


mes(2.2, 5, 1.3038405, 1.5811388, 5, 5)


# In[40]:


mes(3.2, 5, 1.3038405, 1.5811388, 5, 5)


# #### # The standard deviation of the effect size is of critical importance, since it indicates how much uncertainty is included in the measurement. A standard deviation that is too large will make the measurement nearly meaningless.

# # Effect sizes for the orthogonal contrasts

# In[41]:


def r_contrast(t,df):
    return np.sqrt(t**2/(t**2+df))


# In[42]:


dof = 15-2-1   # N-p-1 for normal regression , p is the number of predictors (in this case 2, the two con-trast variables
print(r_contrast(2.474, dof))


# In[43]:


dof = 15-2-1   # N-p-1 for normal regression , p is the number of predictors (in this case 2, the two con-trast variables
print(r_contrast(2.029, dof))


# #### # Both effects are fairly large.

# In[ ]:




