#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pingouin as pg
import scipy.stats as st
import statsmodels.api as sm
import statsmodels.formula.api as smf
get_ipython().run_line_magic('matplotlib', 'inline')


# In[2]:


df = pd.read_csv('../Data_Files/Bushtucker.dat',sep='\t')
df


# ### Converting data from wide to Long format

# In[3]:


df_long = pd.melt(df, id_vars=['participant'], value_vars=['stick_insect','kangaroo_testicle','fish_eye','witchetty_grub'])
df_long.sort_values(['participant','variable'],inplace=True)
df_long.reset_index(drop=True, inplace=True)
df_long.head(8)


# ### Analyzing the Data

# In[4]:


_ = sns.barplot(x='variable', y='value',data=df_long)


# In[5]:


_ = sns.boxplot(x='variable', y='value',data=df_long)


# In[6]:


df_long.groupby('variable').describe()['value']


# ## Repeating measure anova using python

# In[7]:


import statsmodels.stats.anova as sp


# In[10]:


aovrm = sp.AnovaRM(df_long, 'value', 'participant', within=['variable']).fit()
aovrm.summary()


# ####  Calculation of between-subject effects and corrections for violation of sphericity are not yet implemented when you use statsmodels AnovaRM 

# In[18]:


pg.rm_anova(df_long,within=['variable'],subject='participant',dv='value',effsize='n2')


# #### The upper implementation of repeated measure anova in python using pingouin module gives the corrections for violation of sphericity . 
# 
# ##### see https://pingouin-stats.org/generated/pingouin.rm_anova.html

# ### As per  Greenhouse–Geisser correction ,  the F-ratio is non-significant(although mind that this correction is somewhat conservative) .

# ## Using multilevel approach, you can forget about sphericity

# In[53]:


mlm_mod_baseline = smf.mixedlm(
    formula = 'value ~ 1', 
    groups = 'participant', 
    data=df_long
)

# Run the fit
mlm_result_baseline = mlm_mod_baseline.fit(reml=False)

# Print out the summary of the fit
mlm_result_baseline.summary()


# In[54]:


mlm_mod = smf.mixedlm(
    formula = 'value ~ C(variable)', 
    groups ='participant', 
    data=df_long
)
# Run the fit
mlm_result = mlm_mod.fit(reml = False)   # we are using method maximum likelihood instead of restricted maximum  likelihood
# Print out the summary of the fit
mlm_result.summary()


# In[65]:


chiSquare_ratio = (-2*mlm_result_baseline.llf-(-2*mlm_result.llf))
chiSquare_ratio


# In[66]:


print('aic: ',mlm_result.aic,'\t','bic: ',mlm_result.bic,'\t','log likelihood: ',mlm_result.llf)


# In[67]:


print('aic: ',mlm_result_baseline.aic, '\t','bic: ',mlm_result_baseline.bic,'\t','log likelihood: ',mlm_result_baseline.llf)


# In[68]:


chi_df = 3


# In[70]:


chi_square_pvalue = 1 - st.chi2.cdf(12.692,chi_df)
print('chisquare_Stats: ',chiSquare_ratio,'\t', 'p_value',chi_square_pvalue)


# #### # from p-value and L.Ratio, we can see that after including Animal in our independent variable , there is improvement in model correctness.

# ## Post-Hoc tests

# In[71]:


from statsmodels.sandbox.stats.multicomp import MultiComparison


# In[72]:


multicomp = MultiComparison(df_long['value'], df_long['variable'])   # testfunc


# In[73]:


# Bonferroni
com = multicomp.allpairtest(st.ttest_rel, method='bonf')
print(com[0])


# In[74]:


# Tukey HSD
co = multicomp.tukeyhsd(alpha=0.05)
co.summary()


# ## Effect Size

# ### see generalized eta-squared which is the output of pingouin rm_anova() under column labelled n2 

# ## calculating various sum of square

# In[75]:


df_long['value'].describe()


# In[76]:


SSt = df_long['value'].std()**2 * (32-1)
SSt


# In[77]:


SSt_df = df_long['value'].count()-1
SSt_df


# In[79]:


std = list(df_long.groupby('participant').describe()['value']['std'])
std


# In[80]:


SSw = 0
for i in range(8):
  SSw += std[i]**2 * (4-1)

SSw


# In[81]:


SSw_df = (4-1)*8
SSw_df


# In[82]:


mean = list(df_long.groupby('variable').describe()['value']['mean'])
mean


# In[84]:


SSm = 0
for i in range(4):
  SSm += (8)*(df_long['value'].mean()-mean[i])**2

SSm


# In[87]:


SSm_df = 4-1     # k-1
SSm_df


# In[88]:


SSr = SSw-SSm
SSr


# In[89]:


SSr_df = SSw_df-SSm_df
SSr_df


# In[90]:


SSb = SSt-SSw
SSb


# In[91]:


MSm = SSm/SSm_df
MSm


# In[92]:


MSr = SSr/SSr_df
MSr


# In[93]:


F = MSm/MSr
F


# In[ ]:




