#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.formula.api as smf
import statsmodels.api as sm
get_ipython().run_line_magic('matplotlib', 'inline')


# In[3]:


df = pd.read_csv('../Data_Files/CloakofInvisibility.dat', sep='\t')
df.head()


# In[4]:


plt.figure(figsize=(10,6))
ax1 = plt.subplot(121)
_ = sns.boxplot(x='cloak', y='mischief1', data=df)
ax2 = plt.subplot(122, sharey=ax1)
_ = sns.boxplot(x='cloak', y='mischief2', data=df)


# In[5]:


robust_anova= smf.ols('mischief2~mischief1+C(cloak)', data=df).fit()
table_robust = sm.stats.anova_lm(robust_anova, typ=2,robust='hc3')
table_robust


# In[ ]:




