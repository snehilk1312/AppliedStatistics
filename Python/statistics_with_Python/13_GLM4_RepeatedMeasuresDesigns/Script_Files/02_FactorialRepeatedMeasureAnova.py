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


df = pd.read_csv('/home/atrides/Desktop/R/statistics_with_R/13_GLM4_RepeatedMeasuresDesigns/Data_Files/Attitude.dat', sep='\t')
df.head()


# In[5]:


df_long = pd.melt(df, id_vars=['participant'], value_vars=['beerpos','beerneg','beerneut','winepos','wineneg','wineneut','waterpos','waterneg','waterneu'])


# In[6]:


df_long.sort_values(['variable'],inplace=True)
df_long.reset_index(drop=True, inplace=True)


# In[7]:


df_long['drink']=df_long['variable'].replace({'beerneg':'beer','beerneut':'beer','beerpos':'beer',
                                    'winepos':'wine','wineneut':'wine','wineneg':'wine',
                                    'waterpos':'water','waterneu':'water','waterneg':'water'})
df_long['atd'] = df_long['variable'].replace({'beerneg':'neg','beerneut':'neut','beerpos':'pos',
                                    'winepos':'pos','wineneut':'neut','wineneg':'neg',
                                    'waterpos':'pos','waterneu':'neut','waterneg':'neg'})
df_long.head()


# In[8]:


df_long.groupby(['drink','atd']).describe()['value']


# In[9]:


plt.figure(figsize=(10,8))
_ = sns.barplot(x='drink',y='value',data=df_long,hue='atd')


# In[10]:


plt.figure(figsize=(10,8))
_ = sns.boxplot(x='drink',y='value',data=df_long,hue='atd')


# ## Factorial Repeated Measure Anova

# In[13]:


aov = pg.rm_anova(dv='value', within=['drink','atd'],

                  subject='participant', data=df_long, detailed=True,

                  effsize="ng2")
aov


# ### Seeing interaction effect

# In[14]:


plt.figure(figsize=(5,4))
_ = sns.barplot(x='drink',y='value',data=df_long)


# In[15]:


plt.figure(figsize=(5,4))
_ = sns.barplot(x='atd',y='value',data=df_long)


# In[16]:


from statsmodels.graphics.factorplots import interaction_plot
fig = interaction_plot(df_long.drink, df_long.atd, df_long.value,
             colors=['red','blue','green'], markers=['D','^','*'], ms=10)


# ## Posthoc Test

# In[18]:


from statsmodels.sandbox.stats.multicomp import MultiComparison
multicomp = MultiComparison(df_long['value'], df_long['variable'])   # testfunc


# In[19]:


# Bonferroni
com = multicomp.allpairtest(st.ttest_rel, method='bonf')
print(com[0])


# ## Robust factorial repeated-measures ANOVA

# ### no function for now

# ## Effect Size

# ### see output from pingouin Factorial Repeated Measure Anova table under ***ng2*** column

# In[20]:


from IPython.display import Image
Image("../../../../../Downloads/13.png")


# In[ ]:




