#!/usr/bin/env python
# coding: utf-8

# In[7]:


import pandas as pd
import statsmodels.api as sm
from statsmodels.tools.tools import add_constant


# In[8]:


data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/07_Regression/Data_Files/Album Sales 2.dat' , sep='\t')
data = add_constant(data)
print(data.head())


# In[9]:


a_ = data[['const','adverts', 'airplay', 'attract']]
b_ = data[['sales']]


# ## What if I violate an assumption!

# In[10]:


rlm_model = sm.RLM(b_, a_)
rlm_res = rlm_model.fit()
print(rlm_res.summary())


# In[ ]:




