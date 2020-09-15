#!/usr/bin/env python
# coding: utf-8

# ## **Multinomial logistic regression**

# In[6]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
import scipy.stats as st
import pingouin as pg


# In[21]:


chat_up  = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/08_LogisticRegression/Data_Files/Chat-Up Lines.dat', sep='\t')
print(chat_up.head())


# In[22]:


print(chat_up['Success'].unique())


# In[23]:


chat_up['Successx'] = chat_up['Success'].replace({'Get Phone Number':1, 'Go Home with Person':2, 'No response/Walk Off':0})
chat_up['Genderx'] = chat_up['Gender'].replace({'Male':0, 'Female':1})

chat_up['Gen_Funny'] = chat_up['Genderx'] * chat_up['Funny']
chat_up['Gen_Sex'] = chat_up['Genderx'] * chat_up['Sex']


# In[26]:


import statsmodels.formula.api as smf
ml01 = smf.mnlogit('Successx ~ Funny + Sex + Good_Mate +Genderx+Gen_Funny+ Gen_Sex', chat_up).fit()
print(ml01.summary())


# In[27]:


print(np.exp(ml01.params))


# ## Checking Assumptions

# ### Assumptions of Multicollinearity

# In[29]:


from statsmodels.stats.outliers_influence import variance_inflation_factor


# In[31]:


chat_up_ = chat_up.copy()
chat_up_.drop(['Success', 'Successx', 'Gender','Gen_Funny', 'Gen_Sex'], inplace=True, axis=1)
print(chat_up_.head())


# In[33]:


from statsmodels.tools.tools import add_constant


# In[34]:


chat_up_ = add_constant(chat_up_)


# In[36]:


vif = pd.Series([variance_inflation_factor(chat_up_.values, i) 
               for i in range(1, chat_up_.shape[1])], 
              index=chat_up_.columns[1:])

tolerance  = 1/vif


# In[37]:


print(vif)


# In[38]:


print(tolerance)


# ### also correlation value will show that there is no problem of multicollinearity.So, assumption of  multicollinearity has been followed

# In[40]:


# also, correlation table seems ok
print(chat_up[['Funny', 'Sex', 'Good_Mate']].corr())


# ### Assumption of Linearity

# In[44]:


for i in chat_up_.columns:
    if i=='const' or i=='Genderx':
        pass
    else:
        v = f'log_{i}'
        chat_up_[v] = np.log(chat_up_[i])*chat_up_[i]


# In[45]:


chat_up_['Successx'] = chat_up['Successx']
chat_up_['Gen_Funny'] = chat_up['Gen_Funny']
chat_up_['Gen_Sex'] = chat_up['Gen_Sex']


# In[46]:


ml02 = smf.mnlogit('Successx ~ Funny + Sex + Good_Mate +Genderx+Gen_Funny+ Gen_Sex+log_Funny+log_Sex+log_Good_Mate', chat_up_).fit()
print(ml02.summary())


# ## by seeing the log interaction term, its pretty clear that many values has significance p<0.05, hence the assumption of linearity of logit has been violated

# In[ ]:




