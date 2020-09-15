#!/usr/bin/env python
# coding: utf-8

# In[33]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as st
import statsmodels.api as sm
import pingouin as pg
import statsmodels.stats.outliers_influence as sms
from functools import reduce
from statsmodels.stats.stattools import durbin_watson
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm


# In[3]:


da = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/07_Regression/Data_Files/GlastonburyFestivalRegression.dat', sep='\t')
print(da.head())


# In[5]:


da.replace(' ' ,np.nan, inplace=True)
da['music'].unique()


# In[6]:


da_ = da[da['change'].notna()]


# In[8]:


dummies = pd.get_dummies(da_['music'])
dummy = dummies.iloc[:,0:3]
print(dummy.head())


# In[9]:


da_ = pd.merge(da_, dummy, how='inner', left_index=True, right_index=True)

col =list(da_.columns)
col[-2]='Indie'
da_.columns=col

print(da_.info())


# In[13]:


da_['change']=pd.to_numeric(da_['change'])
m03 = ols('change~Crusty+Indie+Metaller', data=da_).fit()
print(m03.summary())


# #### what does these coeffiecients represent this actually represents the difference in the change in hygiene scores if a person has no musical affiliation,  compared to someone who is a crusty, indie, metal, Rescpectively

# ### Outliers and Influential cases

# In[17]:


summary_ = sms.OLSInfluence(m03).summary_frame()
summary_ = summary_[['cooks_d','standard_resid', 'student_resid', 'hat_diag' ]]
summary_.reset_index(inplace=True, drop=True)
print(summary_.head())


# In[19]:


da_.reset_index(inplace=True, drop=True)

resid = pd.DataFrame(da_['change'] - m03.fittedvalues)
resid.columns = ['residual']
resid.reset_index(inplace=True, drop=True)


# In[21]:


dfbeta = pd.DataFrame(pd.DataFrame(sms.OLSInfluence(m03).dfbeta)[0])
dfbeta.columns = ['dfbeta']


# In[25]:


cov_ratio = pd.DataFrame(sms.OLSInfluence(m03).cov_ratio)
cov_ratio.columns = ['cov_ratio']


# In[22]:


dq = [da_, resid, summary_, dfbeta]


# In[23]:


final_summary = reduce(lambda left,right: pd.merge(left,right, left_index=True, right_index=True), dq)


# In[26]:


large_resid  = final_summary[(final_summary['standard_resid']>=2) | (final_summary['standard_resid']<=-2)]
print(large_resid)


# In[29]:


k = 3 #number of predictors
n = 123 #number of objervations

average_leverage = (k+1)/n
print(average_leverage)


# In[30]:


cvr_limit_high = 1+3*average_leverage
cvr_limit_low  = 1-3*average_leverage

print(cvr_limit_low, cvr_limit_high)


# In[31]:


print(cov_ratio.iloc[7])
print(cov_ratio.iloc[21])
print(cov_ratio.iloc[26])
print(cov_ratio.iloc[46])
print(cov_ratio.iloc[69])


# #### Therefore, we are looking for any cases that deviate substantially from these boundaries.. However, given the Cook’s distance for this case, there is probably little cause for alarm.None of them has a Cook’s distance greater than 1, so none of the cases is having an undue influence on the model.

# ## Checking Assumptions

# #### Checking Assumptions of Independent Errors

# In[35]:


print(durbin_watson(m03.resid))


# #### Assumption of no multicollinearity

# In[37]:


from statsmodels.tools.tools import add_constant
from statsmodels.stats.outliers_influence import variance_inflation_factor
da_ = add_constant(da_)


# In[38]:


dz = da_[['const', 'Crusty', 'Indie', 'Metaller']]


# In[39]:


dz.reset_index(drop=True, inplace=True)


# In[40]:


vif = pd.Series([variance_inflation_factor(dz.values, i) 
               for i in range(1, dz.shape[1])], 
              index=dz.columns[1:])
print(vif)


# In[41]:


avg_vif  = np.mean(vif)
print(avg_vif)


# In[42]:


tolerance  = 1/vif
tolerance 


# #### Assumption about the Residuals

# In[44]:


prediction = pd.DataFrame(m03.fittedvalues)
prediction.columns = ['predicted']
prediction.reset_index(drop=True, inplace=True)
print(prediction.head())


# In[45]:


prediction['standarized_prediction']  = (prediction['predicted']-prediction['predicted'].mean())/prediction['predicted'].std()
_ = sns.scatterplot(x= final_summary['standard_resid'], y = prediction['standarized_prediction'] )
plt.show()


# In[46]:


_ = pg.qqplot(final_summary['standard_resid'],)
plt.show()


# In[47]:


fig,ax = plt.subplots(figsize=(6, 4))
ax = plt.hist(final_summary['student_resid'],density=True,bins=30, edgecolor='black', linewidth=1.4)
plt.xlabel('student_resid', fontsize=14)
plt.show()


# ### all assumptions are met :)

# In[ ]:




