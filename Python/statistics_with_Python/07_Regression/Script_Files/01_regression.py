#!/usr/bin/env python
# coding: utf-8

# In[28]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as st
import statsmodels.api as sm
import pingouin as pg


# In[53]:


import statsmodels.stats.outliers_influence as sms
from functools import reduce


# In[29]:


import warnings
warnings.filterwarnings('ignore')


# # Regression

# ### i) Simple Regression
# 
# #### Outcome = Model + Error
# #### Sum of Squares, SSt, SSm , SSr

# In[30]:


data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/07_Regression/Data_Files/Album Sales 1.dat', sep='\t')
print(data.head())


# In[31]:


_ = sns.lmplot(x='adverts', y='sales', data=data)
plt.show()


# In[32]:


model = sm.OLS.from_formula('sales ~ adverts',data=data)
res = model.fit()
print(res.summary())


# ### ii) Multiple Regression

# In[33]:


df = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/07_Regression/Data_Files/Album Sales 2.dat', sep='\t')
print(df.head())


# In[34]:


# with one predictor variable
model_1 = sm.OLS.from_formula("sales~adverts", data=df)
res_1 = model_1.fit()
print(res_1.summary())


# In[35]:


# with all predictor variables
model_2 = sm.OLS.from_formula("sales~adverts+airplay+attract", data=df)
res_2 = model_2.fit()
print(res_2.summary())


# ##### see the inrcrease in R^2 and Adjusted R^2 in model_2 w.r.t model_1

# ### Standarized regression coeffecients (beta coeffecients)
# #####  refers to how many standard deviations a dependent variable will change, per standard deviation increase in the predictor variable.

# In[36]:


df_ = pd.DataFrame()
df_['adverts'] = (df['adverts']-df['adverts'].mean())/df['adverts'].std()
df_['airplay'] = (df['airplay']-df['adverts'].mean())/df['airplay'].std()
df_['attract'] = (df['attract']-df['adverts'].mean())/df['attract'].std()
df_['sales'] = (df['sales']-df['adverts'].mean())/df['sales'].std()


# In[37]:


model = sm.OLS.from_formula("sales~adverts+airplay+attract", data=df_)
res = model.fit()
print(res.summary())


# ### Comparing models using python

# In[38]:


from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm

m01 = ols('sales~adverts', data=df).fit()
m02 = ols('sales~adverts+airplay+attract', data=df).fit()
anovaResults = anova_lm(m01, m02)
print(anovaResults)


# ### Outliers and Influential cases

# #### references
# 
# https://www.statsmodels.org/stable/generated/statsmodels.stats.outliers_influence.OLSInfluence.html#statsmodels.stats.outliers_influence.OLSInfluence
# 
# https://www.statsmodels.org/dev/generated/statsmodels.regression.linear_model.OLS.html
# 
# https://stackoverflow.com/questions/46304514/access-standardized-residuals-cooks-values-hatvalues-leverage-etc-easily-i
# 
# https://www.geeksforgeeks.org/reduce-in-python/

# In[71]:


summary_frame = sms.OLSInfluence(m02).summary_frame()
print(summary_frame.head())


# In[72]:


summary_frame = summary_frame[['cooks_d','standard_resid', 'student_resid', 'hat_diag' ]]
print(summary_frame.head())


# In[73]:


resid = pd.DataFrame(df['sales'] - m02.fittedvalues)
resid.columns = ['residual']


# In[74]:


dfbeta = pd.DataFrame(pd.DataFrame(sms.OLSInfluence(m02).dfbeta)[0])
dfbeta.columns = ['dfbeta']


# In[75]:


df_ = [df, resid, summary_frame, dfbeta]
final_summary = reduce(lambda left,right: pd.merge(left,right, left_index=True, right_index=True), df_)
print(final_summary.head())


# In[76]:


cov_ratio = pd.DataFrame(sms.OLSInfluence(m02).cov_ratio)
cov_ratio.columns = ['cov_ratio']


# In[77]:


# these cases have somewhat large residuals
large_resid  = final_summary[(final_summary['standard_resid']>=2) | (final_summary['standard_resid']<=-2)]
large_resid = pd.merge(large_resid, cov_ratio, how = 'left', right_index=True, left_index=True)
print(large_resid)


# In[78]:


# now let's look at cooks distance, leverage, covariance Ratio for these cases
k = 3 #number of predictors
n = 200 #number of objervations

average_leverage = (k+1)/n
print(average_leverage)


# In[79]:


cvr_limit_high = 1+3*average_leverage
cvr_limit_low  = 1-3*average_leverage

print(cvr_limit_low, cvr_limit_high)


# #### from this large residual model we conclude that
# #### Most of our 12 potential outliers have CVR values within or just outside the boundaries.
# #### none of them has a Cook’s distance greater than 1,  so none of the cases is having an undue influence on the model.
# 
# 
# #### So , Note:
# 
# #### i) Look at standardized residuals and check that no more than 5% of cases have absolute values above 2, 
# ####    and that no more than about 1% have absolute values above 2.5. Any case with a value above about 3 could be an outlier.
# 
# #### ii)Look at the values of Cook’s distance: any value above 1 indicates a case that might be influencing the model.
# 
# #### iii)Calculate the average leverage (the number of predictors plus 1, divided by the sample size) 
# ####      and then look for values greater than twice or three times this average value
# 
# #### iv)Calculate the upper and lower limit of acceptable values for the covariance ratio, CVR.
# ####        The upper limit is 1 plus three times the average leverage, whereas 
# ####        the lower limit is 1 minus three times the average leverage. 
# ####        Cases that have a CVR falling outside these limits may be problematic

# ## Testing Various Assumptions

# ### i) Assumptions of Independent Errors

# In[80]:


from statsmodels.stats.stattools import durbin_watson
print(durbin_watson(m02.resid))

# The closer to 2 that the value is, the better, and for these data the value is 1.950,
# which is so close to 2 that the assumption has almost certainly been met.


# ### ii) Assumption of no multicollinearity

# In[81]:


from statsmodels.tools.tools import add_constant
from statsmodels.stats.outliers_influence import variance_inflation_factor

df_ = add_constant(df)
df_.drop(['sales'], inplace=True,axis=1)  # dropping Dependent variable


# In[82]:


vif = pd.Series([variance_inflation_factor(df_.values, i) 
               for i in range(1, df_.shape[1])], 
              index=df_.columns[1:])


# In[83]:


print(vif)


# In[84]:


avg_vif  = np.mean(vif)
print(avg_vif)


# In[85]:


tolerance  = 1/vif
print(tolerance)


# #####  the assumption of multicollinearity is followed too

# ### iii) Assumption about the Residuals

# In[86]:


prediction = pd.DataFrame(m02.fittedvalues)
prediction.columns = ['predicted']


# In[87]:


prediction['standarized_prediction']  = (prediction['predicted']-prediction['predicted'].mean())/prediction['predicted'].std()
final_summary.head()


# In[89]:


_ = sns.scatterplot(x= final_summary['standard_resid'], y = prediction['standarized_prediction'] )
_ = plt.axhline(y=0)
plt.show()


# In[90]:


_ = pg.qqplot(final_summary['standard_resid'])
plt.show()


# In[99]:


fig,ax = plt.subplots(figsize=(6, 4))
ax = plt.hist(final_summary['student_resid'],density=True,bins=30, edgecolor='black', linewidth=1.4)
plt.xlabel('student_resid', fontsize=14)
plt.show()


# ##### this assumption was also met

# In[ ]:




