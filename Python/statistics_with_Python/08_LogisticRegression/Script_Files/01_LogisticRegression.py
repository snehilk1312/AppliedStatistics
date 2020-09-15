#!/usr/bin/env python
# coding: utf-8

# In[76]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
import scipy.stats as st
import pingouin as pg


# In[77]:


data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/08_LogisticRegression/Data_Files/eel.dat', sep='\t')
print(data.head())


# In[78]:


print(data.info())


# In[79]:


print(data['Cured'].unique())


# In[80]:


print(data['Intervention'].unique())


# In[81]:


data['Curedx'] = data['Cured'].replace({'Not Cured':0,'Cured':1 })
data['Interventionx'] = data['Intervention'].replace({'No Treatment':0,'Intervention':1 })


# In[82]:


print(data.head())


# In[83]:


from statsmodels.tools.tools import add_constant
data=add_constant(data)


# In[112]:


m01 = sm.Logit(data['Curedx'] , data[['const', 'Interventionx']]).fit()
print(m01.summary())


# In[113]:


print(m01.summary2())


# ##### AIC =      -2 * log_likelihood + 2 * k

# In[114]:


AIC = -2*(-72.079)+ 2*2
print(AIC)


# In[115]:


print(m01.llf)           #get Log-Likelihood
print(m01.llr)           #get chi-square
print(m01.llr_pvalue)    #get sig. level of chi-square test
print(m01.prsquared)     #get pseudo-rsquared 


# In[116]:


print(-2*(m01.llnull-m01.llf))


# In[117]:


print(m01.llr)


# ### **chi-square = deviance_null - deviance_new**

# In[118]:


chi_square  = (-2*(-77.042))-(-2*(-72.079))          # previous_deviance - present_deviance
print(chi_square)


# In[119]:


chi_df = 1
print(1 - st.chi2.cdf(chi_square, chi_df))


# ### Seeing above p-value we reject null hypothesis that m01 is not better than just chance at predicting outcome.

# ### **Note: The model chi-square is an analogue of the F-test for the linear regression**

# #### see https://www.statsmodels.org/stable/generated/statsmodels.discrete.discrete_model.LogitResults.html

# In[120]:


print(m01.aic)


# In[121]:


print(m01.bic)


# In[122]:


print(m01.bse)


# In[123]:


print(m01.prsquared)


# In[124]:


print(m01.resid_dev)


# #### chi_df = This creates a value called chidf that is the degrees of freedom for the model  subtracted from the degrees of freedom for the null model. 

# In[125]:


chi_df = 112-111
print(chi_df)


# In[126]:


z_statsistics = 3.074  # from model
deviance_null = -2*m01.llnull


# In[127]:


R  = np.sqrt((z_statsistics**2 - 2*chi_df)/deviance_null)
print(R)


# #### The three different R_Squared :

# In[128]:


deviance_new = -2*m01.llf
deviance_new


# In[129]:


def R2(deviance_null, deivance_new, n):
    R2_l = (deviance_null-deviance_new)/deviance_null
    R2_cs = 1-np.exp((deviance_new-deviance_null)/n)
    R2_n = R2_cs/(1-np.exp(-(deviance_null/n)))
    print("pseudo R^2 for logistic regression:\n")
    print(f"Hosmer and Lemeshow R^2: {R2_l}\n")
    print(f"Cox and Snell R^2: {R2_cs} \n")
    print(f"Nagelkerke R^2 : {R2_n}\n")


# In[130]:


R2(deviance_null, deviance_new, len(data))


# In[131]:


print(np.exp(m01.conf_int()))


# In[132]:


print(m01.fittedvalues)


# In[133]:


print(1/(1+np.exp(-(m01.fittedvalues))))


# In[134]:


m02 = sm.Logit(data['Curedx'] , data[['const', 'Interventionx','Duration']]).fit()
print(m02.summary())


# In[135]:


print(m02.llf*(-2))     # deviance of m02


# In[136]:


print(m02.resid_dev.head())


# In[137]:


model_chi = -2*(m01.llf - m02.llf)     # previous_deviance - present_deviance
print(model_chi)


# In[138]:


chi_df = 111 - 110
print(chi_df)


# In[139]:


print(1 - st.chi2.cdf(model_chi, chi_df))


# ### m02 is not much of an improvement over model 1 , by seeing above output.

# In[145]:


from statsmodels.genmod import families


# In[146]:


res = sm.GLM(data['Curedx'] , data[['const', 'Interventionx','Duration']],
          family=families.Binomial()).fit(attach_wls=True, atol=1e-10)
print(res.summary())


# In[147]:


infl = res.get_influence(observed=False)


# In[148]:


summ_df = infl.summary_frame()
summ_df.sort_values('standard_resid', ascending=False)[:10]


# ## **Diagnostic statistics**

# ### You need to look for cases that might be influencing the logistic regression model:
# 
# 
# #### i) Look at standardized residuals and check that no more than 5% of cases have absolute values above 2, and that no more than about 1% have absolute values above 2.5. Any case with a value above about 3 could be an outlier.
# 
# 
# 
# #### ii)  Calculate the average leverage (the number of predictors plus 1, divided by the sample size) and then look for values greater than twice or three times this average value.
# 
# 
# 
# #### iii)  Look for absolute values of DFBeta greater than 1.

# #### all cases have DFBetas less than 1, and leverage statistics are very close to the calculated expected value of 0.018.
# #### All in all, this means that there are no influential cases having an effect on the model. 
# #### The studentized residuals all have values of less than ±2 and so there seems to be very little here to concern us.

# # Another Example

# In[157]:


pen_df = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/08_LogisticRegression/Data_Files/penalty.dat', sep='\t')


# In[161]:


print(pen_df.head())


# In[162]:


pen_df['Scored'].unique()


# In[163]:


pen_df['Scoredx']  = pen_df['Scored'].replace({'Scored Penalty':1, 'Missed Penalty':0})


# In[164]:


pen_df = add_constant(pen_df)


# In[175]:


p01 = sm.Logit(pen_df['Scoredx'], pen_df[['const', 'PSWQ', 'Previous']]).fit()
print(p01.summary())


# In[176]:


model1_chi_sq = -2*(p01.llnull - p01.llf)   # p01.llr
print(model1_chi_sq)


# In[177]:


chi_df1 = 2


# In[180]:


chisq_prob1 =  print(1 - st.chi2.cdf(model1_chi_sq, chi_df1))   # significant p-value


# ### the chisquare probability 'chisq_prob1' value is less than 0.05 which tells that this model was quite an improvement over a null model(i.e just chance)

# In[183]:


p02 = sm.Logit(pen_df['Scoredx'], pen_df[['const',  'PSWQ', 'Previous', 'Anxious']]).fit()
print(p02.summary())


# In[184]:


model2_chi_sq = -2*(p01.llf - p02.llf)   
print(model2_chi_sq)


# In[185]:


chi_df2 = 1


# In[187]:


chisq_prob2 =  print(1 - st.chi2.cdf(model2_chi_sq, 1))   # non-significant p-value , so no improvement 


# ###  the chisquare probability 'chisq_prob2' value is greater than 0.05 , which tells that this model(i.e p02) was a improvement over p01  , just by chance.

# In[188]:


print(p01.aic)
print(p02.aic)


# In[189]:


print(p01.bic)
print(p02.bic)


# In[190]:


copy_df = pen_df.copy()


# In[191]:


copy_df.drop(['Scored','Scoredx'], inplace=True, axis=1)


# In[192]:


copy_df .head()


# In[193]:


cov = p02.cov_params()
corr = cov / p02.bse / p02.bse[:, None]
vif = np.diag(np.linalg.inv(corr.values[1:, 1:]))[[1, 0, 2]]


# In[196]:


print(vif)


# #### https://stats.stackexchange.com/questions/474964/calculating-variance-inflation-factor-for-logistic-regression-using-statsmodels/475233#475233

# In[197]:


tolerance = 1/vif
print(tolerance)


# ### from the output of  vif and tolerance , we can deduce that there is a high multicollinearity in our model 

# ### **Testing for linearity of the logit**

# In[198]:


pen_df.head()


# In[199]:


pen_df['log_PSWQ'] = np.log(pen_df['PSWQ'])*pen_df['PSWQ']
pen_df['log_Anxious'] = np.log(pen_df['Anxious'])*pen_df['Anxious']
pen_df['log_Previous'] = np.log(pen_df['Previous']+1)*pen_df['Previous']


# In[201]:


p03 = sm.Logit(pen_df['Scoredx'], pen_df[['const',  'PSWQ', 'Previous', 'Anxious','log_PSWQ', 'log_Anxious', 'log_Previous']]).fit()
print(p03.summary())


# ## From the summary output , if any interaction term has significance less than 0.05 , it will mean that assumption of linearity has been violated. In our output we can conclude that the assumption of linearity has been met as all interaction term is non-significant
