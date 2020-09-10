import pandas as pd
import numpy as np



data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/03_Python_Enviroment/Data_Files/Honeymoon Period.dat', sep='\s+')


print(data.head())


print(data.columns)


# wide data to long data
melted_df = pd.melt(data,id_vars=['Person','Gender'], value_vars=['Satisfaction_Base','Satisfaction_6_Months','Satisfaction_12_Months','Satisfaction_18_Months'], 
       var_name='satisfaction_after_time')
print(melted_df.head(10))


# long data to wide data
unmelted_df = pd.pivot_table(melted_df, values='value', index=['Person'],

                    columns=['satisfaction_after_time'], dropna=False)
_ = melted_df[melted_df['satisfaction_after_time']=='Satisfaction_Base']['Gender']
unmelted_df.columns = ['Satisfaction_Base','Satisfaction_6_Months', 'Satisfaction_12_Months','Satisfaction_18_Months']
unmelted_df['Person'] = unmelted_df.index
unmelted_df.reset_index(drop=True, inplace=True)
unmelted_df['Gender'] = _


# data converted from long to wide
print(unmelted_df)



