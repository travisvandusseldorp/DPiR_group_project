#!/usr/bin/env python
# coding: utf-8

# In[1]:


# Import libraries, set options to display all columns in dataframes
import pandas as pd
import numpy as np
import re
import requests
from bigfloat import *
from lxml import html
from functools import partial, reduce 

pd.set_option('display.max_columns', None)  
pd.__version__


# In[ ]:





# In[2]:


# Load financial data
subs = pd.read_csv("data/2019q1//sub.txt", sep='\t')# parsedates=['accepted'])) 
pre = pd.read_csv("data/2019q1/pre.txt", sep='\t') 
num = pd.read_csv("data/2019q1/num.txt", sep='\t')#, parsedates=['ddate']) 
tag = pd.read_csv("data/2019q1/tag.txt", sep='\t') 

subs = subs.append(pd.read_csv("data/2019q2/sub.txt", sep='\t'))
pre = pre.append(pd.read_csv("data/2019q2/pre.txt", sep='\t'))
num = num.append(pd.read_csv("data/2019q2/num.txt", sep='\t'))#, parsedates=['ddate']))

subs = subs.append(pd.read_csv("data/2019q3/sub.txt", sep='\t'))
pre = pre.append(pd.read_csv("data/2019q3/pre.txt", sep='\t'))
num = num.append(pd.read_csv("data/2019q3/num.txt", sep='\t'))

subs = subs.append(pd.read_csv("data/2018q1/sub.txt", sep='\t'))
pre = pre.append(pd.read_csv("data/2018q1/pre.txt", sep='\t'))
num = num.append(pd.read_csv("data/2018q1/num.txt", sep='\t'))#, parsedates=['ddate']))

subs = subs.append(pd.read_csv("data/2018q2/sub.txt", sep='\t'))
pre = pre.append(pd.read_csv("data/2018q2/pre.txt", sep='\t'))
num = num.append(pd.read_csv("data/2018q2/num.txt", sep='\t'))#, parsedates=['ddate']))

subs = subs.append(pd.read_csv("data/2018q3/sub.txt", sep='\t'))
pre = pre.append(pd.read_csv("data/2018q3/pre.txt", sep='\t'))
num = num.append(pd.read_csv("data/2018q3/num.txt", sep='\t'))#, parsedates=['ddate']))

subs = subs.append(pd.read_csv("data/2018q4/sub.txt", sep='\t'))
pre = pre.append(pd.read_csv("data/2018q4/pre.txt", sep='\t'))
num = num.append(pd.read_csv("data/2018q4/num.txt", sep='\t'))#, parsedates=['ddate']))


# In[3]:


# Load and clean ticker and ciks
ciks = pd.read_csv('data/ticker_cik.csv')
ciks.dropna(inplace=True)
ciks['cik'] = ciks['cik'].astype('int64')


# In[4]:


new_sub = pd.merge(left=ciks, right=subs, left_on='cik', right_on='cik')
new_sub = new_sub[['ticker', 'adsh', 'name', 'form', 'period', 'fp', 'fy', 'fye']]


# In[5]:


new_sub.head()


# In[6]:


subs.head()


# In[ ]:





# In[ ]:





# In[7]:


new_pre = pre[['adsh', 'line', 'stmt', 'tag', 'plabel', 'negating']]


# In[8]:


new_pre.head()


# In[9]:


sub_pre = pd.merge(left=new_sub, right=new_pre, on='adsh' )


# In[11]:


sub_pre.head(20)


# In[12]:


new_num = num[['adsh', 'tag', 'version', 'ddate', 'qtrs', 'uom', 'value', 'footnote']]


# In[13]:


merged_items = pd.merge(left=sub_pre, right=new_num, on=['adsh', 'tag'])


# In[14]:


merged_items.head()


# In[16]:


aapl = merged_items[merged_items['ticker']=='aapl']


# In[428]:


##### NEED TO FIGURE THIS OUT.... maybe due to the common numbers like below
fy_convert = pd.to_numeric(aapl['fy'], downcast='integer')


# In[ ]:





# In[17]:


statement_list = []
statment_types = aapl['stmt'].unique()
for statement_type in statment_types:
    statement = aapl[aapl['stmt'] == statement_type]
    statement_list.append(statement)  


# In[ ]:


###################         SKIP         ############################################################
########################################################################################################################
########################################################################################################################


# In[18]:


q10_list = []
k10_list = []
states = {}

for statement in statement_list:
    annual = statement[statement['form']=='10-K']
    a_stmt_type = statement['stmt']

    quarterly = statement[statement['form']=='10-Q']
    q_stmt_type = statement['stmt']
    
    for version in quarterly['version'].unique():

        v = quarterly[quarterly['version']==version]
        q10 = v.pivot_table(index=['ticker', 'form', 'fy', 'stmt', 'line', 'plabel'], columns=['fp'], values = 'value')
        q10_list.append(q10)
        
        v2 = annual[annual['version']==version]
        k10 = v2.pivot_table(index=['ticker', 'form', 'stmt', 'line', 'plabel'], columns=['ddate'], values = 'value')
        k10_list.append(k10)
        
states['10-Q'] = q10_list
states['10-K'] = k10_list
        


# In[ ]:





# In[20]:


##### FOR QUARTERLY STATEMENTS
quarterly_1 = []
for i in range(0,len(states['10-Q'])):
    m = states['10-Q'][i].unstack(level=2)
    #.unstack(level=2)
    display(m)
    quarterly_1.append(m)


# In[21]:


year_labels = []
quarterly_statments_clean = []
for i in quarterly_1:
    year = i.columns.get_level_values(1)[0]
    year_labels.append(year)
    tmp_df = i.copy()
    tmp_df.columns = i.columns.droplevel('fy')
    quarterly_statments_clean.append(tmp_df)


# In[22]:


quarterly_statments_clean[0]


# In[23]:


#### Converts year labels to int but will only keep one of each year
[int(i) for i in year_labels]


# In[ ]:


########################################################################################################################
########################################################################################################################


# In[ ]:





# In[72]:


q10_list = []
k10_list = []
states = {}

for statement in statement_list:
    annual = statement[statement['form']=='10-K']
    a_stmt_type = statement['stmt']

    quarterly = statement[statement['form']=='10-Q']
    q_stmt_type = statement['stmt']
    
    for version in quarterly['version'].unique():

        v = quarterly[quarterly['version']==version]
        q10 = v.pivot_table(index=['ticker', 'form', 'stmt', 'line', 'plabel'], columns=['ddate'], values = 'value')
        q10_list.append(q10)
        
        v2 = annual[annual['version']==version]
        k10 = v2.pivot_table(index=['ticker', 'form', 'stmt', 'line', 'plabel'], columns=['ddate'], values = 'value')
        k10_list.append(k10)
        
states['10-Q'] = q10_list
states['10-K'] = k10_list


# In[74]:


quarter_l = []
for i in range(0,len(states['10-Q'])):
    m = states['10-Q'][i]
    display(m)
    quarter_l.append(m)
    
    

statment_labels = []

for i in annual_l:
    
    #print(i.index.names)
    stt = i.index.get_level_values(2).unique()
    statment_labels.append(list(stt))
    #year = i.columns.get_level_values(1)[0]
    #year_labels.append(year)
    #tmp_df = i.copy()
    #tmp_df.columns = i.columns.droplevel('fy')
    #annual_statments_clean.append(tmp_df)

statement_labels = [''.join(x) for x in statment_labels]


# In[76]:


mamba = dict(zip(statement_labels,quarter_l))


# In[79]:


mamba['IS']


# In[24]:


########## FOR ANNUAL STATEMENET
annual_l = []
for i in range(0,len(states['10-K'])):
    m = states['10-K'][i]
    display(m)
    annual_l.append(m)


# In[63]:


year_labels2 = []
statment_labels = []
annual_statments_clean = []
for i in annual_l:
    
    #print(i.index.names)
    stt = i.index.get_level_values(2).unique()
    statment_labels.append(list(stt))
    #year = i.columns.get_level_values(1)[0]
    #year_labels.append(year)
    #tmp_df = i.copy()
    #tmp_df.columns = i.columns.droplevel('fy')
    #annual_statments_clean.append(tmp_df)

statement_labels = [''.join(x) for x in statment_labels]


# In[68]:


mamba = dict(zip(statement_labels,annual_l))


# In[71]:


mamba['IS']


# In[ ]:





# In[ ]:




