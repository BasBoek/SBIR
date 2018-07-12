
# coding: utf-8

# # SBIR FASE 2 -- Python trend analyse

# ### importeren van benodigde packages

# In[1]:


import geopandas as gpd
import matplotlib.pyplot as plt
from shapely import geometry
get_ipython().magic('matplotlib inline')


# ### importeren van de data

# In[42]:


refpercelen = gpd.read_file("G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/refPercelen_TCT_MeanMaxMin_sentinel2.shp")
print(refpercelen)


# In[32]:


refpercelen[['201612911','201704611','201714611','201728811','201731111','201803811','geometry']]


# In[43]:


refpercelen.head()


# In[44]:


refpercelen.geometry.area


# In[45]:


refpercelen.plot(figsize=(10,10))


# ### Connectie met Erdas Imagine Spatial Modeler

# In[17]:


from imagine import modeler


# In[18]:


m = modeler.Model()


# In[19]:


satelliteImageInput = input("satellite image input")
RPCfileInput = input("rpc file input")
ProxyfileOutput = input("proxy file output")
elevationModel = input("hoogtemodel bestand")

sensorModel = m.SensorModelInput(satelliteImageInput, 'DMC-3 RPC', RPCfileInput)
associateElev = m.AssociateElevation(sensorModel, ElevationFilename = elevationModel)
georefUpdate = m.UpdateGeoreferencing(associateElev, satelliteImageInput, 'True', ProxyfileOutput)

m.Execute()


# ### Starting the timeseries analysis

# In[28]:


import pandas as pd
import geopandas as gpd
import numpy as np
get_ipython().magic('matplotlib inline')

from sklearn.metrics import mean_squared_error
from sklearn.ensemble import RandomForestRegressor


# Insert the data using pandas

# In[35]:


dataset = gpd.read_file("G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/refPercelen_TCT_MeanMaxMin_sentinel2.dbf")
dataset.head()


# In[32]:


melt = dataset.melt(id_vars='ID', var_name1 = 'Year', var_name2 ='JulianDay', value_name='Brightness')
melt['Year'] = melt['Year'].str.extract('(\d+)', expand=False).astype(int)
melt['Julian'] = melt['Julian'].str.extract('(\d+)', expand=False).astype(int)
melt = melt.sort_values(['Year', 'Julian', 'ID'])
melt.head()


# In[1]:


import pandas as pd
df1 = pd.read_csv("G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/2016129.csv")
df2 = pd.read_csv("G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/2017046.csv")
df3 = pd.read_csv("G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/2017146.csv")
a="G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/2016129.csv"
b="G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/2017046.csv"
c="G:/02_Imagem_2017/SBIR_Mutatiedetectie/FASE2/OUTPUT/2017146.csv"


# In[13]:


frames=[]
list = [a, b, c]
for i in range(len(list)):
    frames.append(pd.read_csv(list[i], sep=','))
result = pd.concat(frames)
print(result)


# In[4]:


frames=[]
list = [a, b, c]
for i in range(len(list)):
    frames.append(pd.read_csv(list[i], sep=','))
result = pd.concat(frames)
result.drop(result.columns[0],axis=1,inplace=True)
result['id']=result.index
print(result)


# In[5]:


result.head()


# In[20]:


import pandas as pd
tableJoin = pd.read_csv("D:\\IMAGEM\\SBIR\\FASE2\\OUTPUT\\concat_all_test.csv")


# In[21]:


import datetime
date = datetime.datetime(2016, 1, 1) + datetime.timedelta(129)
print(date)
tableJoin


# In[22]:


print(list(tableJoin.Year))


# In[23]:


listYear = list(tableJoin.Year)
listJD = list(tableJoin.JulianDay)
print(listYear, listJD)


# In[24]:


listDate = []
i=0
for i in range(len(listYear)):
    date = datetime.datetime(listYear[i], 1, 1) + datetime.timedelta(int(listJD[i]))
    listDate.append(date)


# In[25]:


print(listDate)


# In[26]:


tableJoin["Date"] = listDate
tableJoin.set_index('Date', inplace=True)


# In[27]:


test = tableJoin[tableJoin['OBJECTID'] == 879844]
test[['Br_Mean']].plot(figsize=(20,20), linewidth=5,fontsize=20)
plt.xlabel('Date',fontsize=20)


# In[28]:


testje = test[['Br_Mean']]
testje.rolling(3).mean().plot(figsize=(20,10), linewidth=5, fontsize=20)
plt.xlabel('Date', fontsize=20)


# In[29]:


testje.diff().plot(figsize=(20,10),linewidth=5,fontsize=20)
plt.xlabel('Date',fontsize=20)


# In[30]:


import seaborn as sns
sns.set()


# In[31]:


sns.lmplot(x='Br_Mean', y='Gr_Mean',fit_reg=False, data=tableJoin, hue='OBJECTID')


# In[32]:


tableJoin.corr()


# In[33]:


tableJoin.groupby(['OBJECTID']).corr()


# In[34]:


test[["Br_Mean","Gr_Mean","We_Mean"]].diff().plot(figsize=(20,10),linewidth=5,fontsize=20)
plt.xlabel('Date',fontsize=20)


# In[35]:


test[["Br_Mean","Gr_Mean","We_Mean"]].diff().corr()


# In[42]:


pd.plotting.autocorrelation_plot(tableJoin[["Br_Mean"]])


# In[46]:


from sklearn.linear_model import LinearRegression
lr = LinearRegression(normalize=True)


# In[ ]:


X = 


# In[155]:


from sklearn.cross_validation import train_test_split
X_train,X_test,y_train,y_test = train_test_split(test[["Br_Mean","Br_Max","Br_Min"]],test[["Br_Mean","Br_Max","Br_Min"]],test_size=0.25,random_state=1, stratify = None)


# In[156]:


print(X_train,X_test)


# In[157]:


print(y_train,y_test)


# In[136]:


X_test.plot()


# In[137]:


y_test.plot()


# In[13]:


import pandas as pd
df = pd.read_csv("D:\IMAGEM\SBIR\FASE2\DOCUMENTEN\dry wet soil_Jarren_HesterEdit.csv", sep=';')


# In[2]:


df


# In[3]:


drySoil_x2=[]


# In[68]:


a = df.query('NaamBeeld == "S2A_MSIL1C_20170705T105031_N0205_R051_T31UFU_20170705T105605.SAFE"')
a


# In[69]:


drySoil_x2.append(a["FileDrySoil_x"])


# In[70]:


drySoil_x2


# In[20]:


filename = ['S2B_MSIL1C_20170705T105031_N0205_R051_T31UFU_20170705T105605.SAFE']


# In[21]:


a = df.query('NaamBeeld == @filename')
b = a["NaamBeeld"]
if len(b.index) ==0:
    b = "bestandsnaamNietAanwezig"
else:
    b = filename
print(b)


# In[ ]:




