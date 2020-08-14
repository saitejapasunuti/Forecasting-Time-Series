################## Forecast the  Airlines Passengers data set ##########################

import pandas as pd

from pandas import Series,DataFrame

#load dataset
Airlines = pd.read_excel("D:/360digiTMG/unsupervised/mod26 Forecasting Time Series/Datasets (4)/Airlines Data.Xlsx")
type(Airlines)
month =['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'] 

import numpy as np
Airlines['Month'] = pd.DatetimeIndex(Airlines['Month']).month
##    
month_dummies = pd.DataFrame(pd.get_dummies(Airlines['Month']))
Airlines1 = pd.concat([Airlines,month_dummies],axis = 1)
Airlines1.columns = ['Month','Passengers','M1','M2','M3','M4','M5','M6','M7',
                    'M8','M9','M10','M11','M12']
Airlines1.columns
Airlines1["t"] = np.arange(1,97)

Airlines1["t_squared"] = Airlines1["t"]*Airlines1["t"]
Airlines1.columns
Airlines1["log_Rider"] = np.log(Airlines1["Passengers"])
Airlines1.rename(columns={"Passengers ": 'Passengers'}, inplace=True)
Airlines1.Passengers.plot()
Train = Airlines1.head(84)
Test = Airlines1.tail(12)

# to change the index value in pandas data frame 
# Test.set_index(np.arange(1,13))

####################### L I N E A R ##########################
import statsmodels.formula.api as smf 

linear_model = smf.ols('Passengers~t',data=Train).fit()
pred_linear =  pd.Series(linear_model.predict(pd.DataFrame(Test['t'])))
rmse_linear = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(pred_linear))**2))
rmse_linear

##################### Exponential ##############################

Exp = smf.ols('log_Rider~t',data=Train).fit()
pred_Exp = pd.Series(Exp.predict(pd.DataFrame(Test['t'])))
rmse_Exp = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(np.exp(pred_Exp)))**2))
rmse_Exp

#################### Quadratic ###############################

Quad = smf.ols('Passengers~t+t_squared',data=Train).fit()
pred_Quad = pd.Series(Quad.predict(Test[["t","t_squared"]]))
rmse_Quad = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(pred_Quad))**2))
rmse_Quad

################### Additive seasonality ########################

add_sea = smf.ols('Passengers~M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11',data=Train).fit()
pred_add_sea = pd.Series(add_sea.predict(Test[['M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11']]))
rmse_add_sea = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(pred_add_sea))**2))
rmse_add_sea

################## Additive Seasonality Quadratic ############################

add_sea_Quad = smf.ols('Passengers~t+t_squared+M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11',data=Train).fit()
pred_add_sea_quad = pd.Series(add_sea_Quad.predict(Test[['M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','t','t_squared']]))
rmse_add_sea_quad = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(pred_add_sea_quad))**2))
rmse_add_sea_quad 

################## Multiplicative Seasonality ##################

Mul_sea = smf.ols('log_Rider~M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11',data = Train).fit()
pred_Mult_sea = pd.Series(Mul_sea.predict(Test))
rmse_Mult_sea = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(np.exp(pred_Mult_sea)))**2))
rmse_Mult_sea

##################Multiplicative Additive Seasonality ###########

Mul_Add_sea = smf.ols('log_Rider~t+M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11',data = Train).fit()
pred_Mult_add_sea = pd.Series(Mul_Add_sea.predict(Test))
rmse_Mult_add_sea = np.sqrt(np.mean((np.array(Test['Passengers'])-np.array(np.exp(pred_Mult_add_sea)))**2))
rmse_Mult_add_sea 

################## Testing #######################################

data = {"MODEL":pd.Series(["rmse_linear","rmse_Exp","rmse_Quad","rmse_add_sea","rmse_add_sea_quad","rmse_Mult_sea","rmse_Mult_add_sea"]),"RMSE_Values":pd.Series([rmse_linear,rmse_Exp,rmse_Quad,rmse_add_sea,rmse_add_sea_quad,rmse_Mult_sea,rmse_Mult_add_sea])}
table_rmse=pd.DataFrame(data)
table_rmse
##
##               MODEL  RMSE_Values
##0        rmse_linear    53.199237
##1           rmse_Exp    46.057361
##2          rmse_Quad    48.051889
##3       rmse_add_sea   132.819785
##4  rmse_add_sea_quad    26.360818
##5      rmse_Mult_sea   140.063202
##6  rmse_Mult_add_sea    10.519173
