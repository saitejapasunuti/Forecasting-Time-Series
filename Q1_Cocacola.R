#########Forecast the CocaCola prices  data set ###############

install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
install.packages("smooth")
library(smooth) # forsmoothing and MAPE
install.packages("tseries")
library(tseries)
library(readxl)

CocaCola_Sales_Rawdata <- read_excel("D:/360digiTMG/unsupervised/mod26 Forecasting Time Series/Datasets (4)/CocaCola_Sales_Rawdata.xlsx")
View(CocaCola_Sales_Rawdata)

# Converting data into time series object
tssales<-ts(CocaCola_Sales_Rawdata$Sales,frequency = 4,start=c(42))
View(tssales)

# dividing entire data into training and testing data 
train<-tssales[1:38]
test<-tssales[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

# Plotting time series data
plot(tssales) # Visualization shows that it has level, trend, seasonality => Additive seasonality

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape
# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape
# with alpha = 0.2, beta = 0.15, gamma = 0.05 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape
# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape
#
hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape
###train
hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape
#####RMSE Calculation
View(CocaCola_Sales_Rawdata) # Seasonality 4  quarters

# Pre Processing

# So creating 4 dummy variables 
Q1 <-  ifelse(grepl("Q1",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",CocaCola_Sales_Rawdata$Quarter),'1','0')
##
CocacolaData<-cbind(CocaCola_Sales_Rawdata,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)

# input t
CocacolaData["t"] <- c(1:42)
View(CocacolaData)

CocacolaData["log_sales"] <- log(CocacolaData["Sales"])
CocacolaData["t_square"] <- CocacolaData["t"]*CocacolaData["t"]
View(CocacolaData)
## Preprocesing completed

attach(CocacolaData)
# partitioning
train <- CocacolaData[1:38,]
test <- CocacolaData[39:42,]

########################### LINEAR MODEL #############################

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
rmse_linear <- sqrt(mean((test$Sales -linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model <- lm(log_sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales -exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Sales~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(Sales ~ Q1+Q2+Q3, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Sales~ t+t_square+Q1+Q2+Q3, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_sales ~ Q1+Q2+Q3, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)


# Additive seasonality with Quadratic has least RMSE value

write.csv(CocacolaData, file="cocacola_analysis.csv", row.names = F)

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Q1+Q2+Q3, data = CocacolaData)
summary(Add_sea_Quad_model_final)
