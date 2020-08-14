#######Forecast the Airlines Passengers data set ###################

install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
install.packages("smooth")
library(smooth) # forsmoothing and MAPE
install.packages("tseries")
library(tseries)
library(readxl)
Airlines <- read_excel("D:/360digiTMG/unsupervised/mod26 Forecasting Time Series/Datasets (4)/Airlines Data.Xlsx")
View(Airlines)

# Converting data into time series object
tssales<-ts(Airlines$Passengers,frequency = 12,start=c(96))
View(tssales)

# dividing entire data into training and testing data test data taken as 1 year
train<-tssales[1:84]
test<-tssales[85:96] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)

# Plotting time series data
plot(tssales) # Visualization shows that it has level, trend, seasonality => Additive seasonality

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=12))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=12))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape ##17.2363
# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 12))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=12))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape ###11.216
# with alpha = 0.2, beta = 0.15, gamma = 0.05 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 12))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=12))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape
# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 12))
hwna_pred
plot(forecast(hw_na,h=12))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape ##18.555
#
hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=12))
hwnab_pred
plot(forecast(hw_nab,h=12))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape ###13.104
###train
hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw_nabg,h=12))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape ###1.7308
#####RMSE Calculation
View(Airlines) # Seasonality 12  quarters

# Pre Processing

# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names 
View(X)
Airlinespassenger <- cbind(Airlines,X)
View(Airlinespassenger)

colnames(Airlinespassenger)

# input t
Airlinespassenger["t"] <- c(1:96)
View(Airlinespassenger)

Airlinespassenger["log_Passengers"] <- log(Airlinespassenger["Passengers"])
Airlinespassenger["t_square"] <- Airlinespassenger["t"]*Airlinespassenger["t"]
View(Airlinespassenger)
-## Preprocesing completed

attach(Airlinespassenger)
# partitioning
train <- Airlinespassenger[1:84,]
test <- Airlinespassenger[85:96,]

########################### LINEAR MODEL #############################

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model <- lm(log_Passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)


# Additive seasonality with Quadratic has least RMSE value

write.csv(Airlinespassenger, file="Airlines.csv", row.names = F)

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Airlinespassenger)
summary(Add_sea_Quad_model_final)

##### end ####