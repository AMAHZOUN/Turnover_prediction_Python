# 1 ère cas XGboost en utilisant les données Open Data
library(xgboost)
library(tidyverse)
library(ggfortify)
library(tidyverse)
library(dbplyr)
library(lubridate)
# Importing the dataset 
data = read.csv("CrOpen1.csv",sep =';')
data$Date2 <- NULL
data$X <- NULL
data$X.1 <- NULL
ymd("2016-01-31")
##data$DATE <- ymd(data$DATE)
str(data)
ts1 <- timetk::tk_ts(data$ca, start = c(2016, 01),end = c(2019,12), frequency = 12)
ts2 <- timetk::tk_ts(data$Inflation, start = c(2016, 01),end = c(2019, 12), frequency = 12)
ts3 <- timetk::tk_ts(data$Taux_de_Chomage, start = c(2016, 01),end = c(2019, 12), frequency = 12)
ts4 <- timetk::tk_ts(data$Indice.prix.de.production.industrie, start = c(2016, 01),end = c(2019, 12), frequency = 12)
ts5 <- timetk::tk_ts(data$Indice.production.industrielle, start = c(2016, 01),end = c(2019, 12), frequency = 12)
ts <- ts.union(ts1,ts2,ts3,ts4,ts5)
data1 <- timetk::tk_tbl(ts)
data1
# Ajouter les valeurs à prédire 
extended_ts <- stats::ts((c(ts1, rep(NA, 12))),
                         start = start(ts1), 
                         frequency = 12)
class(extended_ts)
extended_ts
ts5 <- ts.union(extended_ts,ts2,ts3,ts4,ts5)
names(extended_ts) <- c("Chiffre d'affaire")
extended_data <- timetk::tk_tbl(ts5)
extended_data
names(extended_data) <- c("Date","Chiffre d'affaire","Inflation","Taux de chomage","indice prix de production industrie","Indice production industrielle")
extended_data
# ajouter des variables pour XGboost 
extended_data_mod <- extended_data %>%
  dplyr::mutate(., 
                index_date = as.Date(paste0(lubridate::year(Date), "-", lubridate::month(Date), "-01")),
                months = lubridate::month(index_date),
                years = lubridate::year(index_date))
extended_data_mod$index_date <- NULL
extended_data_mod
class(extended_data_mod)
length(extended_data_mod)
nrow(extended_data_mod)
class(ts4)
extended_data_mod
str(extended_data_mod)
# Split the data into Training and Prediction
dt <- extended_data_mod[1:nrow(ts), ]
pred <- extended_data_mod[(nrow(ts) + 1):nrow(extended_data), ]
dt
pred
extended_data_mod
# creating the model 
X_train <- xgboost::xgb.DMatrix(as.matrix(dt %>% 
                                            dplyr::select(months, years,Inflation,`Taux de chomage`,`indice prix de production industrie`,`Indice production industrielle`)))
X_train
X_pred  <- xgboost::xgb.DMatrix(as.matrix(pred %>% 
                                            dplyr::select(months, years,Inflation,`Taux de chomage`,`indice prix de production industrie`,`Indice production industrielle`)))
X_pred
data
y_train <- data$ca
y_train
# Xgboost Prediction

xgb_trcontrol <- caret::trainControl(method = 'cv',number = 5, allowParallel = TRUE, verboseIter = FALSE, returnData = FALSE)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20),
    colsample_bytree = seq(0.5),
    eta = 0.1,
    gamma = 0,
    min_child_weight = 1,
    subsample = 1
  ))
# build the model 

xgb_model <- caret::train(X_train, y_train, trControl = xgb_trcontrol, tuneGrid = xgb_grid, method ="xgbTree", nthread = 1)
xgb_model$bestTune

# prediction
X_pred
xgb_pred <- xgb_model %>% stats::predict(X_pred)
xgb_pred
class(xgb_pred)
fitted <- xgb_model %>% 
  stats::predict(X_train) %>%
  stats::ts(start = min(dt$Date), end = max(dt$Date), frequency = 12)
fitted
class(fitted)
ft <- fortify(fitted)
ft
xgb_forecast <- xgb_pred %>% 
  stats::ts(start = min(pred$Date), end = max(pred$Date), frequency = 12)
xgb_forecast
Pred1 <- fortify(xgb_forecast)
Pred1
names(Pred1) <- c("Date","Future Predictions")
