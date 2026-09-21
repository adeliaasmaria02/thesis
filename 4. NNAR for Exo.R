# exogenous variables (its values after differencing) were forecasted 
# using NNAR and ARIMA 

library("nnfor")
library(forecast)
library(tsfgrnn)
library(tseries)
library(ggplot2)
library(writexl)
library(patchwork)
library(strucchange)
library(zoo)

# uji terasvirta
terasvirta.test(x=as.ts(dfr$x1), type = 'F', scale=TRUE)
terasvirta.test(x=as.ts(dfr$x2), type = 'F', scale=TRUE)
terasvirta.test(x=as.ts(df$x3), type = 'F', scale=TRUE)

############## variabel PCA 1 ##################
ts_data1 <- ts(df$x1, start = c(2010,2), frequency = 12)
plot(ts_data1)

# splitting
train1 <- window(ts_data1, end = c(2022,5))
test1 <- window(ts_data1, start = c(2022,6)) #te_index

# scaling input
train1_mean <- mean(train1)
train1_sd   <- sd(train1)
train1_scaled <- (train1 - train_mean) / train_sd

# fit tanpa scaling
results1 <- data.frame()
save1 <- list()
best.mae <- Inf
for (p in 1:20) {
  for (k in 1:20) {
    set.seed(123)
    model <- nnetar(train1_scaled, p = p,size = k,repeats = 20)
    forecast_nn <- forecast(model, h = 36)
    pred_scaled <- as.numeric(forecast_nn$mean)
    pred <- pred_scaled * train1_sd + train1_mean
    rmse <- sqrt(mean((test1 - pred[1:length(test1)])^2))
    mae  <- mean(abs(test1 - pred[1:length(test1)]))
    results1 <- rbind( results1, data.frame(lag = p,
                                            neurons = k,
                                            RMSE = rmse,
                                            MAE = mae))
    if (mae < best.mae ) {
      best.mae = mae
      save1 = list(prediksi = pred,
                   best.model=model,
                   mean = train1_mean,
                   sd = train1_sd)
    }
    
  }
}

#evaluasi terbaik
results1[order(results1$MAE), ]
pred.fci1 <- ts(save1$prediksi,start = c(2022,6), frequency = 12 )

#visuaslisasi
autoplot(test1)+
  autolayer(pred.fci1,series = "Ramalan") +
  ggtitle("FCI 1 NNAR(2,3) Forecast")

#simpen data,
forecast <- list()
forecast[[1]] <- pred.fci1

# peramalan jangka pendek, h=3
set.seed(123)
mod3.x1 <- nnetar(ts_data1, p = 2,size = 3,repeats = 20)
fore3.x1 <- forecast(mod3.x1, h = 3)
as.numeric(fore3.x1$mean)

short.3 <- list()
short.3[[1]] <- as.numeric(fore3.x1$mean)

######################## variabel PCA 2 ###############################
ts_data2 <- ts(df$x2,
               start = c(2010,2),
               frequency = 12)
plot(ts_data2)

# splitting
train2 <- window(ts_data2, end = c(2022,5))
test2 <- window(ts_data2, start = c(2022,6))

# fit tanpa scaling
results2 <- data.frame()
save2 <- list()
best.mae <- Inf
for (p in 1:20) {
  for (k in 1:20) {
    set.seed(123)
    model <- nnetar(train2, p = p,size = k,repeats = 20)
    forecast_nn <- forecast(model, h = 36)
    pred <- as.numeric(forecast_nn$mean)
    rmse <- sqrt(mean((test2 - pred[1:length(test2)])^2))
    mae  <- mean(abs(test2 - pred[1:length(test2)]))
    results2 <- rbind( results2, data.frame(lag = p,
                                            neurons = k,
                                            RMSE = rmse,
                                            MAE = mae))
    if (mae < best.mae ) {
      best.mae = mae
      save2 = list(prediksi = pred,
                   best.model=model)
    }
  }
}

# evaluasi terbaik
results2[order(results2$MAE), ]
pred.fci2 <- ts(save2$prediksi,start = c(2022,6), frequency = 12 )

# visuaslisasi
autoplot(test2)+
  autolayer(pred.fci2,series = "Ramalan") +
  ggtitle("FCI 2 NNAR(4,9) Forecast")

# save tempat di nomer 2
forecast[[2]] <- pred.fci2

# peramalan jangka pendek, h=3
set.seed(123)
mod3.x2 <- nnetar(ts_data2, p = 4,size = 9,repeats = 20)
fore3.x2 <- forecast(mod3.x2, h = 3)
as.numeric(fore3.x2$mean)
short.3[[2]] <- as.numeric(fore3.x2$mean)

############## variabel FSI ##################
# ARIMA -> kurang bagus RMSE
require(tseries)
require(fpp2)
arima_m = auto.arima(train3)
summary(arima_m)
forecast(arima_m,36)
plot(forecast(arima_m,36))

# NNAR
ts_data3 <- ts(df$x3,start = c(2010,2),frequency = 12)
plot(ts_data3)

# splitting
train3 <- window(ts_data3, end = c(2022,5))
test3 <- window(ts_data3, start = c(2022,6))

# fit tanpa scaling
results3 <- data.frame()
save3 <- list()
best.mae <- Inf
for (p in 1:20) {
  for (k in 1:20) {
    set.seed(123)
    model <- nnetar(train3, p = p,size = k,repeats = 20)
    forecast_nn <- forecast(model, h = 36)
    pred <- as.numeric(forecast_nn$mean)
    rmse <- sqrt(mean((test3 - pred[1:length(test3)])^2))
    mae  <- mean(abs(test3 - pred[1:length(test3)]))
    results3 <- rbind( results3, data.frame(lag = p,
                                            neurons = k,
                                            RMSE = rmse,
                                            MAE = mae))
    if (mae < best.mae ) {
      best.mae = mae
      save3 = list(prediksi = pred,
                   best.model=model)
    }
  }
}

#evaluasi terbaik
results3[order(results3$MAE), ]
pred.fsi <- ts(save3$prediksi,start = c(2022,6), frequency = 12 )

#visuaslisasi
autoplot(test3)+
  autolayer(pred.fsi,series = "Ramalan") +
  ggtitle("FSI NNAR(6,7) Forecast")

# save tempat di nomer3
forecast[[3]] <- pred.fsi

# peramalan jangka pendek, h=3
set.seed(123)
mod3.x3 <- nnetar(ts_data3, p = 6,size = 7,repeats = 20)
fore3.x3 <- forecast(mod3.x3, h = 3)
as.numeric(fore3.x3$mean)
short.3[[3]] <- as.numeric(fore3.x3$mean)

# simpan hasil
ekso_fc <- data.frame(
  x1.fc = as.matrix(forecast[[1]]),
  x2.fc = as.matrix(forecast[[2]]),
  x3.fc = as.matrix(forecast[[3]])
)

ekso_short3 <- data.frame(
  x1 = as.matrix(short.3[[1]]),
  x2 = as.matrix(short.3[[2]]),
  x3 = as.matrix(short.3[[3]])
)

write_xlsx(ekso_fc, path = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/fc_eksogen(3).xlsx")
write_xlsx(ekso_short3, path = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/ekso_short3(2).xlsx")
