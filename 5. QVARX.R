## inisiasi 
# gunakan dfr

library(quantreg)
tau1 <- 0.05
tau2 <- 0.5
tau3 <- 0.95
p <-2

restrict_QVARX <- matrix(c(1,1,
                           1,1,
                           1,1,
                           1,1,
                           1,1,
                           1,1,
                           1,1),
                         nrow=7, ncol=2, byrow=TRUE)

## modelling
mod_qvarx_05 <- QVARX(y=as.matrix(tr[,c(2,3)]), x_exog=as.matrix(tr[,c(4:6)]), 
                      xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau1))
mod_qvarx_50 <- QVARX(y=as.matrix(tr[,c(2,3)]), x_exog=as.matrix(tr[,c(4:6)]), 
                      xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau2))
mod_qvarx_95 <- QVARX(y=as.matrix(tr[,c(2,3)]), x_exog=as.matrix(tr[,c(4:6)]), 
                      xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau3))

## prediksi in sample
pred_qvarx_05 <- predict_QVARX(mod_qvarx_05, df=dfr, p=2)
pred_qvarx_50 <- predict_QVARX(mod_qvarx_50, df=dfr, p=2)
pred_qvarx_95 <- predict_QVARX(mod_qvarx_95, df=dfr, p=2)

## akurasi lv 0,5 (0-1)
smape.100(actual=tr[-c(1:2),2], predicted = pred_qvarx_50[,1])
smape.100(actual=tr[-c(1:2),3], predicted = pred_qvarx_50[,2])

## calibration -> empirical frequencies
coverage_multi <- function(Y, Q, tau) {
  CP <- colMeans(Y <= Q)
  CPE <- abs(CP - tau)
  data.frame(
    Variable = colnames(Y),
    Quantile = tau,
    Coverage_Probability = CP,
    Coverage_Error = CPE
  )
}

coverage_multi(Y = tr[-c(1:2),c(2:3)], Q = pred_qvarx_05, tau = 0.05)
coverage_multi(Y = tr[-c(1:2),c(2:3)], Q = pred_qvarx_50, tau = 0.50)
coverage_multi(Y = tr[-c(1:2),c(2:3)], Q = pred_qvarx_95, tau = 0.95)

## visualisasi in sample
qvarx.train <- data.frame(
  time=tr[-c(1:2),1],
  y1.aktual=tr[-c(1:2),2],
  y1.prediksi.05=pred_qvarx_05[,1],
  y1.prediksi.50=pred_qvarx_50[,1],
  y1.prediksi.95=pred_qvarx_95[,1],
  y2.aktual=tr[-c(1:2),3],
  y2.prediksi.05=pred_qvarx_05[,2],
  y2.prediksi.50=pred_qvarx_50[,2],
  y2.prediksi.95=pred_qvarx_95[,2])

#y1
ggplot(qvarx.train, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi.05, color = "QVARX 5%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi.50, color = "QVARX 50%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi.95, color = "QVARX 95%"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 5%" = "red",
               "QVARX 50%" = "blue",
               "QVARX 95%" = "green"
    )
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Perumahan",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

#y2
ggplot(qvarx.train, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi.05, color = "QVARX 5%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi.50, color = "QVARX 50%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi.95, color = "QVARX 95%"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 5%" = "red",
               "QVARX 50%" = "blue",
               "QVARX 95%" = "green"
    )
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Pendidikan",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

## prediksi out-sample
fore_qvarx_05 <- predict_QVARX_out(model=mod_qvarx_05, df=dfr, p=2, newxt=ekso_fc)
fore_qvarx_50 <- predict_QVARX_out(model=mod_qvarx_50, df=dfr, p=2, newxt=ekso_fc)
fore_qvarx_95 <- predict_QVARX_out(model=mod_qvarx_95, df=dfr, p=2, newxt=ekso_fc)

## akurasi lv 0,5
smape.100(actual=te[,2], predicted = fore_qvarx_50[,1])
smape.100(actual=te[,3], predicted = fore_qvarx_50[,2])

## calibration -> empirical frequencies
coverage_multi(Y = te[,c(2,3)], Q = fore_qvarx_05, tau = 0.05)
coverage_multi(Y = te[,c(2,3)], Q = fore_qvarx_50, tau = 0.50)
coverage_multi(Y = te[,c(2,3)], Q = fore_qvarx_95, tau = 0.95)

## visualisasi out-sample
qvarx.test <- data.frame(
  time=te[,1],
  y1.aktual=te[,2],
  y1.prediksi.05=fore_qvarx_05[,1],
  y1.prediksi.50=fore_qvarx_50[,1],
  y1.prediksi.95=fore_qvarx_95[,1],
  y2.aktual=te[,3],
  y2.prediksi.05=fore_qvarx_05[,2],
  y2.prediksi.50=fore_qvarx_50[,2],
  y2.prediksi.95=fore_qvarx_95[,2])

#y1
ggplot(qvarx.test, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi.05, color = "QVARX 5%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi.50, color = "QVARX 50%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi.95, color = "QVARX 95%"), linewidth = 0.8) +
  geom_line(data = qvarx.ramal, aes(x = time, y = y1.prediksi.05, color = "Ramalan 6 bulan QVARX 5%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y1.prediksi.50, color = "Ramalan 6 bulan QVARX 50%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y1.prediksi.95, color = "Ramalan 6 bulan QVARX 95%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 5%" = "red",
               "QVARX 50%" = "blue",
               "QVARX 95%" = "green",
               "Ramalan 6 bulan QVARX 5%" = 'deeppink',
               "Ramalan 6 bulan QVARX 50%" = 'cyan',
               "Ramalan 6 bulan QVARX 95%" = 'seagreen')
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Perumahan",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#y2
ggplot(qvarx.test, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi.05, color = "QVARX 5%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi.50, color = "QVARX 50%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi.95, color = "QVARX 95%"), linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y2.prediksi.05, color = "Ramalan 6 bulan QVARX 5%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y2.prediksi.50, color = "Ramalan 6 bulan QVARX 50%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y2.prediksi.95, color = "Ramalan 6 bulan QVARX 95%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 5%" = "red",
               "QVARX 50%" = "blue",
               "QVARX 95%" = "green",
               "Ramalan 6 bulan QVARX 5%" = 'deeppink',
               "Ramalan 6 bulan QVARX 50%" = 'cyan',
               "Ramalan 6 bulan QVARX 95%" = 'seagreen')
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Pendidikan",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
############################### ramal h horizon #############################
# modelling, estimasi pake data all
mo_qvarx_05 <- QVARX(y=as.matrix(dfr[,c(2,3)]), x_exog=as.matrix(dfr[,c(4:6)]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau1))
mo_qvarx_50 <- QVARX(y=as.matrix(dfr[,c(2,3)]), x_exog=as.matrix(dfr[,c(4:6)]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau2))
mo_qvarx_95 <- QVARX(y=as.matrix(dfr[,c(2,3)]), x_exog=as.matrix(dfr[,c(4:6)]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau3))

# ramal jangka pendek 6 bulan
ramal_05 <- forecast_QVARX(model=mo_qvarx_05, df=dfr, te.new=ekso_short3, p=2)
ramal_50 <- forecast_QVARX(model=mo_qvarx_50, df=dfr, te.new=ekso_short3, p=2)
ramal_95 <- forecast_QVARX(model=mo_qvarx_95, df=dfr, te.new=ekso_short3, p=2)

qvarx.ramal <- data.frame(
  time=seq.Date(from = as.Date("2025/6/1"), to= as.Date("2025/9/1"), by="month"),
  y1.prediksi.05=c(fore_qvarx_05[nrow(fore_qvarx_05),1],ramal_05[,1]),
  y1.prediksi.50=c(fore_qvarx_50[nrow(fore_qvarx_50),1],ramal_50[,1]),
  y1.prediksi.95=c(fore_qvarx_95[nrow(fore_qvarx_95),1],ramal_95[,1]),
  y2.prediksi.05=c(fore_qvarx_05[nrow(fore_qvarx_05),2],ramal_05[,2]),
  y2.prediksi.50=c(fore_qvarx_50[nrow(fore_qvarx_50),2],ramal_50[,2]),
  y2.prediksi.95=c(fore_qvarx_95[nrow(fore_qvarx_95),2],ramal_95[,2]))

############################### simpan hasil ###############################
write_xlsx(list("in05" = as.data.frame(pred_qvarx_05),
                "in50" = as.data.frame(pred_qvarx_50),
                "in95" = as.data.frame(pred_qvarx_95),
                "out05" = as.data.frame(fore_qvarx_05),
                "out50" = as.data.frame(fore_qvarx_50),
                "out95" = as.data.frame(fore_qvarx_95),
                "ramal05"= as.data.frame(ramal_05),
                "ramal50"= as.data.frame(ramal_50),
                "ramal95"= as.data.frame(ramal_95)), 
           path = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/varx/qvarx.xlsx")

