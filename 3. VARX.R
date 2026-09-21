library(MTS)


### call data ### -> differencing x1,x2,x3
dfr <- data.frame(
  time=target$date[-c(1:2)],
  y1=kel$`Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga`[-1],
  y2=kel$`Pendidikan, Rekreasi, dan Olahraga`[-1],
  x1=diff(fci_pca$fci1),
  x2=diff(fci_pca$fci2),
  x3=diff(FSI$fsi)
)

### 1. check stationerity using adf 
adf.test(dfr$y1)
adf.test(dfr$y2)
adf.test(dfr$x1)
adf.test(dfr$x2)
adf.test(dfr$x3)

### 3. splitting data ###
tr_index <- ceiling(0.80*nrow(dfr))
te_index <- nrow(dfr)-tr_index
tr <- dfr[1:tr_index,]
te <- dfr[(tr_index+1):nrow(dfr),]
View(dfp)

### 4. check lag optimum using CIC / AIC ###
VARselect(tr[-1],lag=10, type = "both") #terbaiknya adalah lag 2

### 5. building model ### using vars package
mod.dfr <- VAR(tr[-1],p=2)
summary(mod.dfr)

# breakdown untuk granger check
mod1 <- vars::VAR(tr[-c(1,5:6)],p=2,type = "const")
mod2 <- vars::VAR(tr[-c(1,4,6)],p=2,type = "const")
mod3 <- vars::VAR(tr[-c(1,4:5)],p=2,type = "const")

### 6. granger causality check ### untuk x1 x2 x3 into the system
causality(mod1, cause = "x1") # tolak H0, bahwa x1 memberi pengaruh kpd sistem
causality(mod2, cause = "x2") # gagal tolak H0, bahwa x2 tdk memberi pengaruh kpd sistem
causality(mod3, cause = "x3") # tolak H0, bahwa x3 memberi pengaruh kpd sistem

### 6. granger check pairwise ### kebalikan: showing how y tidak memprediksi x
# x1 to y1 dan y2 (vice versa)
grangertest(x1 ~ y1, order=2, data=tr)
grangertest(x1 ~ y2, order=2, data=tr)

# x2 to y1 dan y2 (vice versa)
grangertest(x2 ~ y1, order=2, data=tr)
grangertest(x2 ~ y2, order=2, data=tr)

# x3 to y1 dan y2 (vice versa)
grangertest(x3 ~ y1, order=2, data=tr)
grangertest(x3 ~ y2, order=2, data=tr)

### 6. granger check endogeneous ### 
grangertest(y1 ~ y2, order=1, data=tr)
grangertest(y2 ~ y1, order=1, data=tr)

grangertest(y1 ~ y2, order=2, data=tr)
grangertest(y2 ~ y1, order=2, data=tr)

### 6. granger test untuk tabel word ###
hasil.l1 <- list()
hasil.l2 <- list()
vb <- colnames(tr[,-1])
for (i in vb) {
  for (j in vb) {
    if (i != j) {
      test.1 <- grangertest(
        as.formula(paste(i, "~", j)),
        order = 1,
        data = tr[,-1]
      )
      hasil.l1[[paste(i, "caused by", j)]] <- test.1
      
      test.2 <- grangertest(
        as.formula(paste(i, "~", j)),
        order = 2,
        data = tr[,-1]
      )
      hasil.l2[[paste(i, "caused by", j)]] <- test.2
    }
  }
}
hasil.l1
hasil.l2

### 7. define model VARX (tanpa restricted mat) ### MTS package
mo_VARX<-VARX(zt=tr[,c(2,3)], p=2, xt=tr[,c(4,5,6)],m=0, include.mean = 0, fixed = NULL)
varx_model <- vars::VAR(tr[,c(2,3)], p = 2, type = "none", exogen = tr[,c(4,5,6)])

### 8. prediksi in sample ### 
dfp <- as.matrix(cbind(y1l1=lag(dfr$y1,1),
                       y2l1=lag(dfr$y2,1),
                       y1l2=lag(dfr$y1,2),
                       y2l2=lag(dfr$y2,2),
                       x1=dfr$x1,
                       x2=dfr$x2,
                       x3=dfr$x3
))
nrow(pred_VARX)
trp <- as.matrix(dfp[1:tr_index,]) #1:148
tep <- as.matrix(dfp[(tr_index+1):nrow(dfp),]) #149:184 (36)

pred_VARX <- matrix(NA, nrow= nrow(trp), ncol = 2)
for (i in 1:ncol(mo_VARX$coef)) {
  pred_VARX[,i] <- trp %*% mo_VARX$coef[,i]
}
print(mo_VARX$coef, round(4))

## visualisasi
varx.y1.train <- data.frame(
  time=tr[,1],
  y1.aktual=tr[,2],
  y1.prediksi=pred_VARX[,1],
  y2.aktual=tr[,3],
  y2.prediksi=pred_VARX[,2]
)

nrow(pred_VARX)
#y1
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi, color = "Prediksi"), linewidth = 0.8) +
  
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi" = "blue")
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
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi, color = "Prediksi"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi" = "blue")
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

### 9. evaluasi smape in-sample
smape.100(actual=mo_VARX$data[-c(1,2),1], predicted = pred_VARX[-c(1,2),1])
smape.100(actual=mo_VARX$data[-c(1,2),2], predicted = pred_VARX[-c(1,2),2])

## lanjut forecast variable eksogen (3b)
ekso_fc <- read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/fc_eksogen(2).xlsx")
View(ekso_fc)

## 10. forecast out-sample
pred_VARX_out <- matrix(NA, nrow= nrow(tep), ncol = 2)
for (i in 1:ncol(mo_VARX$coef)) {
  pred_VARX_out[,i] <- as.matrix(cbind(tep[,c(1:4)],ekso_fc[1:nrow(tep),])) %*% mo_VARX$coef[,i]
}

### 11. evaluasi smape out-sample (0-2)
smape.100(actual=te[,2], predicted = pred_VARX_out[,1])
smape.100(actual=te[,3], predicted = pred_VARX_out[,2])

### visualisasi
varx.test <- data.frame(
  time=c(tr[nrow(tr),1],te[,1]),
  y1.aktual=c(tr[nrow(tr),2],te[,2]),
  y1.prediksi=c(pred_VARX[nrow(pred_VARX),1], pred_VARX_out[,1]),
  y2.aktual=c(tr[nrow(tr),3],te[,3]),
  y2.prediksi=c(pred_VARX[nrow(pred_VARX),2], pred_VARX_out[,2]))

#y1
ggplot(varx.test, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi, color = "Prediksi"), linewidth = 0.8) +
  geom_line(data = varx.ramal.h, aes(x = time, y = y1.ramal, color = "Prediksi 6 bulan"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi" = "blue",
               "Prediksi 6 bulan" = 'cyan')
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
ggplot(varx.test, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi, color = "Prediksi"), linewidth = 0.8)+
  geom_line(data = varx.ramal.h, aes(x = time, y = y2.ramal, color = "Prediksi 6 bulan"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi" = "blue",
               "Prediksi 6 bulan" = 'cyan')
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

## 10. forecast 3 horizon diluar out-sample
mo_VARX_3<-VARX(zt=dfr[,c(2,3)], p=2, xt=dfr[,c(4,5,6)],m=0, include.mean = 0, fixed = NULL)
prediksi <- fore_VARX(mo_VARX_3, newxt = ekso_short3, hstep = 6, orig = 0) 

### visualisasI
varx.ramal.h <- data.frame(
  time=seq.Date(from = as.Date("2025/6/1"), to= as.Date("2025/9/1"), by="month"),
  y1.ramal=c(pred_VARX_out[nrow(pred_VARX_out),1],prediksi$pred[,1]),
  y2.ramal=c(pred_VARX_out[nrow(pred_VARX_out),2],prediksi$pred[,2])
)

# save the data, if you want
write_xlsx(list("in" = as.data.frame(pred_VARX),
                "out" = as.data.frame(pred_VARX_out),
                "ramal" = as.data.frame(prediksi$pred)), 
           path = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/varx.xlsx")
