# Visualization 
# REKONSILIASI VISUAL
varx.y1.train
varx.test
varx.ramal.h

qvarx.train
qvarx.test
qvarx.ramal

moqrnn
moqrnn.fore
moqrnn.ramal

# kuantil 50
# y1
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Actual"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y1.aktual, color = "Actual"),linewidth = 0.8)+
  geom_line(aes(y = y1.prediksi, color = "VARX"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y1.prediksi, color = "VARX"),linewidth = 0.8)+
  geom_line(data = varx.ramal.h, aes(x = time, y = y1.ramal, color = "VARX"),linewidth = 0.8)+
  geom_line(data = qvarx.train, aes(x = time, y = y1.prediksi.50, color = "QVARX 50%"),linewidth = 0.8)+
  geom_line(data = qvarx.test, aes(x = time, y = y1.prediksi.50, color = "QVARX 50%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y1.prediksi.50, color = "QVARX 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn, aes(x = time, y = y1.prediksi50, color = "MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y1.prediksi50, color = "MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y1.prediksi.50, color = "MO-QRNN 50%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Actual" = "black",
               "VARX" = "blue",
               "QVARX 50%" = 'red',
               "MO-QRNN 50%"='green')
  ) +
  labs(
    x = "Year",
    y = "Inflation of Housing",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")+
  geom_vline(xintercept = c(as.Date("2022-06-01"),as.Date("2025-06-01")), 
             color = "red", linewidth = 0.8,linetype = "dashed")+
  coord_cartesian(ylim = c(-10, 10), xlim = c(as.Date("2010-01-01"),as.Date("2015-06-01")))

#y2
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y2.aktual, color = "Aktual"),linewidth = 0.8)+
  geom_line(aes(y = y2.prediksi, color = "VARX"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y2.prediksi, color = "VARX"),linewidth = 0.8)+
  geom_line(data = varx.ramal.h, aes(x = time, y = y2.ramal, color = "VARX"),linewidth = 0.8)+
  geom_line(data = qvarx.train, aes(x = time, y = y2.prediksi.50, color = "QVARX 50%"),linewidth = 0.8)+
  geom_line(data = qvarx.test, aes(x = time, y = y2.prediksi.50, color = "QVARX 50%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y2.prediksi.50, color = "QVARX 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn, aes(x = time, y = y2.prediksi50, color = "MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y2.prediksi50, color = "MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.50, color = "MO-QRNN 50%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "VARX" = "blue",
               "QVARX 50%" = 'red',
               "MO-QRNN 50%"='green')
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Pendidikan",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom")+
  geom_vline(xintercept = c(as.Date("2022-06-01"),as.Date("2025-06-01")), 
             color = "red", linewidth = 0.8,linetype = "dashed")+
  coord_cartesian(ylim = c(-5, 10), xlim = c(as.Date("2022-05-01"),as.Date("2025-9-01")))

# kuantil 5% 
# y1
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y1.aktual, color = "Aktual"),linewidth = 0.8)+
  geom_line(data = qvarx.train, aes(x = time, y = y1.prediksi.05, color = "QVARX 5%"),linewidth = 0.8)+
  geom_line(data = qvarx.test, aes(x = time, y = y1.prediksi.05, color = "QVARX 5%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y1.prediksi.05, color = "QVARX 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn, aes(x = time, y = y1.prediksi05, color = "MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y1.prediksi05, color = "MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y1.prediksi.05, color = "MO-QRNN 5%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 5%" = 'red',
               "MO-QRNN 5%"='green')
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Perumahan",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")+
  geom_vline(xintercept = c(as.Date("2022-06-01"),as.Date("2025-06-01")), 
             color = "red", linewidth = 0.8,linetype = "dashed")+
  coord_cartesian(ylim = c(-2.5, 3), xlim = c(as.Date("2010-02-01"),as.Date("2022-12-01")))

#y2
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y2.aktual, color = "Aktual"),linewidth = 0.8)+
  geom_line(data = qvarx.train, aes(x = time, y = y2.prediksi.05, color = "QVARX 5%"),linewidth = 0.8)+
  geom_line(data = qvarx.test, aes(x = time, y = y2.prediksi.05, color = "QVARX 5%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y2.prediksi.05, color = "QVARX 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn, aes(x = time, y = y2.prediksi05, color = "MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y2.prediksi05, color = "MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.05, color = "MO-QRNN 5%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 5%" = 'red',
               "MO-QRNN 5%"='green')
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Pendidikan",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom")+
  geom_vline(xintercept = c(as.Date("2022-06-01"),as.Date("2025-06-01")), 
             color = "red", linewidth = 0.8,linetype = "dashed")+
  coord_cartesian(ylim = c(-5, 10), xlim = c(as.Date("2022-02-01"),as.Date("2025-9-01")))

# qvss 0,05 in-sample
sc05 <- qvss(y=tr[,2:3], ypred_ref = qvarx.train[,c(3,7)] , ypred_for = moqrnn[,c(3,7)],
             tau=0.05, eps= min(eps.seq), W1 = bobot05[[1]], W2= bobot05[[2]], penalty = 0.3, 
             penalty2=0, unpenalized=NULL);sc05
# qvss 0,05 out-sample
sc05.out <- qvss(y=te[,2:3], ypred_ref = qvarx.test[,c(3,7)] , ypred_for = moqrnn.fore[,c(3,7)],
                 tau=0.05, eps= min(eps.seq), W1 = bobot05[[1]], W2= bobot05[[2]], penalty = 0.3, 
                 penalty2=0, unpenalized=NULL);sc05.out

# kuantil 95% 
# y1
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y1.aktual, color = "Aktual"),linewidth = 0.8)+
  geom_line(data = qvarx.train, aes(x = time, y = y1.prediksi.95, color = "QVARX 95%"),linewidth = 0.8)+
  geom_line(data = qvarx.test, aes(x = time, y = y1.prediksi.95, color = "QVARX 95%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y1.prediksi.95, color = "QVARX 95%"),linewidth = 0.8)+
  geom_line(data = moqrnn, aes(x = time, y = y1.prediksi95, color = "MO-QRNN 95%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y1.prediksi95, color = "MO-QRNN 95%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y1.prediksi.95, color = "MO-QRNN 95%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 95%" = 'red',
               "MO-QRNN 95%"='green')
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Perumahan",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")+
  geom_vline(xintercept = c(as.Date("2022-06-01"),as.Date("2025-06-01")), 
             color = "red", linewidth = 0.8,linetype = "dashed")+
  coord_cartesian(ylim = c(-2.5, 5), xlim = c(as.Date("2010-02-01"),as.Date("2022-12-01")))

#y2
ggplot(varx.y1.train, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(data = varx.test, aes(x = time, y = y2.aktual, color = "Aktual"),linewidth = 0.8)+
  geom_line(data = qvarx.train, aes(x = time, y = y2.prediksi.95, color = "QVARX 95%"),linewidth = 0.8)+
  geom_line(data = qvarx.test, aes(x = time, y = y2.prediksi.95, color = "QVARX 95%"),linewidth = 0.8)+
  geom_line(data = qvarx.ramal, aes(x = time, y = y2.prediksi.95, color = "QVARX 95%"),linewidth = 0.8)+
  geom_line(data = moqrnn, aes(x = time, y = y2.prediksi95, color = "MO-QRNN 95%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y2.prediksi95, color = "MO-QRNN 95%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.95, color = "MO-QRNN 95%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "QVARX 95%" = 'red',
               "MO-QRNN 95%"='green')
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi Pendidikan",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom")+
  geom_vline(xintercept = c(as.Date("2022-06-01"),as.Date("2025-06-01")), 
             color = "red", linewidth = 0.8,linetype = "dashed")+
  coord_cartesian(ylim = c(-10, 10), xlim = c(as.Date("2010-01-01"),as.Date("2022-12-01")))

# qvss 0,95 in sample
sc95 <- qvss(y=tr[,2:3], ypred_ref = qvarx.train[,c(5,9)] , ypred_for = moqrnn[,c(5,9)],
             tau=0.95, eps= min(eps.seq), W1 = bobot95[[1]], W2= bobot95[[2]], penalty = 0.3, 
             penalty2=0, unpenalized=NULL);sc95
# qvss 0,05 out-sample
sc95.out <- qvss(y=te[,2:3], ypred_ref = qvarx.test[,c(5,9)] , ypred_for = moqrnn.fore[,c(5,9)],
                 tau=0.95, eps= min(eps.seq), W1 = bobot95[[1]], W2= bobot95[[2]], penalty = 0.3, 
                 penalty2=0, unpenalized=NULL);sc95.out


# running code below individually
# plot data training dan testing
moqrnn
moqrnn.fore.old
moqrnn.fore

#y1
ggplot(moqrnn, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi05, color = "Prediction MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi50, color = "Prediction MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi95, color = "Prediction MO-QRNN 95%"), linewidth = 0.8) +
  geom_line(data = moqrnn.fore, aes(x = time, y = y1.prediksi05, color = "Forecast MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y1.prediksi50, color = "Forecast MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.fore, aes(x = time, y = y1.prediksi95, color = "Forecast MO-QRNN 95%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Actual" = "black",
               "Prediction MO-QRNN 5%" = "red",
               "Prediction MO-QRNN 50%" = "blue",
               "Prediction MO-QRNN 95%" = "green",
               "Forecast MO-QRNN 5%"= 'pink',
               "Forecast MO-QRNN 50%" = 'cyan',
               "Forecast MO-QRNN 95%" = 'darkolivegreen')
  ) +
  labs(
    x = "Year",
    y = "Inflation of Estate",
    color = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

#y2
ggplot(moqrnn.fore, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi05, color = "Prediksi MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi50, color = "Prediksi MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi95, color = "Prediksi MO-QRNN 95%"), linewidth = 0.8) +
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.05, color = "Ramalan MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.50, color = "Ramalan MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.95, color = "Ramalan MO-QRNN 95%"),linewidth = 0.8)+
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi MO-QRNN 5%" = "red",
               "Prediksi MO-QRNN 50%" = "blue",
               "Prediksi MO-QRNN 95%" = "green",
               "Ramalan MO-QRNN 5%"= 'pink',
               "Ramalan MO-QRNN 50%" = 'cyan',
               "Ramalan MO-QRNN 95%" = 'darkolivegreen')
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
