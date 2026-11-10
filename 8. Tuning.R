# TUNING
quantile_loss <- function(y, yhat, tau) {
  e <- y - yhat
  mean((tau - (e < 0)) * e)
}

smape.100 <- function(actual, predicted){
  if(length(actual) != length(predicted)){
    stop("actual and predicted must have the same length")
  }
  smape_value <- mean( abs(predicted - actual) / (abs(predicted) + abs(actual)) )
  return(smape_value)
}

tune_qrnn <- function(x, y,
                      hidden.range = 7:8,
                      penalty.range = c(0, 0.5, 0.1),
                      tau = 0.05,
                      eps.seq = 10^seq(-2, -4, length.out = 5),
                      n.trials = 10,
                      init.range = c(-0.1,0.5,-0.1,0.5),
                      scale.y=FALSE) {
  
  results <- data.frame()
  best.model <- list()
  best.smape <- Inf
  best.c <- Inf
  
  if (scale.y) {
    y <- scale(y)
  } else {
    attr(y, "scaled:center") <- rep(0, ncol(y))
    attr(y, "scaled:scale") <- rep(1, ncol(y))
  }
  y.center <- attr(y, "scaled:center")
  y.scale <- attr(y, "scaled:scale")
  
  w <- matrix(1/nrow(y), nrow(y), ncol(y))
  
  for (h in hidden.range) {
    
    for (pen in penalty.range) {
      cat("Hidden:", h,
          " Penalty:", pen, "\n")
      cost.best <- Inf
      weights.best <- NULL
      
      # multiple random initializations
      for (i in 1:n.trials) {
        wg <- qrnn.initialize(x = x, y = y, n.hidden = h,init.range = init.range)
        fit <- NULL
        
        # smoothing optimization
        for (eps in eps.seq) {
          fit <- suppressWarnings(
            nlm(qrnn.cost,wg, x = x,y = y,n.hidden = h, w = w,tau = tau,
                lower = -Inf,eps = eps, Th = Th,Th.prime = Th.prime,
                penalty = pen,unpenalized = NULL, iterlim = 5000,gradtol = 1e-4,
                check.analyticals = FALSE, monotone = NULL,additive = FALSE
            )
          )
          wg <- fit$estimate
        }
        if (fit$minimum < cost.best) {
          cost.best <- fit$minimum
          weights.best <- fit$estimate
        }
      }
      
      # prediction in-sample
      weights <- qrnn.reshape(x=x, y=y, weights=weights.best, n.hidden=h)
      
      pred.in <- matrix(0, ncol = length(weights), nrow = nrow(x))
      pred.in<- qrnn.eval(x, weights$W1, weights$W2, lower=-Inf, monotone=NULL, 
                          additive=FALSE, eps=3.906250e-03, Th=Th)
      for (j in 1:ncol(pred.in)) {
        pred.in[, j] <- pred.in[, j] * y.scale[j] + y.center[j]
      }
      
      # calculate quantile loss
      val.loss <- quantile_loss(y, pred.in, tau)
      E.cost <- tilted.approx(x=y-pred.in, tau, min(eps.seq)) 
      
      # calculate smape
      if (tau == 0.5) {
        smapey <- matrix(0, ncol = length(pred.in), nrow = 1)
        for (j in 1:ncol(pred.in)) {
          smapey[,j] <- smape.100(y[,j], pred.in[, j])
        }
        sm.mean <- mean(c(smapey[,1],smapey[,2]))
      }
      
      results <- rbind(
        results,
        data.frame(
          hidden = h,
          penalty = pen,
          train.cost = cost.best,
          validation.loss = val.loss
        )
      )
      
      if (cost.best < best.c) {
        best.c <- cost.best
        best.model <- list(
          hidden = h,
          penalty = pen,
          weights = weights,
          prediksi = pred.in,
          train.cost = cost.best,
          validation.loss = val.loss
        )
      }
      
    }
  }
  return(
    list(
      results = results,
      best.model = best.model
    )
  )
}

############################### coba ##########################################
simulasi <- tune_qrnn(x=x, y=y,
                      hidden.range = 14,
                      penalty.range = 0.3,
                      tau = 0.95,
                      eps.seq = 10^seq(-2, -4, length.out = 5),
                      n.trials = 10,
                      init.range = c(-0.1,0.5,-0.1,0.5),
                      scale.y=FALSE)
simulasi$results
simulasi$best.model
write.xlsx(list("konfig"= simulasi$results,
                "y_pred" = simulasi$best.model$prediksi,
                "w1"= simulasi$best.model$weights$W1,
                "w2"= simulasi$best.model$weights$W2),
           file = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn95(7).xlsx")

## bobot baru
bobot95 <- list()
bobot05[[1]] <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn05(7).xlsx", sheet = "w1"))
bobot05[[2]] <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn05(7).xlsx", sheet = "w2"))
bobot50[[1]] <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn50(7).xlsx", sheet = "w1"))
bobot50[[2]] <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn50(7).xlsx", sheet = "w2"))
bobot95[[1]] <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn95(7).xlsx", sheet = "w1"))
bobot95[[2]] <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn95(7).xlsx", sheet = "w2"))

# y_pred
y_pred05 <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn05(7).xlsx", sheet = "y_pred"))
y_pred50 <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn50(7).xlsx", sheet = "y_pred"))
y_pred95 <- as.matrix(read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn95(7).xlsx", sheet = "y_pred"))

# visualisasi prediksi
moqrnn <- data.frame(
  time=tr[,1],
  y1.aktual=tr[,2],
  y1.prediksi05=y_pred05[,1],
  y1.prediksi50=y_pred50[,1],
  y1.prediksi95=y_pred95[,1],
  y2.aktual=tr[,3],
  y2.prediksi05=y_pred05[,2],
  y2.prediksi50=y_pred50[,2],
  y2.prediksi95=y_pred95[,2]
)

#y1
ggplot(moqrnn, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi05, color = "Prediksi MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi50, color = "Prediksi MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi95, color = "Prediksi MO-QRNN 95%"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi MO-QRNN 5%" = "red",
               "Prediksi MO-QRNN 50%" = "blue",
               "Prediksi MO-QRNN 95%" = "green")
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
ggplot(moqrnn, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi05, color = "Prediksi MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi50, color = "Prediksi MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi95, color = "Prediksi MO-QRNN 95%"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi MO-QRNN 5%" = "red",
               "Prediksi MO-QRNN 50%" = "blue",
               "Prediksi MO-QRNN 95%" = "green")
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
# evaluasi
smape.100(moqrnn$y1.aktual,moqrnn$y1.prediksi50)
smape.100(moqrnn$y2.aktual,moqrnn$y2.prediksi50)


################################### OUT-SAMPLE ###################################
ekso_fc <- read.xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/fc_eksogen(2).xlsx")
xtt=as.matrix(cbind(xt[,c(1:4)],ekso_fc))

fore05<- qrnn.eval(xtt, bobot05[[1]], bobot05[[2]], lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
fore50<- qrnn.eval(xtt, bobot50[[1]], bobot50[[2]], lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
fore95<- qrnn.eval(xtt, bobot95[[1]], bobot95[[2]], lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=Th)

fore.out <- list(
  "05"=fore05,
  "50"=fore50,
  "95"=fore95
)
write.xlsx(fore.out,
           file = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn-out(7).xlsx")
shoot <- read.xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn-out(5).xlsx", sheet="50")

# smape un lv 0,5
smape.100(actual=te[,2], predicted = fore50[,1])
smape.100(actual=te[,3],predicted = fore50[,2])

# evaluasi loss (y-ypred) 
w1<- matrix(1/nrow(yt), nrow(yt), ncol(yt))
loss05 <- sum(w1* tilted.approx(yt-fore05, tau=0.05, eps=min(eps.seq)));loss05
loss95 <- sum(w1* tilted.approx(yt-fore05, tau=0.95, eps=min(eps.seq)));loss95

## visualisasi out-sample
moqrnn.fore <- data.frame(
  time=te[,1],
  y1.aktual=te[,2],
  y1.prediksi05=fore05[,1],
  y1.prediksi50=fore50[,1],
  y1.prediksi95=fore95[,1],
  y2.aktual=te[,3],
  y2.prediksi05=fore05[,2],
  y2.prediksi50=fore50[,2],
  y2.prediksi95=fore95[,2]
)
moqrnn.fore.old <- moqrnn.fore

## y1
ggplot(moqrnn.fore, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi05, color = "Prediksi MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi50, color = "Prediksi MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi95, color = "Prediksi MO-QRNN 95%"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi MO-QRNN 5%" = "red",
               "Prediksi MO-QRNN 50%" = "blue",
               "Prediksi MO-QRNN 95%" = "green")
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

## y2
ggplot(moqrnn.fore, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi05, color = "Prediksi MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi50, color = "Prediksi MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi95, color = "Prediksi MO-QRNN 95%"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi MO-QRNN 5%" = "red",
               "Prediksi MO-QRNN 50%" = "blue",
               "Prediksi MO-QRNN 95%" = "green")
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

################################### ramalan ###################################
# model
# data dfr dan dfp memuat NAs pada 2 baris atas 
nrow(as.matrix(dfr[-c(1:2),2:3])); nrow(dfp[-c(1:2),])

ramalz <- tune_qrnn(x=as.matrix(dfp[-c(1:2),]), y=as.matrix(dfr[-c(1:2),2:3]),
                    hidden.range = 14,
                    penalty.range = 0.3,
                    tau = 0.5,
                    eps.seq = 10^seq(-2, -4, length.out = 5),
                    n.trials = 10,
                    init.range = c(-0.1,0.5,-0.1,0.5),
                    scale.y=FALSE)
ramalz$results
ramalz$best.model

#ramalz05
bobot3mo.05 <- list()
bobot3mo.05[[1]] <- ramalz05$best.model$weights$W1
bobot3mo.05[[2]] <- ramalz05$best.model$weights$W2

#ramalz
bobot3mo.50 <- list()
bobot3mo.50[[1]] <- ramalz$best.model$weights$W1
bobot3mo.50[[2]] <- ramalz$best.model$weights$W2

#ramalz95
bobot3mo.95 <- list()
bobot3mo.95[[1]] <- ramalz95$best.model$weights$W1
bobot3mo.95[[2]] <- ramalz95$best.model$weights$W2

# call ekso_short3
xttt = cbind(tail(dfr[,2:3],2),ekso_short3[1,])
cbind(t(as.numeric(c(as.vector(t(tail(dfr[,2:3],2))),ekso_short3[1,]))),1) %*% bobot3mo.05[[1]]

# ramal 3 bulan
rm05<- recursive_moqrnn(W=bobot3mo.05, df=dfr, te.new=ekso_short3, p=2,lower=-Inf, 
                        monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
rm50<- recursive_moqrnn(W=bobot3mo.50, df=dfr, te.new=ekso_short3, p=2,lower=-Inf, 
                        monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
rm95<- recursive_moqrnn(W=bobot3mo.95, df=dfr, te.new=ekso_short3, p=2,lower=-Inf, 
                        monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)

write.xlsx(list("05"=as.data.frame(rm05),
                "50"=as.data.frame(rm50),
                "95"=as.data.frame(rm95)),
           file = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/bobot/moqrnn-ramal(7)new.xlsx")

# visualisasi
moqrnn.ramal <- data.frame(
  time=seq.Date(from = as.Date("2025/6/1"), to= as.Date("2025/12/1"), by="month"),
  y1.prediksi.05=c(fore05[nrow(fore05),1],rm05[1:6,1]),
  y1.prediksi.50=c(fore50[nrow(fore50),1],rm50[1:6,1]),
  y1.prediksi.95=c(fore95[nrow(fore95),1],rm95[1:6,1]),
  y2.prediksi.05=c(fore05[nrow(fore05),2],rm05[1:6,2]),
  y2.prediksi.50=c(fore50[nrow(fore50),2],rm50[1:6,2]),
  y2.prediksi.95=c(fore95[nrow(fore95),2],rm95[1:6,2]))

# y1
ggplot(moqrnn.fore, aes(x = time)) +
  geom_line(aes(y = y1.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi05, color = "Prediksi MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi50, color = "Prediksi MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y1.prediksi95, color = "Prediksi MO-QRNN 95%"), linewidth = 0.8) +
  geom_line(data = moqrnn.ramal, aes(x = time, y = y1.prediksi.05, color = "Ramalan 6 bulan MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y1.prediksi.50, color = "Ramalan 6 bulan MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y1.prediksi.95, color = "Ramalan 6 bulan MO-QRNN 95%"),linewidth = 0.8)+
  
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi MO-QRNN 5%" = "red",
               "Prediksi MO-QRNN 50%" = "blue",
               "Prediksi MO-QRNN 95%" = "green",
               "Ramalan 6 bulan MO-QRNN 5%" = 'deeppink',
               "Ramalan 6 bulan MO-QRNN 50%" = 'cyan',
               "Ramalan 6 bulan MO-QRNN 95%" = 'seagreen')
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

# y2
ggplot(moqrnn.fore, aes(x = time)) +
  geom_line(aes(y = y2.aktual, color = "Aktual"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi05, color = "Prediksi MO-QRNN 5%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi50, color = "Prediksi MO-QRNN 50%"), linewidth = 0.8) +
  geom_line(aes(y = y2.prediksi95, color = "Prediksi MO-QRNN 95%"), linewidth = 0.8)  +
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.05, color = "Ramalan 6 bulan MO-QRNN 5%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.50, color = "Ramalan 6 bulan MO-QRNN 50%"),linewidth = 0.8)+
  geom_line(data = moqrnn.ramal, aes(x = time, y = y2.prediksi.95, color = "Ramalan 6 bulan MO-QRNN 95%"),linewidth = 0.8)+
  
  scale_color_manual(
    values = c("Aktual" = "black",
               "Prediksi MO-QRNN 5%" = "red",
               "Prediksi MO-QRNN 50%" = "blue",
               "Prediksi MO-QRNN 95%" = "green",
               "Ramalan 6 bulan MO-QRNN 5%" = 'deeppink',
               "Ramalan 6 bulan MO-QRNN 50%" = 'cyan',
               "Ramalan 6 bulan MO-QRNN 95%" = 'seagreen')
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

## RUNNING BELOW SECTION INDIVIDUALLY
tuning2 <- list()
tun <- tune_qrnn(x=x, y=y,
                 hidden.range = 14,
                 penalty.range = 0,
                 tau = 0.5,
                 eps.seq = 10^seq(-2, -4, length.out = 5),
                 n.trials = 10,
                 init.range = c(-0.1,0.5,-0.1,0.5),
                 scale.y=FALSE)
tuning2[[10]] <- tun
fore50<- qrnn.eval(xtt, tun$best.model$weights$W1, tun$best.model$weights$W2, 
                   lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)

# smape un lv 0,5
smape.100(actual=te[,2], predicted = fore50[,1])
smape.100(actual=te[,3],predicted = fore50[,2])
tun

tuning2[[3]]