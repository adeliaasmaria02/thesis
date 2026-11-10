# simulation

library(quantreg)
library(tseries)
library(MASS)
library(ggplot2)
library(quantreg)

# simulation: menghapus 1000 data awal hasil generate
n = 200
out = 1000
N = n + out
t = seq(n)

# 1. bangkitkan var ekso
set.seed(123)
ek1 <- arima.sim(list(order = c(1, 0,0), ar=-.8), n=N); plot(ek1); adf.test(ek1)
ek2 <- arima.sim(list(order = c(1, 0,0), ar=.5), n=N); plot(ek2); adf.test(ek2)
ek3 <- arima.sim(list(order = c(1, 0,0), ar=.2), n=N); plot(ek3); adf.test(ek3)

# 2. VAR linear
X <- matrix(NA_real_, nrow = N, ncol = 2); colnames(X) <- c("x1", "x2")
X[1, ] <- c(0, 0)
X[2, ] <- c(0, 0)

for (t in 3:(N)) {
  x1_l1 <- X[t - 1, 1]; x1_l2 <- X[t-2, 1]
  x2_l1 <- X[t - 1, 2]; x2_l2 <- X[t-2, 2]
  X[t, 1] <-  0.3 * x1_l1 + 0.5 * x2_l1 + 0.6 * x1_l2 + 0.12 * x2_l2 + ex1[t] + ex2[t] + ex3[t] + U[t-2, "u1"]
  X[t, 2] <- -0.7 * x1_l1 + 0.8 * x2_l1 + 0.1 * x1_l2 - 0.3 * x2_l2 + ex1[t] + ex2[t] + ex3[t] + U[t-2, "u2"]
}

X <- X[(out + 1):N, ]   # drop burn-in
as.data.frame(X)
plot(t, X[,1], type="l")
lines(t, X[,2], col='blue')
m_koef <- c
eigenvalues <- eigen(m_koef)$values
max_magnitude <- max(abs(eigenvalues))

# 3. MESTAR nonlinear
Xn <- matrix(NA_real_, nrow = N, ncol = 2); colnames(X) <- c("x1", "x2")
Xn[1, ] <- c(0, 0)
Xn[2, ] <- c(0, 0)

for (t in 2:(N)) {
  Xn1_l1 <- Xn[t-1,1]
  Xn2_l1 <- Xn[t-1,2]
  
  Xn[t, 1] <- 6.5 * Xn1_l1 * exp(-0.25 * Xn1_l1^2) +3.5 * Xn2_l1 * exp(-0.45 * Xn2_l1^2)  + ex1[t] + ex2[t] + ex3[t] + U[t - 1, "u1"]
  Xn[t, 2] <- 4.3 * Xn1_l1 * exp(-0.15 * Xn1_l1^2) + 3.7 * Xn2_l1 * exp(-0.25 * Xn2_l1^2)  + ex1[t] + ex2[t] + ex3[t] + U[t - 1, "u2"]
  }

Xn <- Xn[(out + 1):N, ]   # drop burn-in
as.data.frame(Xn);t = 1:nrow(Xn)
plot(t, Xn[,1], type="l")
lines(t, Xn[,2], col='blue')

# 4. MIXED
Xm <- matrix(NA_real_, nrow = N, ncol = 2)
colnames(Xm) <- c("x1", "x2")
Xm[1, ] <- c(0.1,0.1)

for (t in 2:(N)) {
  Xm1_l1 <- Xm[t - 1, 1]
  Xm2_l2 <- Xm[t - 1, 2]
  
  Xm[t, 1] <- -0.8 * Xm1_l1  + 0.2*ex1[t] + 0.5*ex2[t] - 0.7*ex3[t] + U[t - 1, "u1"]
  Xm[t, 2] <- 2.9 * Xm1_l1 * exp(-0.15 * Xm1_l1^2) +3.1 * Xm2_l2 * exp(-0.25 * Xm2_l2^2)  + 0.2*ex1[t] + 0.5*ex2[t] - 0.7*ex3[t] + U[t - 1, "u2"]
}

Xm <- Xm[(out + 1):N, ]
as.data.frame(Xm)
plot(t, Xm[,1], type="l")
lines(t, Xm[,2], col='blue')


df_mixed <- data.frame(t = seq(1:length(Xm[,1])),
                       y1=Xm[,1],
                       y2=Xm[,2],
                       y1l1 = lag(Xm[,1],1),
                       y2l1 = lag(Xm[,2],1),
                       y1l2 = lag(Xm[,1],2),
                       y2l2 = lag(Xm[,2],2),
                       x1 = g1,
                       x2 = g2,
                       x3 = g3)

# visualisasi
op <- par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(t, X[,1],   type = "l", main = "VAR: y1",    xlab = "t", ylab = "y1")
plot(t, X[,2],   type = "l", main = "VAR: y2",    xlab = "t", ylab = "y2")
plot(t, Xn[,1], type = "l", main = "MESTAR: y1", xlab = "t", ylab = "y1")
plot(t, Xn[,2], type = "l", main = "MESTAR: y2", xlab = "t", ylab = "y2")
plot(t, Xm[,1],  type = "l", main = "MIXED: y1",  xlab = "t", ylab = "y1")
plot(t, Xm[,2],  type = "l", main = "MIXED: y2",  xlab = "t", ylab = "y2")
par(op)

# generate eksogen untuk pemodelan
g1 <- arima.sim(list(order = c(1, 0,0), ar=-.8), n=n); plot(g1); adf.test(g1)
g2 <- arima.sim(list(order = c(1, 0,0), ar=.5), n=n); plot(g2); adf.test(g2)
g3 <- arima.sim(list(order = c(1, 0,0), ar=.2), n=n); plot(g3); adf.test(g3)

# matriks input lin & nonlin
df_lin <- data.frame(t = seq(1:length(X[,1])),
                     y1= X[,1],
                     y2= X[,2],
                     y1l1 = lag(X[,1],1),
                     y2l1 = lag(X[,2],1),
                     y1l2 = lag(X[,1],2),
                     y2l2 = lag(X[,2],2),
                     x1 = g1,
                     x2 = g2,
                     x3 = g3)

df_nonlin <- data.frame(t = seq(1:length(Xn[,1])),
                        y1=Xn[,1],
                        y2=Xn[,2],
                        y1l1 = lag(Xn[,1],1),
                        y2l1 = lag(Xn[,2],1),
                        y1l2 = lag(Xn[,1],2),
                        y2l2 = lag(Xn[,2],2),
                        x1 = g1,
                        x2 = g2,
                        x3 = g3)
                        
df_mixed <- data.frame(t = seq(1:length(Xm[,1])),
                        y1=Xm[,1],
                        y2=Xm[,2],
                        y1l1 = lag(Xm[,1],1),
                        y2l1 = lag(Xm[,2],1),
                        y1l2 = lag(Xm[,1],2),
                        y2l2 = lag(Xm[,2],2),
                        x1 = g1,
                        x2 = g2,
                        x3 = g3)

# batasan : tidak dilakukan qvarx check

# split
i_train = 0.8
i_test = 0.2

d <- list()
d = list(lin=df_lin[1:nrow(df_lin),],
         lintr=df_lin[3:(nrow(df_lin)*i_train),],
         lints=df_lin[((nrow(df_lin)*i_train)+1):nrow(df_lin),],
         nonlin=df_nonlin[1:nrow(df_nonlin),],
         nonlintr=df_nonlin[3:(nrow(df_nonlin)*i_train),],
         nonlints=df_nonlin[((nrow(df_nonlin)*i_train)+1):nrow(df_nonlin),],
         mixed=df_mixed[1:nrow(df_mixed),],
         mixedtr=df_mixed[3:(nrow(df_mixed)*i_train),],
         mixedts=df_mixed[((nrow(df_mixed)*i_train)+1):nrow(df_mixed),]
)

# qvarx modelling dg 2 lag dan tanpa restricted matrix
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

# batasan penelitian
# (1) Tidak dilakukan ramalan diluar out-sample
# (2) Tidak dilakukan VARX check
# (3) Tidak dilakukan uji terasvirta

################################ LINEAR ####################################
# PEMODELAN VARX
m_lin<-VARX(zt=d$lintr[,2:3], p=2, xt=d$lintr[,8:10],m=0, include.mean = 0, fixed = NULL)
p_lin <- matrix(NA, nrow= nrow(d$lintr), ncol = 2)
for (i in 1:ncol(m_lin$coef)) {
  p_lin[,i] <- as.matrix(d$lintr[,4:10]) %*% m_lin$coef[,i]
}
print(m_lin$coef, round(4))

# akurasi in-sample
smape.100(actual=d$lintr[,2], predicted = p_lin[,1])
smape.100(actual=d$lintr[,3], predicted = p_lin[,2])

# ramalan out-sample
p_lin_out <- matrix(NA, nrow= nrow(d$lints), ncol = 2)
for (i in 1:ncol(m_lin$coef)) {
  p_lin_out[,i] <- as.matrix(d$lints[,4:10]) %*% m_lin$coef[,i]
}

# akurasi out-sample
smape.100(actual=d$lints[,2], predicted = p_lin_out[,1])
smape.100(actual=d$lints[,3], predicted = p_lin_out[,2])

# PEMODELAN QVARX
m_lin_05 <- QVARX(y=as.matrix(d$lintr[,2:3]), x_exog=as.matrix(d$lintr[,8:10]), 
                  xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau1))
m_lin_50 <- QVARX(y=as.matrix(d$lintr[,2:3]), x_exog=as.matrix(d$lintr[,8:10]), 
                  xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau2))
m_lin_95 <- QVARX(y=as.matrix(d$lintr[,2:3]), x_exog=as.matrix(d$lintr[,8:10]), 
                  xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau3))

# prediksi in sample, needed full data
p_lin_05 <- predict_QVARX(m_lin_05, df=d$lin[,-(4:7)], p=2, i_tr = i_train)
p_lin_50 <- predict_QVARX(m_lin_50, df=d$lin[,-(4:7)], p=2, i_tr = i_train)
p_lin_95 <- predict_QVARX(m_lin_95, df=d$lin[,-(4:7)], p=2, i_tr = i_train)
nrow(p_lin_50)

# akurasi in-sample lv 0,5
smape.100(actual=d$lintr[,2], predicted = p_lin_50[-1,1])
smape.100(actual=d$lintr[,3], predicted = p_lin_50[-1,2])

# peramalan out-sample
f_lin_05 <- predict_QVARX_out(model=m_lin_05, df=d$lin[,-(4:7)], p=2, newxt=d$lints[-1,8:10])
f_lin_50 <- predict_QVARX_out(model=m_lin_50, df=d$lin[,-(4:7)], p=2, newxt=d$lints[-1,8:10])
f_lin_95 <- predict_QVARX_out(model=m_lin_95, df=d$lin[,-(4:7)], p=2, newxt=d$lints[-1,8:10])

# akurasi out-sample lv 0,5
smape.100(actual=d$lints[-1,2], predicted = f_lin_50[,1])
smape.100(actual=d$lints[-1,3], predicted = f_lin_50[,2])

# PEMODELAN MO-QRNN, needed training data, x=input layer, y=output layer
lin_s <- tune_qrnn0.5(x=as.matrix(d$lintr[,4:10]), y=as.matrix(d$lintr[,2:3]),
                   hidden.range = 3:14,
                   penalty.range = c(0,0.1,0.5),
                   tau = 0.5,
                   eps.seq = 10^seq(-2, -4, length.out = 5),
                   n.trials = 10,
                   init.range = c(-0.1,0.5,-0.1,0.5),
                   scale.y=FALSE)

# ganti fungsi
lin_s_05 <- tune_qrnn(x=as.matrix(d$lintr[,4:10]), y=as.matrix(d$lintr[,2:3]),
                      hidden.range = 3:14,
                      penalty.range = c(0,0.1,0.5),
                      tau = 0.05,
                      eps.seq = 10^seq(-2, -4, length.out = 5),
                      n.trials = 10,
                      init.range = c(-0.1,0.5,-0.1,0.5),
                      scale.y=FALSE)
lin_s_95 <- tune_qrnn(x=as.matrix(d$lintr[,4:10]), y=as.matrix(d$lintr[,2:3]),
                      hidden.range = 3:14,
                      penalty.range = c(0,0.1,0.5),
                      tau = 0.95,
                      eps.seq = 10^seq(-2, -4, length.out = 5),
                      n.trials = 10,
                      init.range = c(-0.1,0.5,-0.1,0.5),
                      scale.y=FALSE)

hasil_sim_lin <- list(tau05 = lin_s_05, tau50=lin_s, tau95=lin_s_95)
bobot_sim_lin <- list(b_05_1=lin_s_05$best.model$weights$W1,
                      b_05_2=lin_s_05$best.model$weights$W2,
                      b_50_1=lin_s$best.model$weights$W1,
                      b_50_2=lin_s$best.model$weights$W2,
                      b_95_1=lin_s_95$best.model$weights$W1,
                      b_95_2=lin_s_95$best.model$weights$W2)
pred_sim_lin <- list(y_05 = lin_s_05$best.model$prediksi,
                     y_50 = lin_s$best.model$prediksi,
                     y_95 = lin_s_95$best.model$prediksi)

# evaluasi in-sample
smape.100(d$lintr[,2],pred_sim_lin$y_50[,1])
smape.100(d$lintr[,3],pred_sim_lin$y_50[,2])

# ramalan out-sample
r_sim_lin05<- qrnn.eval(d$lints[,4:10], bobot_sim_lin$b_05_1, bobot_sim_lin$b_05_2, lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
r_sim_lin50<- qrnn.eval(d$lints[,4:10], bobot_sim_lin$b_50_1, bobot_sim_lin$b_50_2, lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
r_sim_lin95<- qrnn.eval(d$lints[,4:10], bobot_sim_lin$b_95_1, bobot_sim_lin$b_95_2, lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=Th)

# smape untuk lv 0,5
which(apply(is.nan(as.matrix(r_sim_lin50)), 1, any)) #r_sim_lin50[62,]
smape.100(actual=d$lints[-62,2], predicted = r_sim_lin50[-62,1])
smape.100(actual=d$lints[-62,3], predicted = r_sim_lin50[-62,2])
dim(p_lin_05[-1,])
# qvss 0.05, needed matrix (Txm), ref=model qvarx, for=mo-qrnn
in_sc05_lin <- qvss(y=d$lintr[,2:3], ypred_ref = p_lin_05[-1,] , ypred_for = pred_sim_lin$y_05,
                    tau=0.05, eps= min(eps.seq), W1 = bobot_sim_lin$b_05_1, W2= bobot_sim_lin$b_05_2, penalty = 0, 
                    penalty2=0, unpenalized=NULL); in_sc05_lin
out_sc05_lin <- qvss(y=d$lints[,2:3], ypred_ref = f_lin_05  , ypred_for = r_sim_lin05,
                     tau=0.05, eps= min(eps.seq), W1 = bobot_sim_lin$b_05_1, W2= bobot_sim_lin$b_05_2, penalty = 0, 
                     penalty2=0, unpenalized=NULL); out_sc05_lin

# qvss 0.95, needed matrix (Txm), ref=model qvarx, for=mo-qrnn
in_sc95_lin <- qvss(y=d$lintr[,2:3], ypred_ref = p_lin_95[-1] , ypred_for = pred_sim_lin$y_95,
                    tau=0.95, eps= min(eps.seq), W1 = bobot_sim_lin$b_95_1, W2= bobot_sim_lin$b_95_2, penalty = 0, 
                    penalty2=0, unpenalized=NULL); in_sc95_lin
out_sc95_lin <- qvss(y=d$lints[,2:3], ypred_ref = f_lin_95  , ypred_for = r_sim_lin95,
                     tau=0.95, eps= min(eps.seq), W1 = bobot_sim_lin$b_95_1, W2= bobot_sim_lin$b_95_2, penalty = 0, 
                     penalty2=0, unpenalized=NULL); out_sc95_lin


################################ NON-LINEAR ##################################
# PEMODELAN VARX
m_nonlin<-VARX(zt=d$nonlintr[,2:3], p=2, xt=d$nonlintr[,8:10],m=0, include.mean = 0, fixed = NULL)
p_nonlin <- matrix(NA, nrow= nrow(d$nonlintr), ncol = 2)
for (i in 1:ncol(m_nonlin$coef)) {
  p_nonlin[,i] <- as.matrix(d$nonlintr[,4:10]) %*% m_nonlin$coef[,i]
}
print(m_nonlin$coef, round(4))

#akurasi in-sample
smape.100(actual=d$nonlintr[,2], predicted = p_nonlin[,1])
smape.100(actual=d$nonlintr[,3], predicted = p_nonlin[,2])

#ramalan out-sample
p_nonlin_out <- matrix(NA, nrow= nrow(d$nonlints), ncol = 2)
for (i in 1:ncol(m_nonlin$coef)) {
  p_nonlin_out[,i] <- as.matrix(d$nonlints[,4:10]) %*% m_nonlin$coef[,i]
}

#akurasi in-sample
smape.100(actual=d$nonlints[,2], predicted = p_nonlin_out[,1])
smape.100(actual=d$nonlints[,3], predicted = p_nonlin_out[,2])

# PEMODELAN QVARX
m_nonlin_05 <- QVARX(y=as.matrix(d$nonlintr[,2:3]), x_exog=as.matrix(d$nonlintr[,8:10]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau1))
m_nonlin_50 <- QVARX(y=as.matrix(d$nonlintr[,2:3]), x_exog=as.matrix(d$nonlintr[,8:10]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau2))
m_nonlin_95 <- QVARX(y=as.matrix(d$nonlintr[,2:3]), x_exog=as.matrix(d$nonlintr[,8:10]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau3))

# prediksi in-sample, needed full data
p_nonlin_05 <- predict_QVARX(m_nonlin_05, df=d$nonlin[,-(4:7)], p=2, i_tr = i_train)
p_nonlin_50 <- predict_QVARX(m_nonlin_50, df=d$nonlin[,-(4:7)], p=2, i_tr = i_train)
p_nonlin_95 <- predict_QVARX(m_nonlin_95, df=d$nonlin[,-(4:7)], p=2, i_tr = i_train)

# akurasi lv 0,5 (0-1)
smape.100(actual=d$nonlintr[,2], predicted = p_nonlin_50[-1,1])
smape.100(actual=d$nonlintr[,3], predicted = p_nonlin_50[-1,2])

# batasan penelitian : variabel eksogen tidak di forecast

# ramalan out-sample
f_nonlin_05 <- predict_QVARX_out(model=m_nonlin_05, df=d$nonlin[,-(4:7)], p=2, newxt=d$nonlints[-1,8:10])
f_nonlin_50 <- predict_QVARX_out(model=m_nonlin_50, df=d$nonlin[,-(4:7)], p=2, newxt=d$nonlints[-1,8:10])
f_nonlin_95 <- predict_QVARX_out(model=m_nonlin_95, df=d$nonlin[,-(4:7)], p=2, newxt=d$nonlints[-1,8:10])

# akurasi lv 0,5
smape.100(actual=d$nonlints[-1,2], predicted = f_nonlin_50[,1])
smape.100(actual=d$nonlints[-1,3], predicted = f_nonlin_50[,2])

# PEMODELAN MO-QRNN, needed training data, x=input layer, y=output layer
nonlin_s <- tune_qrnn0.5(x=as.matrix(d$nonlintr[,4:10]), y=as.matrix(d$nonlintr[,2:3]),
                      hidden.range = 3:14,
                      penalty.range = c(0,0.1,0.5),
                      tau = 0.5,
                      eps.seq = 10^seq(-2, -4, length.out = 5),
                      n.trials = 10,
                      init.range = c(-0.1,0.5,-0.1,0.5),
                      scale.y=FALSE)

# ganti fungsi
nonlin_s_05 <- tune_qrnn(x=as.matrix(d$nonlintr[,4:10]), y=as.matrix(d$nonlintr[,2:3]),
                         hidden.range = 3:14,
                         penalty.range = c(0,0.1,0.5),
                         tau = 0.05,
                         eps.seq = 10^seq(-2, -4, length.out = 5),
                         n.trials = 10,
                         init.range = c(-0.1,0.5,-0.1,0.5),
                         scale.y=FALSE)
nonlin_s_95 <- tune_qrnn(x=as.matrix(d$nonlintr[,4:10]), y=as.matrix(d$nonlintr[,2:3]),
                         hidden.range = 3:14,
                         penalty.range = c(0,0.1,0.5),
                         tau = 0.95,
                         eps.seq = 10^seq(-2, -4, length.out = 5),
                         n.trials = 10,
                         init.range = c(-0.1,0.5,-0.1,0.5),
                         scale.y=FALSE)

hasil_sim_nonlin <- list(tau05 = nonlin_s_05, tau50=nonlin_s, tau95=nonlin_s_95)
bobot_sim_nonlin <- list(b_05_1=nonlin_s_05$best.model$weights$W1,
                         b_05_2=nonlin_s_05$best.model$weights$W2,
                         b_50_1=nonlin_s$best.model$weights$W1,
                         b_50_2=nonlin_s$best.model$weights$W2,
                         b_95_1=nonlin_s_95$best.model$weights$W1,
                         b_95_2=nonlin_s_95$best.model$weights$W2)
pred_sim_nonlin <- list(y_05 = nonlin_s_05$best.model$prediksi,
                        y_50 = nonlin_s$best.model$prediksi,
                        y_95 = nonlin_s_95$best.model$prediksi)

# evaluasi in-sample
smape.100(d$nonlintr[,2],pred_sim_nonlin$y_50[,1])
smape.100(d$nonlintr[,3],pred_sim_nonlin$y_50[,2])

# ramalan out-sample
r_sim_nonlin05<- qrnn.eval(d$nonlints[,4:10], bobot_sim_nonlin$b_05_1, bobot_sim_nonlin$b_05_2, lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
r_sim_nonlin50<- qrnn.eval(d$nonlints[,4:10], bobot_sim_nonlin$b_50_1, bobot_sim_nonlin$b_50_2, lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
r_sim_nonlin95<- qrnn.eval(d$nonlints[,4:10], bobot_sim_nonlin$b_95_1, bobot_sim_nonlin$b_95_2, lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=Th)

# smape un lv 0,5
which(apply(is.nan(as.matrix(r_sim_nonlin50)), 1, any)) #r_sim_nonlin50[62,]
smape.100(actual=d$nonlints[,2], predicted = r_sim_nonlin50[,1])
smape.100(actual=d$nonlints[,3], predicted = r_sim_nonlin50[,2])

# qvss 0.05, needed matrix (Txm), ref=model qvarx, for=mo-qrnn
in_sc05_nonlin <- qvss(y=d$nonlintr[,2:3], ypred_ref = p_nonlin_05[-1] , ypred_for = pred_sim_nonlin$y_05,
                       tau=0.05, eps= min(eps.seq), W1 = bobot_sim_nonlin$b_05_1, W2= bobot_sim_nonlin$b_05_2, penalty = 0, 
                       penalty2=0, unpenalized=NULL); in_sc05_nonlin
out_sc05_nonlin <- qvss(y=d$nonlints[,2:3], ypred_ref = f_nonlin_05  , ypred_for = r_sim_nonlin05,
                        tau=0.05, eps= min(eps.seq), W1 = bobot_sim_nonlin$b_05_1, W2= bobot_sim_nonlin$b_05_2, penalty = 0, 
                        penalty2=0, unpenalized=NULL); out_sc05_nonlin

# qvss 0.95, needed matrix (Txm), ref=model qvarx, for=mo-qrnn
in_sc95_nonlin <- qvss(y=d$nonlintr[,2:3], ypred_ref = p_nonlin_95[-1] , ypred_for = pred_sim_nonlin$y_95,
                       tau=0.95, eps= min(eps.seq), W1 = bobot_sim_nonlin$b_95_1, W2= bobot_sim_nonlin$b_95_2, penalty = 0, 
                       penalty2=0, unpenalized=NULL); in_sc95_nonlin
out_sc95_nonlin <- qvss(y=d$nonlints[,2:3], ypred_ref = f_nonlin_95  , ypred_for = r_sim_nonlin95,
                        tau=0.95, eps= min(eps.seq), W1 = bobot_sim_nonlin$b_95_1, W2= bobot_sim_nonlin$b_95_2, penalty = 0, 
                        penalty2=0, unpenalized=NULL); out_sc95_nonlin

################################ MIXED ####################################
# PEMODELAN VARX
m_mixed<-VARX(zt=d$mixedtr[,2:3], p=2, xt=d$mixedtr[,8:10],m=0, include.mean = 0, fixed = NULL)
p_mixed <- matrix(NA, nrow= nrow(d$mixedtr), ncol = 2)
for (i in 1:ncol(m_mixed$coef)) {
  p_mixed[,i] <- as.matrix(d$mixedtr[,4:10]) %*% m_mixed$coef[,i]
}
print(m_mixed$coef, round(4))

#akurasi in-sample
smape.100(actual=d$mixedtr[,2], predicted = p_mixed[,1])
smape.100(actual=d$mixedtr[,3], predicted = p_mixed[,2])

#ramalan out-sample
p_mixed_out <- matrix(NA, nrow= nrow(d$mixedts), ncol = 2)
for (i in 1:ncol(m_mixed$coef)) {
  p_mixed_out[,i] <- as.matrix(d$mixedts[,4:10]) %*% m_mixed$coef[,i]
}

#akurasi in-sample
smape.100(actual=d$mixedts[,2], predicted = p_mixed_out[,1])
smape.100(actual=d$mixedts[,3], predicted = p_mixed_out[,2])

# PEMODELAN QVARX
m_mixed_05 <- QVARX(y=as.matrix(d$mixedtr[,2:3]), x_exog=as.matrix(d$mixedtr[,8:10]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau1))
m_mixed_50 <- QVARX(y=as.matrix(d$mixedtr[,2:3]), x_exog=as.matrix(d$mixedtr[,8:10]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau2))
m_mixed_95 <- QVARX(y=as.matrix(d$mixedtr[,2:3]), x_exog=as.matrix(d$mixedtr[,8:10]), 
                     xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau3))

# prediksi in-sample, needed full data
p_mixed_05 <- predict_QVARX(m_mixed_05, df=d$mixed[,-(4:7)], p=2, i_tr = i_train)
p_mixed_50 <- predict_QVARX(m_mixed_50, df=d$mixed[,-(4:7)], p=2, i_tr = i_train)
p_mixed_95 <- predict_QVARX(m_mixed_95, df=d$mixed[,-(4:7)], p=2, i_tr = i_train)

# akurasi lv 0,5 (0-1)
smape.100(actual=d$mixedtr[,2], predicted = p_mixed_50[-1,1])
smape.100(actual=d$mixedtr[,3], predicted = p_mixed_50[-1,2])

# batasan penelitian : variabel eksogen tidak di forecast

# ramalan out-sample
f_mixed_05 <- predict_QVARX_out(model=m_mixed_05, df=d$mixed[,-(4:7)], p=2, newxt=d$mixedts[-1,8:10])
f_mixed_50 <- predict_QVARX_out(model=m_mixed_50, df=d$mixed[,-(4:7)], p=2, newxt=d$mixedts[-1,8:10])
f_mixed_95 <- predict_QVARX_out(model=m_mixed_95, df=d$mixed[,-(4:7)], p=2, newxt=d$mixedts[-1,8:10])

# akurasi lv 0,5
smape.100(actual=d$mixedts[-1,2], predicted = f_mixed_50[,1])
smape.100(actual=d$mixedts[-1,3], predicted = f_mixed_50[,2])

# PEMODELAN MO-QRNN, needed training data, x=input layer, y=output layer
mixed_s <- tune_qrnn0.5(x=as.matrix(d$mixedtr[,4:10]), y=as.matrix(d$mixedtr[,2:3]),
                         hidden.range = 3:14,
                         penalty.range = c(0,0.1,0.5),
                         tau = 0.5,
                         eps.seq = 10^seq(-2, -4, length.out = 5),
                         n.trials = 10,
                         init.range = c(-0.1,0.5,-0.1,0.5),
                         scale.y=FALSE)

# ganti fungsi
mixed_s_05 <- tune_qrnn(x=as.matrix(d$mixedtr[,4:10]), y=as.matrix(d$mixedtr[,2:3]),
                         hidden.range = 3:14,
                         penalty.range = c(0,0.1,0.5),
                         tau = 0.05,
                         eps.seq = 10^seq(-2, -4, length.out = 5),
                         n.trials = 10,
                         init.range = c(-0.1,0.5,-0.1,0.5),
                         scale.y=FALSE)
mixed_s_95 <- tune_qrnn(x=as.matrix(d$mixedtr[,4:10]), y=as.matrix(d$mixedtr[,2:3]),
                         hidden.range = 3:14,
                         penalty.range = c(0,0.1,0.5),
                         tau = 0.95,
                         eps.seq = 10^seq(-2, -4, length.out = 5),
                         n.trials = 10,
                         init.range = c(-0.1,0.5,-0.1,0.5),
                         scale.y=FALSE)

hasil_sim_mixed <- list(tau05 = mixed_s_05, tau50=mixed_s, tau95=mixed_s_95)
bobot_sim_mixed <- list(b_05_1=mixed_s_05$best.model$weights$W1,
                         b_05_2=mixed_s_05$best.model$weights$W2,
                         b_50_1=mixed_s$best.model$weights$W1,
                         b_50_2=mixed_s$best.model$weights$W2,
                         b_95_1=mixed_s_95$best.model$weights$W1,
                         b_95_2=mixed_s_95$best.model$weights$W2)
pred_sim_mixed <- list(y_05 = mixed_s_05$best.model$prediksi,
                        y_50 = mixed_s$best.model$prediksi,
                        y_95 = mixed_s_95$best.model$prediksi)

# evaluasi in-sample
smape.100(d$mixedtr[,2],pred_sim_mixed$y_50[,1])
smape.100(d$mixedtr[,3],pred_sim_mixed$y_50[,2])

# ramalan out-sample
r_sim_mixed05<- qrnn.eval(d$mixedts[,4:10], bobot_sim_mixed$b_05_1, bobot_sim_mixed$b_05_2, lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
r_sim_mixed50<- qrnn.eval(d$mixedts[,4:10], bobot_sim_mixed$b_50_1, bobot_sim_mixed$b_50_2, lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)
r_sim_mixed95<- qrnn.eval(d$mixedts[,4:10], bobot_sim_mixed$b_95_1, bobot_sim_mixed$b_95_2, lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=Th)

# smape un lv 0,5
smape.100(actual=d$mixedts[,2], predicted = r_sim_mixed50[,1])
smape.100(actual=d$mixedts[,3], predicted = r_sim_mixed50[,2])

# qvss 0.05, needed matrix (Txm), ref=model qvarx, for=mo-qrnn
in_sc05_mixed <- qvss(y=d$mixedtr[,2:3], ypred_ref = p_mixed_05[-1] , ypred_for = pred_sim_mixed$y_05,
                       tau=0.05, eps= min(eps.seq), W1 = bobot_sim_mixed$b_05_1, W2= bobot_sim_mixed$b_05_2, penalty = 0, 
                       penalty2=0, unpenalized=NULL); in_sc05_mixed
out_sc05_mixed <- qvss(y=d$mixedts[,2:3], ypred_ref = f_mixed_05  , ypred_for = r_sim_mixed05,
                        tau=0.05, eps= min(eps.seq), W1 = bobot_sim_mixed$b_05_1, W2= bobot_sim_mixed$b_05_2, penalty = 0, 
                        penalty2=0, unpenalized=NULL); out_sc05_mixed

# qvss 0.95, needed matrix (Txm), ref=model qvarx, for=mo-qrnn
in_sc95_mixed <- qvss(y=d$mixedtr[,2:3], ypred_ref = p_mixed_95[-1] , ypred_for = pred_sim_mixed$y_95,
                       tau=0.95, eps= min(eps.seq), W1 = bobot_sim_mixed$b_95_1, W2= bobot_sim_mixed$b_95_2, penalty = 0, 
                       penalty2=0, unpenalized=NULL); in_sc95_mixed
out_sc95_mixed <- qvss(y=d$mixedts[,2:3], ypred_ref = f_mixed_95  , ypred_for = r_sim_mixed95,
                        tau=0.95, eps= min(eps.seq), W1 = bobot_sim_mixed$b_95_1, W2= bobot_sim_mixed$b_95_2, penalty = 0, 
                        penalty2=0, unpenalized=NULL); out_sc95_mixed
