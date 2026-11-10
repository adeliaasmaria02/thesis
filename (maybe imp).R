N <- n + burn

## generate white noise
Sigma <- matrix(c(1.0, 0.45,0.45, 0.80),2,2)
U <- mvrnorm(N, mu = c(0,0), Sigma = Sigma)
colnames(U) <- c("u1", "u2")

## generate exogenous
ek1 <- arima.sim(list(order = c(1, 0,0), ar=-.8), n=N)
ek2 <- arima.sim(list(order = c(1, 0,0), ar=.5), n=N)
ek3 <- arima.sim(list(order = c(1, 0,0), ar=.2), n=N)

## generate VAR
X <- matrix(NA_real_, nrow = N, ncol = 2); colnames(X) <- c("x1", "x2")
X[1, ] <- c(0, 0)
X[2, ] <- c(0, 0)

for (t in 3:(N)) {
  x1_l1 <- X[t - 1, 1]; x1_l2 <- X[t-2, 1]
  x2_l1 <- X[t - 1, 2]; x2_l2 <- X[t-2, 2]
  X[t, 1] <-  0.3 * x1_l1 + 0.5 * x2_l1 + 0.6 * x1_l2 + 0.12 * x2_l2 + 0.2*ex1[t] + 0.5*ex2[t] - 0.7*ex3[t] + U[t-2, "u1"]
  X[t, 2] <- -0.7 * x1_l1 + 0.8 * x2_l1 + 0.1 * x1_l2 - 0.3 * x2_l2 + 0.2*ex1[t] + 0.5*ex2[t] - 0.7*ex3[t] + U[t-2, "u2"]
}

## generate MESTAR
Xn <- matrix(NA_real_, nrow = N, ncol = 2); colnames(X) <- c("x1", "x2")
Xn[1, ] <- c(0, 0)
Xn[2, ] <- c(0, 0)

for (t in 2:(N)) {
  Xn1_l1 <- Xn[t-1,1]
  Xn2_l1 <- Xn[t-1,2]
  
  Xn[t, 1] <- 6.5 * Xn1_l1 * exp(-0.25 * Xn1_l1^2) +3.5 * Xn2_l1 * exp(-0.45 * Xn2_l1^2)  + 0.2*ex1[t] + 0.5*ex2[t] - 0.7*ex3[t] + U[t - 1, "u1"]
  Xn[t, 2] <- 4.3 * Xn1_l1 * exp(-0.15 * Xn1_l1^2) + 3.7 * Xn2_l1 * exp(-0.25 * Xn2_l1^2)  + 0.2*ex1[t] + 0.5*ex2[t] - 0.7*ex3[t] + U[t - 1, "u2"]
}

## burn in
X  <- X [(burn+1):N,]
Xn <- Xn[(burn+1):N,]

## generate exogenous modelling
g1 <- arima.sim(list(order = c(1, 0,0), ar=-.8), n=n)
g2 <- arima.sim(list(order = c(1, 0,0), ar=.5), n=n)
g3 <- arima.sim(list(order = c(1, 0,0), ar=.2), n=n)

## dataframe
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

tau1 <- 0.05
tau2 <- 0.5
tau3 <- 0.95
p <-2
restrict_QVARX <- matrix(1, nrow=7, ncol=2, byrow=TRUE)

## split
i_train = 0.8

df <- df_lin

d <- list()
d = list(full=df[1:nrow(df),],
         tr=df[3:(nrow(df)*i_train),],
         ts=df[((nrow(df)*i_train)+1):nrow(df),])

## VARX
m_lin<-VARX(zt=d$tr[,2:3], p=2, xt=d$tr[,8:10],m=0, include.mean = 0, fixed = NULL)
# prediksi in-sample
p_lin <- matrix(NA, nrow= nrow(d$tr), ncol = 2)
for (i in 1:ncol(m_lin$coef)) {
  p_lin[,i] <- as.matrix(d$tr[,4:10]) %*% m_lin$coef[,i] }

# ramalan out-sample
p_lin_out <- matrix(NA, nrow= nrow(d$ts), ncol = 2)
for (i in 1:ncol(m_lin$coef)) {
  p_lin_out[,i] <- as.matrix(d$ts[,4:10]) %*% m_lin$coef[,i] }

## QVARX
m_lin_05 <- QVARX(y=as.matrix(d$tr[,2:3]), x_exog=as.matrix(d$tr[,8:10]), 
                  xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau1))
m_lin_50 <- QVARX(y=as.matrix(d$tr[,2:3]), x_exog=as.matrix(d$tr[,8:10]), 
                  xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau2))
m_lin_95 <- QVARX(y=as.matrix(d$tr[,2:3]), x_exog=as.matrix(d$tr[,8:10]), 
                  xreg=restrict_QVARX, configuration=list(nlag=p, tau=tau3))

# prediksi in sample, needed full data
p_lin_05 <- predict_QVARX(m_lin_05, df=d$full[,-(4:7)], p=2, i_tr = i_train)
p_lin_50 <- predict_QVARX(m_lin_50, df=d$full[,-(4:7)], p=2, i_tr = i_train)
p_lin_95 <- predict_QVARX(m_lin_95, df=d$full[,-(4:7)], p=2, i_tr = i_train)

# peramalan out-sample
f_lin_05 <- predict_QVARX_out(model=m_lin_05, df=d$full[,-(4:7)], p=2, newxt=d$ts[-1,8:10])
f_lin_50 <- predict_QVARX_out(model=m_lin_50, df=d$full[,-(4:7)], p=2, newxt=d$ts[-1,8:10])
f_lin_95 <- predict_QVARX_out(model=m_lin_95, df=d$full[,-(4:7)], p=2, newxt=d$ts[-1,8:10])

# 0,05
lin_s_05 <- tune_qrnn(x=as.matrix(d$tr[,4:10]), y=as.matrix(d$tr[,2:3]),
                      hidden.range = 14,
                      penalty.range = 0,
                      tau = 0.05,
                      eps.seq = 10^seq(-2, -4, length.out = 5),
                      n.trials = 10,
                      init.range = c(-0.1,0.5,-0.1,0.5),
                      scale.y=FALSE)

pred_sim_lin$y_05 = lin_s_05$best.model$prediksi
r_sim_lin05<- qrnn.eval(d$ts[,4:10], bobot_sim_lin$b_05_1, bobot_sim_lin$b_05_2, lower=-Inf, monotone=NULL,  additive=FALSE, eps=3.906250e-03, Th=Th)

nrow(d$full[,-(4:7)])

in05=qvss(y=d$tr[,2:3], ypred_ref = p_lin_05[-1,] , ypred_for = pred_sim_lin$y_05,
          tau=0.05, eps= min(eps.seq), W1 = bobot_sim_lin$b_05_1, W2= bobot_sim_lin$b_05_2, penalty = 0, 
          penalty2=0, unpenalized=NULL)
out05=qvss(y=d$ts[-1,2:3], ypred_ref = f_lin_05  , ypred_for = r_sim_lin05[-1,],
           tau=0.05, eps= min(eps.seq), W1 = bobot_sim_lin$b_05_1, W2= bobot_sim_lin$b_05_2, penalty = 0, 
           penalty2=0, unpenalized=NULL)
in95=qvss(y=d$tr[,2:3], ypred_ref = p_lin_95[-1,] , ypred_for = pred_sim_lin$y_95,
          tau=0.95, eps= min(eps.seq), W1 = bobot_sim_lin$b_95_1, W2= bobot_sim_lin$b_95_2, penalty = 0, 
          penalty2=0, unpenalized=NULL),
out95=qvss(y=d$ts[,2:3], ypred_ref = f_lin_95  , ypred_for = r_sim_lin95,
           tau=0.95, eps= min(eps.seq), W1 = bobot_sim_lin$b_95_1, W2= bobot_sim_lin$b_95_2, penalty = 0, 
           penalty2=0, unpenalized=NULL)
