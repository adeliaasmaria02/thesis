## differencing tidak diterapkan pada MOQRNN
df <- data.frame(
  time=target$date[-1],
  y1=kel$`Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga`,
  y2=kel$`Pendidikan, Rekreasi, dan Olahraga`,
  y1l1=lag(kel$`Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga`,1),
  y1l2=lag(kel$`Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga`,2),
  y2l1=lag(kel$`Pendidikan, Rekreasi, dan Olahraga`,1),
  y2l2=lag(kel$`Pendidikan, Rekreasi, dan Olahraga`,2),
  x1=fci_pca$fci1,
  x2=fci_pca$fci2,
  x3=FSI$fsi
)

## data qvarx diterapkan pada mo-qrnn: eksogen di differencing
df <- cbind(dfr[,1:3],dfp)
df <- df[-c(1,2),]

plot(df$time, df$y1, type="l")
plot(df$time, df$y2, type="l")

## terasvirta test multivariat ## 
library(tseries)
ujiy1 <- terasvirta.test(y=df[,2], x=df[,-c(1,2,3)], type = 'F'); ujiy1 # tdk sign
ujiy2 <- terasvirta.test(y=df[,3], x=df[,-c(1,2,3)], type = 'F'); ujiy2

# 2. split: tr dan te pada qvarx dan moqrnn tdk identik shg harus diinisiasi setiap kali pemodelan
tr_index <- ceiling(0.80*nrow(df))
te_index <- floor(0.20*nrow(df))

tr <- df[1:tr_index,]
te <- df[(tr_index+1):nrow(df),]
View(te)

# define x: input, y:output training
x=as.matrix(tr[,c(4:10)])
y=as.matrix(tr[,c(2:3)])

xt=as.matrix(te[,c(4:10)])
yt=as.matrix(te[,c(2:3)]) 

# 3. scaling : tidak di scaling
# x.center <- attr(x[,3], "scaled:center") 
# x.scale <- attr(x[,3], "scaled:scale")

scale.y = FALSE
if (scale.y) {
  y <- scale(y)
} else {
  attr(y, "scaled:center") <- rep(0, ncol(y))
  attr(y, "scaled:scale") <- rep(1, ncol(y))
}
y.center <- attr(y, "scaled:center")
y.scale <- attr(y, "scaled:scale")

# 4. Inisiasi
t= 0.5
p = 0.1
nh = 3
w <- matrix(rep(1/nrow(y)),nrow(y),ncol(y))
wg <- qrnn.initialize(x=x, y=y, n.hidden=7, init.range = c(-0.1,0.5, -0.1,0.5))
eps.seq = 10^seq(-2, -4, length.out = 5)
weights.best = list()

# 5. optimasi
cost.best = Inf
n.trials = 1:10
for (i in seq(n.trials)) {
  wg <- qrnn.initialize(x=x, y=y, n.hidden=nh, init.range = c(-0.1,0.5, -0.1,0.5))
  fit <- NULL
  for (eps in eps.seq) {
    fit <- suppressWarnings(nlm(qrnn.cost,wg,x=x, y=y,n.hidden=nh,w=w,tau=t,lower = -Inf,eps=eps , 
                                Th=Th, Th.prime=Th.prime, penalty=p, unpenalized=NULL,iterlim = 5000, 
                                gradtol = 1e-4,check.analyticals = FALSE, monotone = NULL, additive = FALSE))
    wg <- fit$estimate
  }
  cost <- fit$minimum
  cat(i, cost, "\n")
  
  if (cost < cost.best) {
    cost.best <- cost
    weights.best <- fit$estimate
  }} 
cost.best
weights.best

weights <- qrnn.reshape(x=x, y=y, weights=weights.best, n.hidden=nh)
ypred_05 <- qrnn.eval(x, weights$W1, weights$W2,lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=Th)
ypred_05[, 1] <- ypred_05[, 1] * y.scale[1] + y.center[1]
ypred_05[, 2] <- ypred_05[, 2] * y.scale[2] + y.center[2]

smape.100(y,ypred_05)

# 6. predict dan eval
x <- sweep(x, 2, x.center, "-")
x <- sweep(x, 2, x.scale, "/")

# 10. prediksi training & evaluasi : model peramalan var eksogen berbeda
# tau = 0.05 
ypred_05 <- qrnn.eval(xt, w0.05$W1, w0.05$W2,lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=tangh)
ypred_05[, 1] <- ypred_05[, 1] * y.scale[1] + y.center[1]
ypred_05[, 2] <- ypred_05[, 2] * y.scale[2] + y.center[2]

ypred_5<- qrnn.eval(xt, w0.5$W1, w0.5$W2, 
                    lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=tangh)

ypred_5[, 1] <- ypred_5[, 1] * y.scale[1] + y.center[1]
ypred_5[, 2] <- ypred_5[, 2] * y.scale[2] + y.center[2]

ypred_95<- qrnn.eval(xt, w0.95$W1, w0.95$W2, 
                     lower=-Inf, monotone=NULL, additive=FALSE, eps=3.906250e-03, Th=tangh)

ypred_95[, 1] <- ypred_95[, 1] * y.scale[1] + y.center[1]
ypred_95[, 2] <- ypred_95[, 2] * y.scale[2] + y.center[2]

# aktual
plot(tr$time, tr$x1,type = "l",col = "black",lwd = 2, xlab = "Waktu",ylab = "Nilai", main = "Visualisasi Dua Time Series")

# mean
lines(tr$time, ypred_5[,1],col = "brown",lwd = 2)

#kuantil bawah
lines(te$time, ypred_05[,1],col = "red",lwd = 2)

#kuantil atas
lines(te$time, ypred_95[,1],col = "blue",lwd = 2)

# 8. visuaslisasi Y2
# aktual
plot(te$time, te$y2,type = "l",col = "black",lwd = 2, xlab = "Waktu",ylab = "Nilai", main = "Visualisasi Dua Time Series")

# mean
lines(te$time, ypred_5[,2],col = "brown",lwd = 2)

#kuantil bawah
lines(te$time, ypred_05[,2],col = "red",lwd = 2)

#kuantil atas
lines(te$time, ypred_95[,2],col = "blue",lwd = 2)
