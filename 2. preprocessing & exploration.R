# make sure running all library in 1.import 

# 1. Pembentukan FCI dengan PCA, pembentuk: (mulai 02/2010)
# Jumlah Uang Beredar = JUB = M rupiah
# Consumer Confidence Index = cci = >100 harapan rumah tangga bagus, <100 diperhatikan (kaitan dg daya beli)
# BI-Rate = birt = <7% (diputuskan BI)
# jakarta interchange .. = jibor = 1 bulan (%) <10%
View(FCI)

# multikolinearitas (bukan tujuan utama digunakan pc) - using VIF
model <- lm(birt ~ jub + cci + jibor, data = FCI)
vif(model)

# 1. standarisasi / scaled -> apakah perlu ? perlu, harus agar varians tdk condong pd data dg skala besar
fci_scaled <- data.frame(scale(FCI[,-1]))
fci_scaled$date <- FCI$date

# 2a. hitung matriks kovarians 
print(cov(FCI[,-1]))
print(cov(fci_scaled[,-5]))

# 2b. hitung matriks korelasi
print(cor(fci_scaled[,-5]))
r <- cor(FCI[,-1])

# 3. eigen dekomposisi (memecah)
eig <- eigen(r)

# 3a. Eigenvalue = lambda
eig$values

# 3b. Eigenvector = matriks e
eig$vectors

# 3 alternatif. numerically using SVD
hasil <- prcomp(fci_scaled[,1:4], center = TRUE, scale. = TRUE)
print(summary(hasil))

# eigen val = lambda
hasil$sdev^2

# eigen vec = matriks e 
hasil$rotation

# 4. menyimpan hasil PC
scores <- hasil$x
head(scores)
stat.desc(scores) #var = lambda (eigen values)

# Scree plot (variance explained)
var_explained <- hasil$sdev^2 / sum(hasil$sdev^2)
qplot(y = var_explained, x = 1:length(var_explained), geom = "line") +
  geom_point() +
  labs(title = "Scree Plot", x = "Principal Component", y = "Variance Explained")

# simpan data -> pake ini buat analisis
fci_pca <- data.frame(
  Time = fci_scaled$date,
  fci1 = scores[, 1],
  fci2 = scores[, 2]
)
View(fci_pca)

#ljung box test -> uji autokorelasi unt melihat apakah residual saling berhubungan (selayaknya data time series)
# H0 = tidak ada autokor smplag ke-h
# H1 = minimal ada 1 autokor != 0 (yg diinginkan)
# tolak H0 jika p-value < alfa
model1 <- lm (fci_pca$fci1 ~ fci_pca$fci2)
res1 <- residuals(model1)
Box.test(res1, type = "Ljung-Box")

model2 <- lm(fci_pca$fci2 ~ fci_pca$fci1)
res2 <- residuals(model2)
Box.test(res2, type = "Ljung-Box")

print(Box.test(fci_pca[,2], lag = 10, type = "Ljung-Box"))
print(Box.test(fci_pca[,3], lag = 10, type = "Ljung-Box"))

#plot
ggplot(fci_pca, aes(x = Time)) +
  geom_line(aes(y = fci1, color = "FCI1")) +
  geom_line(aes(y = fci2, color = "FCI2")) +
  labs(y = "Nilai",
       x= "Tahun",
       color = "") +
  theme_minimal()

#2. Pembentukan FSI dengan Persamaan pembobot
print(cor(FSI[,-1])) # korelasi

#covariance
c <- cov(FSI[,-1])
penyebut <- sum(c)
W <- numeric(ncol(FSI)-2)

for(i in 1:(ncol(FSI)-2)){
  pembilang <- sum(c[i, ])   # sum_{j=1}^r Cov(xi,xj)
  W[i] <- pembilang / penyebut
}

# Result
W[2]
sum(W)

FSI$fsi <- w[1]*FSI[,2]+w[2]*FSI[,3]+w[3]*FSI[,4]+w[4]*FSI[,5]+w[5]*FSI[,6] #drop aja bro
summary(FSI[,-7])
stat.desc(FSI[,-7])

# PLOT FSI
ggplot(FSI, aes(x = date)) +
  geom_line(aes(y = fsi, color = "FSI")) +
  labs( x = "Tahun",
        y = "Nilai",
        color = " ") +
  theme_minimal()

# 3 - Eksplorasi Data

kel <- read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/inflasi/Infkel.xlsx")
df <- data.frame(
  time = seq.Date(from = as.Date("2010/2/1"), to= as.Date("2025/6/1"), by="month"),
  y1 = kel$`Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga`,
  y2 = kel$`Pendidikan, Rekreasi, dan Olahraga`
)

stat.desc(df)
ggplot(df, aes(x = time)) +
  geom_line(aes(y = y2, color = "Inflasi Pendidikan (Y2)"), linewidth = 1) +
  geom_line(aes(y = y1, color = "Inflasi Perumahan (Y1)"), linewidth = 1) +
  scale_color_manual(
    values = c("Inflasi Pendidikan (Y2)" = "blue","Inflasi Perumahan (Y1)" = "red")
  ) +
  labs(
    x = "Tahun",
    y = "Inflasi",
    color = ""
  ) +
  theme_grey() +
  theme(
    legend.position = "bottom"
  )

# save the data, if you want
write_xlsx(list(FSI = FSI,
                FCI = FCI),
           path = "C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/kumpul.xlsx")
