library(readxl)
library(ggplot2)
library(lmtest)
library(rugarch)
library(pastecs)
library(tseries)
library(stats)
library(car)
library(dplyr)
library(tidyr)
library(scales) 
library(lmtest)
library(vars)
library(reshape2)

# IMPORT FCI
# X1.1 : Jumlah Uang Beredar
jubb <- list( 
  j2010 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2010.xlsx"),
  j2011 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2011.xlsx"),
  j2012 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2012.xlsx"),
  j2013 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2013.xlsx"),
  j2014 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2014.xlsx"),
  j2015 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2015.xlsx"),
  j2016 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2016.xlsx"),
  j2017 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2017.xlsx"),
  j2018 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2018.xlsx"),
  j2019 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2019.xlsx"),
  j2020 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2020.xlsx"),
  j2021 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2021.xlsx"),
  j2022 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2022.xlsx"),
  j2023 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2023.xlsx"),
  j2024 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2024.xlsx"),
  j2025 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JUB/Uang Beredar, 2025.xlsx")
)

jub <- as.data.frame(matrix(0,nrow = 185, ncol = 1)) # row, col
colnames(jub) <- c("JUB")

for (j in jubb) {
  datjub <- j
  if(jub[1,1]==0){
    for(i in 1:11){
      jub[i,1] <- as.numeric(datjub[11,i+2])
    }}
  
  else if (jub[12,1]==0){
    for(i in 1:12){
      jub[i+11,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[24,1]==0){
    for(i in 1:12){
      jub[i+23,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[36,1]==0){
    for(i in 1:12){
      jub[i+35,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[48,1]==0){
    for(i in 1:12){
      jub[i+47,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[60,1]==0){
    for(i in 1:12){
      jub[i+59,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[72,1]==0){
    for(i in 1:12){
      jub[i+71,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[84,1]==0){
    for(i in 1:12){
      jub[i+83,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[96,1]==0){
    for(i in 1:12){
      jub[i+95,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[108,1]==0){
    for(i in 1:12){
      jub[i+107,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[120,1]==0){
    for(i in 1:12){
      jub[i+119,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[132,1]==0){
    for(i in 1:12){
      jub[i+131,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[144,1]==0){
    for(i in 1:12){
      jub[i+143,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[156,1]==0){
    for(i in 1:12){
      jub[i+155,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else if (jub[168,1]==0){
    for(i in 1:12){
      jub[i+167,1] <- as.numeric(datjub[11,i+1])
    }}
  
  else {
    for(i in 1:6){
      jub[i+179,1] <- as.numeric(datjub[11,i+1])
    }}
}


# X1.2 : Consumer Confident Index 
cci <- data.frame(read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_CCI/CCI.xlsx"))
cci <- cci[order(as.numeric(rownames(cci)),decreasing = TRUE),] # 2:186

#X1.3 : BI RATE
birtt <- list(
  bi2010 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI Rate, 2010.xlsx"),
  bi2011 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI Rate, 2011.xlsx"),
  bi2012 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI Rate, 2012.xlsx"),
  bi2013 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI Rate, 2013.xlsx"),
  bi2014 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI Rate, 2014.xlsx"),
  bi2015 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI Rate, 2015.xlsx"),
  bi2016 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI Rate, 2016.xlsx")
)

birt <- as.data.frame(matrix(0,nrow = 185, ncol = 1)) # row, col
colnames(birt) <- c("BI-RATE")

for (j in birtt) {
  datbirt <- j
  if(birt[1,1]==0){
    for(i in 1:11){
      birt[i,1] <- as.numeric(datbirt[4,i+2])
    }}
  
  else if (birt[12,1]==0){
    for(i in 1:12){
      birt[i+11,1] <- as.numeric(datbirt[4,i+1])
    }}
  
  else if (birt[24,1]==0){
    for(i in 1:12){
      birt[i+23,1] <- as.numeric(datbirt[4,i+1])
    }}
  
  else if (birt[36,1]==0){
    for(i in 1:12){
      birt[i+35,1] <- as.numeric(datbirt[4,i+1])
    }}
  
  else if (birt[48,1]==0){
    for(i in 1:12){
      birt[i+47,1] <- as.numeric(datbirt[4,i+1])
    }}
  
  else if (birt[60,1]==0){
    for(i in 1:12){
      birt[i+59,1] <- as.numeric(datbirt[4,i+1])
    }}
  
  else {
    for(i in 1:12){
      birt[i+71,1] <- as.numeric(datbirt[4,i+1])
    }
  }
}

birtt2 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_BI rate/BI-7Day-RR.xlsx", skip = 4)
birtt2$`BI-7Day-RR` <- gsub("\\s*%\\s*", "", birtt2$`BI-7Day-RR`)

for (i in 1:102) {
  birt[i+83,1] <- as.numeric(birtt2[109-i,3])
}

# X1.4: Jakarta Interbank Ooffered Rate
jibor <- list( 
  r2010 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR.xlsx", skip = 5),
  r2011 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (1).xlsx", skip = 5),
  r2012 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (2).xlsx", skip = 5),
  r2013 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (3).xlsx", skip = 5),
  r2014 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (4).xlsx", skip = 5),
  r2015 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (5).xlsx", skip = 5),
  r2016 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (6).xlsx", skip = 5),
  r2017 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (7).xlsx", skip = 5),
  r2018 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (8).xlsx", skip = 5),
  r2019 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (9).xlsx", skip = 5),
  r2020 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (10).xlsx", skip = 5),
  r2021 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (11).xlsx", skip = 5),
  r2022 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (12).xlsx", skip = 5),
  r2023 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (13).xlsx", skip = 5),
  r2024 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (14).xlsx", skip = 5),
  r2025 = read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR (15).xlsx", skip = 5)
)

datjib <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FCI_JIBOR/Histori JIBOR.xlsx", sheet = "Sheet1")

for (i in 1:nrow(datjib)) {
  datjib[i,3]=as.numeric(datjib[i,2])
}
for (i in 1:11) {
  datjib[i+174,3]=as.numeric(gsub(",",".",datjib[i+174,2]))
}

FCI <- data.frame(
  date= seq.Date(from = as.Date("2010/2/1"), to= as.Date("2025/6/1"), by="month"),
  jub = jub[,1],
  cci = cci[2:186,3],
  birt = birt[,1],
  jibor = datjib[,3]
)
colnames(FCI) <- c ("date","jub", "cci", "birt", "jibor")

# IMPORT & CONSTRUCT FSI
# X2.1 : tingkat stress Sektor Perbankan 
idxf <- read.csv("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_JKFINANCE/Data Historis IDX Finance (2).csv")
idxf$Terakhir <- as.numeric(gsub(",", ".", gsub("\\.", "", idxf$Terakhir)))

ihsg <- read.csv("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_JKSE_IHSG/Data Historis Jakarta Stock Exchange Composite.csv")
ihsg$Terakhir <- as.numeric(gsub(",", ".", gsub("\\.", "", ihsg$Terakhir)))

idxf$no <- c(1:nrow(idxf)) #numbering
idxf <- idxf[order(idxf$no, decreasing = TRUE),]
idxf$noo <- c(1:nrow(idxf))

ihsg <- ihsg[order(as.numeric(rownames(ihsg)), decreasing = TRUE), ]
ihsg$no <- c(1:nrow(ihsg))

beta <- data.frame( # compile
  date = idxf[121:307,1],
  banking = idxf[121:307,2],
  overall = ihsg[1:187,2]
)

beta$ret_bank <- 0
for (i in 2:187) {
  beta[i,4] <- (beta[i,2]-beta[i-1,2])/beta[i-1,2]
}

beta$ret_ihsg <-0
for (i in 2:187) {
  beta[i,5] <- (beta[i,3]-beta[i-1,3])/beta[i-1,3]
}

beta$beta <- 0 # length tiap iterasi berkurang
for (i in 2:187) {
  beta[i,6] <- cov(beta[i:nrow(beta),4], beta[i:nrow(beta),5])/var(beta[i:nrow(beta),5])
}

beta$beta2 <- 0 # length tiap iterasi berkurang
beta <- beta[c(-1,-187),]
for (i in 1:185) {
  beta[i,7] <- cov(beta[-i,4], beta[-i,5])/var(beta[-i,5])
}
beta[-1,4]

# X2.2 : tingkat stress Pasar Forex (posisi rp scr internasional)  
kurs <- read.csv("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Kurs RP-USD/Data Historis USD_IDR.csv")
str(kurs$Perubahan.)
kurs$Perubahan. <- as.numeric(gsub(",", ".", gsub("%", "", kurs$Perubahan)))
kurs <- kurs[order(as.numeric(rownames(kurs)), decreasing = TRUE), ]
kurs$index <- seq_len(nrow(kurs))

library(readxl)
cadev <- as.data.frame(matrix(0,nrow = 187, ncol = 3)) # row, col
cadev.dat <- list(
  jan2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (1).xlsx"),
  feb2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (2).xlsx"),
  mar2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (3).xlsx"),
  apr2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (4).xlsx"),
  mei2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (5).xlsx"),
  jun2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (6).xlsx"),
  jul2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (7).xlsx"),
  agu2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (8).xlsx"),
  sep2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (9).xlsx"),
  okt2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (10).xlsx"),
  nov2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (11).xlsx"),
  des2010 <- read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2010/Indikator Moneter (12).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2011/Indikator Moneter (12).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2012/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2013/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2014/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2015/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2016/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2017/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2018/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2019/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2020/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2021/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2022/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2023/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (6).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (7).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (8).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (9).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (10).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2024/Indikator Moneter (11).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2025/Indikator Moneter.xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2025/Indikator Moneter (1).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2025/Indikator Moneter (2).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2025/Indikator Moneter (3).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2025/Indikator Moneter (4).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2025/Indikator Moneter (5).xlsx"),
  read_excel("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Devisa BI/2025/Indikator Moneter (6).xlsx")
  
)
cadev[,1] = seq.Date( from = as.Date("2010/1/1"), to= as.Date("2025/7/1"), by="month")
cadev[3,2] = as.numeric(cadev.dat[[3]][9,5])

for (j in 1:187) {
  cadev[j,2] = as.numeric(cadev.dat[[j]][9,5])
}

for (j in 2:187) {
  cadev[j,3] = (cadev[j,2]-cadev[j-1,2])/cadev[j-1,2]*100
}

empi <- data.frame(
  date = cadev[,1],
  delta_e = kurs[121:307,7],
  delta_res = cadev[,3]
)
empi <- empi[-c(1,187),]
empi$empi <- (empi$delta_e- mean(empi$delta_e))/sd(empi$delta_e)-(empi$delta_res- mean(empi$delta_res))/sd(empi$delta_res)


# X3.3 : volatilitas pasar ekuitas (using garch) 
ret_ihsg <- as.data.frame(matrix(0,nrow = 186, ncol = 2)) # row, col
for (i in 2:187) {
  ret_ihsg[i-1,2] <- (ihsg[i,2]-ihsg[i-1,2])/ihsg[i-1,2]
}
ret_ihsg[,1] <- seq.Date( from = as.Date("2010/1/1"), to= as.Date("2025/6/1"), by="month")
colnames(ret_ihsg) <- c("date", "return")

ggplot(ret_ihsg, aes(x = date, y = return)) + # plot
  geom_line() + # Line plot
  labs(title = "IHSG Monthly Return", x = "Date", y = "Value")

ts.plot(ret_ihsg[,2])
ts.plot(ret_ihsg$return, ylim=c(-0.25, 0.35))

mean(ret_ihsg$return)
sd(ret_ihsg$return)
ts.plot(rnorm(185, 0.006115846, 0.04045651), ylim=c(-0.25, 0.35), ylab="simulated return")

resid<-ret_ihsg[,2]-mean(ret_ihsg[,2])
arch1<-garch(resid, order=c(0, 1))
garch11<-garch(resid, order=c(1, 1))
summary(arch1)
summary(garch11)

# Extract conditional variance sigma(t)^2
sigma2_t <- garch11$fitted.values[,1]

# View first values
head(sigma2_t)

# X2.4 : pengembalian pasar saham (return) 
ret_ihsg$logret <- 0
for (i in 2:186) {
  ret_ihsg[i,3] <- log(ihsg[i+1,2]) - log(ihsg[i,2])
}

# X2.5 : Tekanan pasar utang 
yust <- read.csv("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Obl US/Data Historis Imbal Hasil Obligasi Amerika Serikat 10 Tahun.csv")
yust$Terakhir <- as.numeric(gsub(",", ".", yust$Terakhir))
yust <- yust[order(as.numeric(rownames(yust)), decreasing = TRUE), ]

yidgb <- read.csv("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/FSI_Obl ID/Data Historis Imbal Hasil Obligasi Indonesia 10 Tahun.csv")
yidgb$Terakhir <- as.numeric(gsub(",", ".", yidgb$Terakhir))
yidgb <- yidgb[order(as.numeric(rownames(yidgb)), decreasing = TRUE), ]

dmp <- as.data.frame(matrix(0, nrow = 185, ncol=1))
for (i in 1:185) {
  dmp[i,1] = yidgb[i+1,2] - yust[i+1,2]
}

mean(dmp[1,])
sd(dmp[1,])

FSI <- data.frame(
  date= seq.Date(from = as.Date("2010/2/1"), to= as.Date("2025/6/1"), by="month"),
  beta = beta[,7],
  empi = empi$empi,
  sig2 = sigma2_t[-1],
  sret = ret_ihsg[-1,3],
  dmp = dmp[,1]
)

## IMPORT INFLASI KELOMPOK
kel <- read_xlsx("C:/Users/ADELIA NUR ASMARIA/Documents/S2/Thesis/data/inflasi/Infkel.xlsx")
