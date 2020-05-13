rm(list = ls())

setwd("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/4_extreme_computation/")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)


load("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/data_clean.RData")
load("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/pp_hydro_l_dataset.Rdata")
ls()

#############NEW####################

pp_hydro_l <- pp_hydro_l[, -match(c("880","776"),
                            colnames(pp_hydro_l))]

d.paper <- d.paper[, -match(c("880","776"),
                            colnames(d.paper))]

coordenadas.d.paper <- coordenadas.d.paper[-match(c("880","776"),
                                                  coordenadas.d.paper$COD1), ]

coordenadas.d.paper$COD1 == colnames(d.paper)
coordenadas.d.paper$COD1 == colnames(pp_hydro_l)


#####################################

source('/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/functions/funciones_indices_extremos_pp_v3_06092015.R')

########## EXTREMES COMPUTATION ################

#El periodo para todos los analisis es: 1971 - 2013. La climatolog?a: 1971 - 2000
pp_hydro_l <- window(pp_hydro_l, start = "1971-01-01", end = "2013-12-31") ##save(pp_hydro_l,file = "datos_paper_hydro.RData")
nyears <- 2013-1971+1
n_estaciones <- dim(pp_hydro_l)[2]

#---1. the total precipitation (PRCPTOT)-----
PRCPTOT.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  PRCPTOT.index[,i] <- apply.yearly(pp_hydro_l[,i], PRCPTOT)
}
colnames(PRCPTOT.index) <- colnames(pp_hydro_l)
PRCPTOT.index <- zoo(PRCPTOT.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.csv(PRCPTOT.index,"PRCPTOT.index.csv",quote=F)

#---3. the ratio of wet days (R1mm)--------- 
R11mm.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  R11mm.index[,i] <- apply.yearly(pp_hydro_l[,i], R11mm)
}
colnames(R11mm.index) <- colnames(pp_hydro_l)
R11mm.index <- zoo(R11mm.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.csv(R10mm.index,"R10mm.index.csv",quote=F)

#---4. the simple daily precipitation intensity (SDII)-------
SDII.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  SDII.index[,i] <- apply.yearly(pp_hydro_l[,i], SDII)
}
SDII.index <- zoo(SDII.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
colnames(SDII.index) <- colnames(pp_hydro_l)
#write.csv(SDII.index,"SDII.index.csv",quote=F)

#---5. the annual maximum precipitation (RX1DAY)-------
RX1DAY.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  RX1DAY.index[,i] <- apply.yearly(pp_hydro_l[,i], RX1DAY)
}
RX1DAY.index <- zoo(RX1DAY.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
colnames(RX1DAY.index) <- colnames(pp_hydro_l)
#write.csv(RX1DAY.index,"RX1DAY.index.csv",quote=F)

#---5.1 the annual maximum precipitation (RX1DAY)-------
RX5DAY.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  RX5DAY.index[,i] <- apply.yearly(pp_hydro_l[,i], RX5DAY)
}
RX5DAY.index <- zoo(RX5DAY.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
colnames(RX5DAY.index) <- colnames(pp_hydro_l)
#write.csv(RX5DAY.index,"RX5DAY.index.csv",quote=F)


#---6. fraction of the anual total precipitation above Prec95p (R95pTOT)-----
P95 <- NULL
R95pTOT.index <- matrix(nrow=nyears,ncol=n_estaciones)
for (i in 1:n_estaciones){
  time.serie <- pp_hydro_l[,i]
  ts_range <- window(time.serie,start=as.Date("1981-01-01"),end=as.Date("2010-12-31"))
  ts_range[ts_range < 1] <- NA #percentile of precipitation on wet days
  P95[i] <- as.numeric(quantile(ts_range, probs = c(0.95), na.rm = T))
  R95pTOT.index[,i] <- apply.yearly(time.serie, R95pTOT)
}
colnames(R95pTOT.index) <- colnames(pp_hydro_l)
R95pTOT.index <- zoo(R95pTOT.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.zoo(R95pTOT.index,"R95pTOT.index.csv",quote=F,sep=",")

#---7. the maximum length of dry spells (CDD)-------
CDD.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  CDD.index[,i] <- apply.yearly(pp_hydro_l[,i], CDD)
}
colnames(CDD.index) <- colnames(pp_hydro_l)
CDD.index <- zoo(CDD.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.csv(CDD.index,"CDD.index.csv",quote=F)

#---8. the mean length of dry spells (CDDm)-----
CDDm.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  CDDm.index[,i] <- apply.yearly(pp_hydro_l[,i], CDDm)
}
colnames(CDDm.index) <- colnames(pp_hydro_l)
CDDm.index <- zoo(CDDm.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.csv(CDDm.index,"CDDm.index.csv",quote=F)

#---9. the maximum length of wet spells (CWD)-------
CWD.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  CWD.index[,i] <- apply.yearly(pp_hydro_l[,i],CWD)
}
colnames(CWD.index) <- colnames(pp_hydro_l)
CWD.index <- zoo(CWD.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.csv(CWD.index,"CWD.index.csv",quote=F)

#----10. the mean length of wet spells (CWDm)-----
CWDm.index <- matrix(nrow = nyears, ncol = n_estaciones)
for(i in 1:n_estaciones){
  CWDm.index[,i] <- apply.yearly(pp_hydro_l[,i], CWDm)
}
colnames(CWDm.index) <- colnames(pp_hydro_l)
CWDm.index <- zoo(CWDm.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.csv(CWDm.index,"CWDm.index.csv",quote=F)

#---11. Number of events above Prec95p (R95p)----------
Px95 <- NULL
R95p.index <- matrix(nrow=nyears,ncol=n_estaciones)
for (i in 1:n_estaciones){
  time.serie <- pp_hydro_l[,i]
  ts_range <- window(time.serie, start = as.Date("1981-01-01"),end = as.Date("2010-12-31"))
  ts_range[ts_range < 1] <- NA #percentile of precipitation on wet days
  Px95[i] <- as.numeric(quantile(ts_range, probs = c(0.95), na.rm = T))
  R95p.index[,i] <- apply.yearly(time.serie, R95p)
}
colnames(R95p.index) <- colnames(pp_hydro_l)
R95p.index <- zoo(R95p.index, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))
#write.zoo(R95p.index,"R95p.index.csv",quote=F,sep=",")
# VIENDO EL PROMEDIO DE NUMERO DE EVENTOS QUE SOBREPASA :) SALE ESTO:
# MEAN = 2.185586
# MIN = 2
# MAX = 5

data.indices <- list(PRCPTOT.index, R11mm.index, SDII.index, RX1DAY.index, RX5DAY.index, R95pTOT.index, CDD.index, CDDm.index, CWD.index, CWDm.index, R95p.index)
name.indices <- c("PRCPTOT", "R1mm", "SDII", "RX1day", "RX5day", "R95pTOT", "CDD", "CDDm", "CWD", "CWDm", "R95p")
names(data.indices) <- name.indices

coordenadas.indices <- coordenadas.d.paper


###########################

coordenadas.indices.length <- cbind(coordenadas.indices, do.call("cbind", lapply(data.indices, function(x) apply(x, 2, 
                                                                     function(y) sum(!is.na(y))))))
write.csv(coordenadas.indices.length, "coordenadas.indices.length.csv")

##########################

save(data.indices, file = "data.indices.Rdata")
save(coordenadas.indices, file = "coordenadas.indices.Rdata")
