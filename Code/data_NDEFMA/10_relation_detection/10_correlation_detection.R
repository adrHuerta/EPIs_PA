rm(list = ls())
setwd("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/5th_review/data_NDEFMA/10_relation_detection")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(lattice)
library(scales)

source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/g_legend.R')
source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/fdr.R')

load("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/5th_review/data_NDEFMA/8_break_detection/3rd_review/indices.aftP.Rdata")
ls()

coordenadas.indices <- coor.ind.aftP

ggplot() + 
  geom_point(data = coordenadas.indices, aes(x = XX, y = YY), size = 8)

n_estaciones <- dim(data.indices.aftP[[1]])[2]
#names <- colnames(data.indices.aftP[[1]])
names <- as.character(coordenadas.indices$NOM1)
colnames(data.indices.aftP[[1]]) == coordenadas.indices$COD1

data.indices.aftP <- lapply(data.indices.aftP, function(x){
  colnames(x) <- as.character(coordenadas.indices$NOM1)
  return(x)
  })

data.indices.aftP <- data.indices.aftP[c("R1mm","CWD","CWDm","CDD","CDDm",
                                         "R95p","R95pTOT",
                                         "RX1day","RX5day","SDII","PRCPTOT")]


#----------------------- INDICES DE PATRONES DE CIRCULACIÓN------------

#datos de los indices de circulacion
load("modos_Indices_5_Trencorr.rDat")

#data frames en "modos_Indices.rDat", con los modos en formato y el periodo 1971-2013
indices.anual 
indices.hydro
indices.no.hydro
indices.verano
indices.invierno

#"PRCPTOT","R10mm","SDII","RX1DAY","CDD","CWD","R95pTOT","R95p"
#indices anual   E.index    C.index  MEI.index  SOI.index TSA.index.corr
#list(PRCPTOT.index[,-c(19)], RX1DAY.index[,-c(19)], CDD.index[,-c(19)] ,CWD.index[,-c(19)])#, R95p.index)

indice <- list(indices.anual, indices.hydro, indices.no.hydro, indices.verano, indices.invierno)
indice <- lapply(indice, function(x){
  #x[,4] <- -x[,4]
  colnames(x) <- c("Ei","Ci","MEi","SOi","TNAi","TSAi","TN_TSi","PDOi")
  CEi <- x[,2] - x[,1]
  dfr <- cbind(x[,1], x[,2], CEi,  x[,3],  x[,4],  x[,5],  x[,6],  x[,7],  x[,8])
  colnames(dfr) <- c("Ei","Ci","C_Ei","MEi","SOi","TNAi","TSAi","TN_TSi","PDOi")
  return(dfr)
  })

names(indice) <- c("A","H","noH","V","I")

# ####################################
library(pracma)

est1 <- data.indices.aftP$R1mm[,c("HUARAYA MOHO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$R1mm[,c("HUARAYA MOHO")]) == T)] <- NA
coredata(data.indices.aftP$R1mm)[,c("HUARAYA MOHO")] <-  est1.d


est1 <- data.indices.aftP$R1mm[,c("DESAGUADERO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$R1mm[,c("DESAGUADERO")]) == T)] <- NA
coredata(data.indices.aftP$R1mm)[,c("DESAGUADERO")] <-  est1.d


est1 <- data.indices.aftP$CWD[,c("CHUQUIBAMBILLA")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$CWD[,c("CHUQUIBAMBILLA")]) == T)] <- NA
coredata(data.indices.aftP$CWD)[,c("CHUQUIBAMBILLA")] <- est1.d


est1 <- data.indices.aftP$CWDm[,c("CHUQUIBAMBILLA")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$CWDm[,c("CHUQUIBAMBILLA")]) == T)] <- NA
coredata(data.indices.aftP$CWDm)[,c("CHUQUIBAMBILLA")] <-  est1.d
  
  
est1 <- data.indices.aftP$CWDm[,c("HUARAYA MOHO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$CWDm[,c("HUARAYA MOHO")]) == T)] <- NA
coredata(data.indices.aftP$CWDm)[,c("HUARAYA MOHO")] <-  est1.d


est1 <- data.indices.aftP$CWDm[,c("DESAGUADERO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$CWDm[,c("DESAGUADERO")]) == T)] <- NA
coredata(data.indices.aftP$CWDm)[,c("DESAGUADERO")] <- est1.d



est1 <- data.indices.aftP$CDD[,c("CAPACHICA")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$CDD[,c("CAPACHICA")]) == T)] <- NA
coredata(data.indices.aftP$CDD)[,c("CAPACHICA")] <-  est1.d

est1 <- data.indices.aftP$CDD[,c("MAZO CRUZ")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$CDD[,c("MAZO CRUZ")]) == T)] <- NA
coredata(data.indices.aftP$CDD)[,c("MAZO CRUZ")] <-  est1.d



est1 <- data.indices.aftP$CDDm[,c("HUARAYA MOHO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
#est1.d[which(is.na(data.indices.aftP$CDDm[,c("HUARAYA MOHO")]) == T)] <- NA
coredata(data.indices.aftP$CDDm)[,c("HUARAYA MOHO")] <-  est1.d

est1 <- data.indices.aftP$CDDm[,c("MAZO CRUZ")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
#est1.d[which(is.na(data.indices.aftP$CDDm[,c("MAZO CRUZ")]) == T)] <- NA
coredata(data.indices.aftP$CDDm)[,c("MAZO CRUZ")] <-  est1.d

est1 <- data.indices.aftP$CDDm[,c("CAPAZO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
#est1.d[which(is.na(data.indices.aftP$CDDm[,c("CAPAZO")]) == T)] <- NA
coredata(data.indices.aftP$CDDm)[,c("CAPAZO")] <- est1.d


######


est1 <- data.indices.aftP$R95p[,c("CABANILLAS")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$R95p[,c("CABANILLAS")]) == T)] <- NA
coredata(data.indices.aftP$R95p)[,c("CABANILLAS")] <- est1.d

est1 <- data.indices.aftP$R95p[,c("CAPAZO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$R95p[,c("CAPAZO")]) == T)] <- NA
coredata(data.indices.aftP$R95p)[,c("CAPAZO")] <-  est1.d


est1 <- data.indices.aftP$R95pTOT[,c("CABANILLAS")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$R95pTOT[,c("CABANILLAS")]) == T)] <- NA
coredata(data.indices.aftP$R95pTOT)[,c("CABANILLAS")] <-  est1.d

est1 <- data.indices.aftP$R95pTOT[,c("CAPAZO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$R95pTOT[,c("CAPAZO")]) == T)] <- NA
coredata(data.indices.aftP$R95pTOT)[,c("CAPAZO")] <-  est1.d




######

est1 <- data.indices.aftP$RX1day[,c("CABANILLAS")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$RX1day[,c("CABANILLAS")]) == T)] <- NA
coredata(data.indices.aftP$RX1day)[,c("CABANILLAS")] <- est1.d

est1 <- data.indices.aftP$RX1day[,c("CAPAZO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$RX1day[,c("CAPAZO")]) == T)] <- NA
coredata(data.indices.aftP$RX1day)[,c("CAPAZO")] <-  est1.d



est1 <- data.indices.aftP$RX5day[,c("HUANCANE")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$RX5day[,c("HUANCANE")]) == T)] <- NA
coredata(data.indices.aftP$RX5day)[,c("HUANCANE")] <-  est1

est1 <- data.indices.aftP$RX5day[,c("PUTINA")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$RX5day[,c("PUTINA")]) == T)] <- NA
coredata(data.indices.aftP$RX5day)[,c("PUTINA")] <- est1.d

est1 <- data.indices.aftP$RX5day[,c("CAPAZO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$RX5day[,c("CAPAZO")]) == T)] <- NA
coredata(data.indices.aftP$RX5day)[,c("CAPAZO")] <-  est1.d



est1 <- data.indices.aftP$SDII[,c("TARACO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$SDII[,c("TARACO")]) == T)] <- NA
coredata(data.indices.aftP$SDII)[,c("TARACO")] <-  est1.d

est1 <- data.indices.aftP$SDII[,c("LARAQUERI")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$SDII[,c("LARAQUERI")]) == T)] <- NA
coredata(data.indices.aftP$SDII)[,c("LARAQUERI")] <- est1.d

est1 <- data.indices.aftP$SDII[,c("CAPAZO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$SDII[,c("CAPAZO")]) == T)] <- NA
coredata(data.indices.aftP$SDII)[,c("CAPAZO")] <-  est1.d



est1 <- data.indices.aftP$PRCPTOT[,c("CHUQUIBAMBILLA")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$PRCPTOT[,c("CHUQUIBAMBILLA")]) == T)] <- NA
coredata(data.indices.aftP$PRCPTOT)[,c("CHUQUIBAMBILLA")] <- est1.d

est1 <- data.indices.aftP$PRCPTOT[,c("CAPAZO")]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
est1.d <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
est1.d[which(is.na(data.indices.aftP$PRCPTOT[,c("CAPAZO")]) == T)] <- NA
coredata(data.indices.aftP$PRCPTOT)[,c("CAPAZO")] <- est1.d


####################################

# seleccion.index.pp <- data.indices.aftP$R95p
# adrt <- "R95p_TNC.csv"

for(z in names(data.indices.aftP)){
  
  seleccion.index.pp <- data.indices.aftP[[z]]
  adrt <- paste(z,"_TNC",".csv", sep = "")
  
  
  index.cor.test <- matrix(NA,ncol=3,nrow=(n_estaciones)); index.cor.test <- as.data.frame(index.cor.test); colnames(index.cor.test) <- c("cor","pvalue","sig")
  index.result <- list()
  index.result.plot <- list()
  res <- list()
  for(z in 1:5){ #5 tipos de ponderacion
    
    ttt <- indice[[z]]
    names.modos <- colnames(ttt)
    
    for(h in 1:9){ #8 indices climaticos
      
      y <- ttt[,h]
      
      
      for(i in 1:15){ #15 estaciones
        
        x <- seleccion.index.pp[,i]
        correlation <-  cor.test(as.numeric(x), as.numeric(y), method = c("spearman"))
        index.cor.test$cor[i] <- correlation$estimate
        index.cor.test$pvalue[i] <- correlation$p.value
        
        if (correlation$p.value >= 0.05){
          index.cor.test$sig[i] <- "ns"
        } else  { index.cor.test$sig[i] <- "s"}
        
      }
      
      rownames(index.cor.test) <- colnames(seleccion.index.pp)
      df.table <- cbind(coordenadas.indices[,c(5,6,7)] , index.cor.test, names.modos[h], p.adjust(index.cor.test$pvalue,  method = c("BH")))
      colnames(df.table) <- c("NOMBRE","LONGITUD","LATITUD","cor","pvalue","sig","names.modos","p.value.FDR")
      index.result[[h]] <- df.table
      
    }
    
    index.result2 <- do.call("rbind",  index.result) 
    res[[z]] <- data.frame(index.result2, type = names(indice)[z])
    
  }
  resultados.cor <- do.call("rbind",  res) 
  resultados.cor <- transform(resultados.cor, figura = ifelse( cor > 0, 1, 
                                                               ifelse( cor < 0, -1,
                                                                       0)))
  resultados.cor <- transform(resultados.cor, color.fill = ifelse( sig == "s" & figura == 1, 11,
                                                                   ifelse( sig == "s" & figura == -1, -11,0)))
  write.csv(resultados.cor, adrt, quote = F)
  
  
  
  
  
  
}







###########################################################

data.sel <- data.indices.aftP$CWD
data.sel <- data.sel[,c("708","762","779", "878","889","158310")]
plot(data.sel, plot.type = "single")

library(lattice)
library(latticeExtra)

data.sela <- apply(data.sel, 1, mean, na.rm = T)
data.sela <- zoo(data.sela, time(data.sel))

x1 <- xyplot(data.sela, col = "gray", screen = 1, lwd = 3)

xf <- indices.hydro[,2]
xf <- zoo(xf, seq(as.Date("1971-01-01"), as.Date("2013-01-01"), by = "year"))

x2 <- xyplot(xf , col = "black", screen = 1, lwd = 3)
doubleYScale(x1,x2)

##

data.sela.anom <- data.sela - mean(data.sela, na.rm = T)
data.xf.anom <- xf - mean(xf, na.rm = T)

xx <- xyplot(data.sela.anom, col = "blue", screen = 1, lwd = 5, ylim = c(-15,15), panel = function(...) {
  panel.abline(v = seq(0,365*45, 365), lty = "dotted", col = "black")
  panel.abline(h = c(-sd(data.sela.anom)*1.4 ,sd(data.sela.anom)*1.4), col = "black", lwd = 3)
  panel.xyplot(...)
})
yy <- xyplot(data.xf.anom, col = "red", screen = 1, lwd = 5, ylim = c(-2,2))

doubleYScale(xx, yy)

#+red -blue:  1973, 1983, 1987, 1992, 1998, 2003, 2007, 2010
#+blue -red: 1974, 1984, 1989,1999, 2009, 2012

#+ 1974, 1984, 2012
#- 1983, 1992, 1998
