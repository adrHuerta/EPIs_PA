rm(list = ls())

setwd("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\8_break_detection\\3rd_review")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(geoR)
library(lattice)
library(psych)
library(corrplot)
library(trend)

source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/pettitt_test.R')
source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/fdr.R')

load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\data.indices.Rdata")
load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\coordenadas.indices.Rdata")
ls()

n_estaciones <- dim(data.indices[[1]])[2]
names <- colnames(data.indices[[1]])


# # CORR_p.value  Sen_slope	NOM1	COD2	XX	YY	index	FDR
# 0.03530386  2.85	CHUQUIBAMBILLA	764	-70.728333	-14.796389	PRCPTOT	0.264778952
# 0.019206966	-4.625714286	CAPAZO	158326	-69.745556	-17.186389	PRCPTOT	0.264778952
# 0.025259233	-0.357142857	HUARAYA MOHO	787	-69.491389	-15.389722	R1mm	0.232214754
# 0.030961967	-0.242424242	DESAGUADERO	883	-69.0404	-16.5688	R1mm	0.232214754

#---------------------------------ANALIZANSO PRCPTOT 1ero..-------------------------------------

indice.chg.point <-  coredata(data.indices$PRCPTOT)
df.pettitt.test1 <- list(); df.pettitt.test2 <- list(); resultados.pettitt <- list(); p.value.pettitt <- list() 
for (i in 1:n_estaciones){
  time <- c(1971:2013)
  resultado <- pettitt(cbind(time,indice.chg.point[,i]),0.05)
  resultados.pettitt[[i]] <- resultado  
  df <- cbind(as.data.frame(resultado$Xk), names[i])
  p.value.pettitt[[i]] <- resultado$pval
  df.pettitt.test1[[i]] <- df
  tt <- c(resultado$sig ,resultado$Xksign, resultado$XEa)
  if (length(tt) != 4) {
    df.pettitt.test2[[i]] <- c(tt, NA)
  } else { df.pettitt.test2[[i]] <- tt }
}
# names
pettitt.1 <- do.call("rbind", df.pettitt.test1); colnames(pettitt.1) <- c("YYYY", "VALUE", "rank","rank_cumsum","k", "Xk", "names")
pettitt.2 <- cbind(data.frame(do.call("rbind", df.pettitt.test2)), names); colnames(pettitt.2) <- c("SIG","Xksign", "XEa1", "XEa2", "names")

resto <- subset(pettitt.2, XEa2 != "NA")[,-c(2,3)]; colnames(resto) <- c("SIG", "XEa1", "names")
resumen.pettitt <- rbind(pettitt.2[,c("SIG", "XEa1", "names")], resto) 


ggplot(data = pettitt.1 , aes(x=YYYY, y = VALUE)) + geom_line(size=1) + geom_point(size=3) + theme_bw() + xlab("") + ylab("") +
  facet_wrap(~  names, ncol = 4)

ggplot(data = pettitt.1, aes(x=YYYY , y = Xk )) + geom_line(size=1) + theme_bw()  +   xlab("") + ylab("") +
  facet_wrap(~  names, ncol = 4)  + 
  geom_hline(data = pettitt.2 , aes(yintercept = c(Xksign)) ,linetype="dashed") +
  geom_hline(data = pettitt.2 , aes(yintercept = c(-Xksign)) ,linetype="dashed") + 
  geom_vline(data = pettitt.2, aes(xintercept = c(XEa1)), colour="green") +
  geom_vline(data = pettitt.2, aes(xintercept = c(XEa2)), colour="green") +
  ggtitle("PRCPTOT")

resto1 <- resumen.pettitt
resto1 <- transform(resto1, index = "PRCPTOT")

dotplot(data = resto1 , names ~ XEa1 | factor(index), groups=factor(SIG),
        pch = 20, cex = 3.5, xlim=c(1971,2013), xlab = "" ,col = c("black", "red"),
        scales=list(x=list(at = seq(1971, 2013, by = 6), labels=seq(1971,2013,6), cex= 1 ),
                    y=list(cex=0.8)),
        strip = strip.custom(par.strip.text = list(cex = 1.2, col= "black", font=2)),
        par.settings = list(layout.heights=list(strip=1), strip.background=list(col="gray")))

p.value.pettitt.oro <- do.call("rbind",p.value.pettitt)
write.csv(cbind(pettitt.2, p.value.pettitt.oro), "PRCPTOT_ORIGINAL_PVALOR.csv", quote = F, row.names = F)

#quitando la tendencia en aquellas estaciones con tendencia significante
#The stations where I found significant trend are:
#  PRPCTOT: CHUQUIBAMBILLA (764) y CAPAZO (158326)
#los datos son completados por el promedio y luego se le quita la tendencia
library(pracma)
est1 <- data.indices$PRCPTOT[,3]; est1.mean <- mean(est1, na.rm = T)
est1[is.na(est1)] <- est1.mean
coredata(data.indices$PRCPTOT)[,3] <-  ts(detrend(as.matrix(est1)), start = 1971,frequency = 1)
  
est2 <- data.indices$PRCPTOT[,15]; est2.mean <- mean(est2, na.rm = T)
est2[is.na(est2)] <- est2.mean
coredata(data.indices$PRCPTOT)[,15] <- ts(detrend(as.matrix(est2)), start = 1971,frequency = 1)



df.pettitt.test1 <- list(); df.pettitt.test2 <- list(); resultados.pettitt <- list(); p.value.pettitt <- list() 
for (i in 1:n_estaciones){
  time <- c(1971:2013)
  resultado <- pettitt(cbind(time, coredata(data.indices$PRCPTOT)[,i]),0.05)
  resultados.pettitt[[i]] <- resultado  
  df <- cbind(as.data.frame(resultado$Xk), names[i])
  p.value.pettitt[[i]] <- resultado$pval
  df.pettitt.test1[[i]] <- df
  tt <- c(resultado$sig ,resultado$Xksign, resultado$XEa)
  if (length(tt) != 4) {
    df.pettitt.test2[[i]] <- c(tt, NA)
  } else { df.pettitt.test2[[i]] <- tt }
}
# names
pettitt.1 <- do.call("rbind", df.pettitt.test1); colnames(pettitt.1) <- c("YYYY", "VALUE", "rank","rank_cumsum","k", "Xk", "names")
pettitt.2 <- cbind(data.frame(do.call("rbind", df.pettitt.test2)), names); colnames(pettitt.2) <- c("SIG","Xksign", "XEa1", "XEa2", "names")

resto <- subset(pettitt.2, XEa2 != "NA")[,-c(2,3)]; colnames(resto) <- c("SIG", "XEa1", "names")
resumen.pettitt <- rbind(pettitt.2[,c("SIG", "XEa1", "names")], resto) 

p.value.pettitt.aft.tend <- do.call("rbind",p.value.pettitt)
write.csv(cbind(pettitt.2, p.value.pettitt.aft.tend), "PRCPTOT_aftTREND_PVALOR.csv", quote = F, row.names = F)


resto1.1 <- resumen.pettitt
resto1.1 <- transform(resto1.1, index = "PRCPTOT")

dotplot(data = resto1.1 , names ~ XEa1 | factor(index), groups=factor(SIG),
        pch = 20, cex = 3.5, xlim=c(1971,2013), xlab = "" ,col = c("black", "red"),
        scales=list(x=list(at = seq(1971, 2013, by = 6), labels=seq(1971,2013,6), cex= 1 ),
                    y=list(cex=0.8)),
        strip = strip.custom(par.strip.text = list(cex = 1.2, col= "black", font=2)),
        par.settings = list(layout.heights=list(strip=1), strip.background=list(col="gray")))


#ANALISIS FDR 1er PRCPTOT 
FDR.salida.PRCPTOT_oro <- data.frame(FDR_oro = p.adjust(p.value.pettitt.oro,  method = c("BH")), nom = names)
FDR.salida.PRCPTOT_aft <- data.frame(FDR_tend = p.adjust(p.value.pettitt.aft.tend,  method = c("BH")), nom = names)
write.csv(FDR.salida.PRCPTOT_oro, "FDRoro_PRCPTOT.csv", quote = F)
write.csv( FDR.salida.PRCPTOT_aft, "FDRtrend_PRCPTOT.csv", quote = F)
fdr(p.value.pettitt.oro, method = "original")
fdr(p.value.pettitt.aft.tend, method = "original")

#FDR NO SIGNIFICANTE AMBOS :O



############################################################################################

rm(list = ls())

source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/pettitt_test.R')
source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/fdr.R')

load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\data.indices.Rdata")
load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\coordenadas.indices.Rdata")
ls()

n_estaciones <- dim(data.indices[[1]])[2]
names <- colnames(data.indices[[1]])

# # CORR_p.value  Sen_slope  NOM1	COD2	XX	YY	index	FDR
# 0.03530386  2.85	CHUQUIBAMBILLA	764	-70.728333	-14.796389	PRCPTOT	0.264778952
# 0.019206966	-4.625714286	CAPAZO	158326	-69.745556	-17.186389	PRCPTOT	0.264778952
# 0.025259233	-0.357142857	HUARAYA MOHO	787	-69.491389	-15.389722	R1mm	0.232214754
# 0.030961967	-0.242424242	DESAGUADERO	883	-69.0404	-16.5688	R1mm	0.232214754

#---------------------------------ANALIZANSO R1mm 2do.-------------------------------------

indice.chg.point <-  coredata(data.indices$R1mm)
df.pettitt.test1 <- list(); df.pettitt.test2 <- list(); resultados.pettitt <- list(); p.value.pettitt <- list() 
for (i in 1:n_estaciones){
  time <- c(1971:2013)
  resultado <- pettitt(cbind(time,indice.chg.point[,i]),0.05)
  resultados.pettitt[[i]] <- resultado  
  df <- cbind(as.data.frame(resultado$Xk), names[i])
  p.value.pettitt[[i]] <- resultado$pval
  df.pettitt.test1[[i]] <- df
  tt <- c(resultado$sig ,resultado$Xksign, resultado$XEa)
  if (length(tt) != 4) {
    df.pettitt.test2[[i]] <- c(tt, NA)
  } else { df.pettitt.test2[[i]] <- tt }
}
# names
pettitt.1 <- do.call("rbind", df.pettitt.test1); colnames(pettitt.1) <- c("YYYY", "VALUE", "rank","rank_cumsum","k", "Xk", "names")
pettitt.2 <- cbind(data.frame(do.call("rbind", df.pettitt.test2)), names); colnames(pettitt.2) <- c("SIG","Xksign", "XEa1", "XEa2", "names")

resto <- subset(pettitt.2, XEa2 != "NA")[,-c(2,3)]; colnames(resto) <- c("SIG", "XEa1", "names")
resumen.pettitt <- rbind(pettitt.2[,c("SIG", "XEa1", "names")], resto) 

ggplot(data = pettitt.1 , aes(x=YYYY, y = VALUE)) + geom_line(size=1) + geom_point(size=3) + theme_bw() + xlab("") + ylab("") +
  facet_wrap(~  names, ncol = 4)

ggplot(data = pettitt.1, aes(x=YYYY , y = Xk )) + geom_line(size=1) + theme_bw()  +   xlab("") + ylab("") +
  facet_wrap(~  names, ncol = 4)  + 
  geom_hline(data = pettitt.2 , aes(yintercept = c(Xksign)) ,linetype="dashed") +
  geom_hline(data = pettitt.2 , aes(yintercept = c(-Xksign)) ,linetype="dashed") + 
  geom_vline(data = pettitt.2, aes(xintercept = c(XEa1)), colour="green") +
  geom_vline(data = pettitt.2, aes(xintercept = c(XEa2)), colour="green") +
  ggtitle("R1mm")

p.value.pettitt.oro <- do.call("rbind",p.value.pettitt)
write.csv(cbind(pettitt.2, p.value.pettitt.oro), "R1mm_ORIGINAL_PVALOR.csv", quote = F, row.names = F)

resto2 <- resumen.pettitt
resto2 <- transform(resto2, index = "R1mm")

dotplot(data = resto2 , names ~ XEa1 | factor(index), groups=factor(SIG),
        pch = 20, cex = 3.5, xlim=c(1971,2013), xlab = "" ,col = c("black", "red"),
        scales=list(x=list(at = seq(1971, 2013, by = 6), labels=seq(1971,2013,6), cex= 1 ),
                    y=list(cex=0.8)),
        strip = strip.custom(par.strip.text = list(cex = 1.2, col= "black", font=2)),
        par.settings = list(layout.heights=list(strip=1), strip.background=list(col="gray")))


#quitando la tendencia en aquellas estaciones con tendencia significante
#The stations where I found significant trend are:
#R1mm: HUARAYA MOHO Y DESAGUADERO
est11 <- coredata(data.indices$R1mm)[,8]; est11.mean <- mean(est11, na.rm = T)
est11[is.na(est11)] <- est11.mean
coredata(data.indices$R1mm)[,8] <- ts(detrend(as.matrix(est11)), start = 1971,frequency = 1)

est1.2 <- coredata(data.indices$R1mm)[,13]; est1.2.mean <- mean(est1.2, na.rm = T)
est1.2[is.na(est1.2)] <- est1.2.mean
coredata(data.indices$R1mm)[,13] <- ts(detrend(as.matrix(est1.2)), start = 1971,frequency = 1)



df.pettitt.test1 <- list(); df.pettitt.test2 <- list(); resultados.pettitt <- list(); p.value.pettitt <- list() 
for (i in 1:n_estaciones){
  time <- c(1971:2013)
  resultado <- pettitt(cbind(time, coredata(data.indices$R1mm)[,i]),0.05)
  resultados.pettitt[[i]] <- resultado  
  df <- cbind(as.data.frame(resultado$Xk), names[i])
  p.value.pettitt[[i]] <- resultado$pval
  df.pettitt.test1[[i]] <- df
  tt <- c(resultado$sig ,resultado$Xksign, resultado$XEa)
  if (length(tt) != 4) {
    df.pettitt.test2[[i]] <- c(tt, NA)
  } else { df.pettitt.test2[[i]] <- tt }
}
# names
pettitt.1 <- do.call("rbind", df.pettitt.test1); colnames(pettitt.1) <- c("YYYY", "VALUE", "rank","rank_cumsum","k", "Xk", "names")
pettitt.2 <- cbind(data.frame(do.call("rbind", df.pettitt.test2)), names); colnames(pettitt.2) <- c("SIG","Xksign", "XEa1", "XEa2", "names")

resto <- subset(pettitt.2, XEa2 != "NA")[,-c(2,3)]; colnames(resto) <- c("SIG", "XEa1", "names")
resumen.pettitt <- rbind(pettitt.2[,c("SIG", "XEa1", "names")], resto) 

p.value.pettitt.aft.tend <- do.call("rbind",p.value.pettitt)
write.csv(cbind(pettitt.2, p.value.pettitt.aft.tend), "R1MM_aftTREND_PVALOR.csv", quote = F, row.names = F)


resto2.1 <- resumen.pettitt
resto2.1 <- transform(resto2.1, index = "R1mm")

dotplot(data = resto2.1 , names ~ XEa1 | factor(index), groups=factor(SIG),
        pch = 20, cex = 3.5, xlim=c(1971,2013), xlab = "" ,col = c("black", "red"),
        scales=list(x=list(at = seq(1971, 2013, by = 6), labels=seq(1971,2013,6), cex= 1 ),
                    y=list(cex=0.8)),
        strip = strip.custom(par.strip.text = list(cex = 1.2, col= "black", font=2)),
        par.settings = list(layout.heights=list(strip=1), strip.background=list(col="gray")))

# #ANALISIS FDR 2do R11mm
# FDR.salida.R11mm <- cbind(p.adjust(unlist(p.value.pettitt),  method = c("BH")), names)
FDR.salida.R1mm_oro <- data.frame(FDR_oro = p.adjust(p.value.pettitt.oro,  method = c("BH")), nom = names)
FDR.salida.R1mm_aft <- data.frame(FDR_tend = p.adjust(p.value.pettitt.aft.tend,  method = c("BH")), nom = names)
write.csv(FDR.salida.R1mm_oro, "FDRoro_R1mm.csv", quote = F)
write.csv(FDR.salida.R1mm_aft, "FDRtrend_R1mm.csv", quote = F)
fdr(p.value.pettitt.oro,method="original")
fdr(p.value.pettitt.aft.tend,method="original")

#FDR NO SIGNIFICANTE AMBOS :O

