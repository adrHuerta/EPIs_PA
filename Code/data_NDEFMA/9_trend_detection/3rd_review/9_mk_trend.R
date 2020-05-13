rm(list = ls())

setwd("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\9_trend_detection\\3rd_review")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(geoR)
library(lattice)
library(psych)
library(corrplot)
library(fume)
#library(zyp)

################# FIRST ANALYSIS: ALL DATA BEFORE PETTITT'S TEST ################


source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/g_legend.R')
source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/fdr.R')


load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\data.indices.Rdata")
load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\coordenadas.indices.Rdata")
ls()

colnames(data.indices[[1]]) == coordenadas.indices$COD1
colnames(data.indices[[7]]) == coordenadas.indices$COD1

ggplot() + 
  geom_point(data = coordenadas.indices, aes(x = XX, y = YY), size = 3)


n_estaciones <- dim(data.indices[[1]])[2]
names <- colnames(data.indices[[1]])

indice.trend <- data.indices
names.indices <- names(data.indices)

coor.correg <- coordenadas.indices[,c("NOM1","COD2", "XX", "YY")] 
#fdr(p.value.pettitt.oro, method = "original")
indice.results <- list()
mk.results <- as.data.frame(matrix(NA, nrow = n_estaciones, ncol = 7))
colnames(mk.results) <- c("Z","p.value","Zc","CORR_p.value","tau","N/N","Sen_slope")

for (j in 1:length(indice.trend)){
  for (i in 1:(n_estaciones)){
    xyz <- indice.trend[[j]][,i]
    mk.yue <- mkTrend(coredata(xyz))
    mk.results[i,] <- do.call("cbind", mk.yue)
  }
  mk.results.plot.2 <- data.frame(mk.results[,c("CORR_p.value","Sen_slope")], coor.correg, index = names.indices[j], FDR = p.adjust(mk.results[,c("CORR_p.value")],  method = c("BH")) )
  mk.results.plot.2 <- transform(mk.results.plot.2, figura = ifelse( Sen_slope > 0, 1, 
                                                                     ifelse( Sen_slope < 0, -1,
                                                                             0)), 
                                 significancia = ifelse( CORR_p.value < 0.05, 1, -1))
  indice.results[[j]] <-  mk.results.plot.2 
}
fdr.procedure.results <- lapply(indice.results, function(x){
  fdr(x[,c("CORR_p.value")], method = "original")})
indice.results <- do.call("rbind", indice.results)
write.csv(indice.results, "G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/3rd_review/data_NDEFMA/9_trend_detection/3rd_review/before_pettitt_test/MK.TEST.RESULTS.csv", quote = F)

################# SECOND ANALYSIS: ALL DATA AFTER PETTITT'S TEST ################
rm(list = ls())

source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/g_legend.R')
source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/fdr.R')

load("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/3rd_review/data_NDEFMA/8_break_detection/3rd_review/indices.aftP.Rdata")
ls()


n_estaciones <- dim(data.indices.aftP[[1]])[2]
names <- colnames(data.indices.aftP[[1]])

indice.trend <- data.indices.aftP
names.indices <- names(data.indices.aftP)

coor.correg <- coor.ind.aftP[,c("NOM1","COD2", "XX", "YY")] 

indice.results <- list()
mk.results <- as.data.frame(matrix(NA, nrow = n_estaciones, ncol = 7))
colnames(mk.results) <- c("Z","p.value","Zc","CORR_p.value","tau","N/N","Sen_slope")

for (j in 1:length(indice.trend)){
  for (i in 1:(n_estaciones)){
    xyz <- indice.trend[[j]][,i]
    mk.yue <- mkTrend(coredata(xyz))
    mk.results[i,] <- do.call("cbind", mk.yue)
  }
  mk.results.plot.2 <- data.frame(mk.results[,c("CORR_p.value","Sen_slope")], coor.correg, index = names.indices[j], FDR = p.adjust(mk.results[,c("CORR_p.value")],  method = c("BH")) )
  mk.results.plot.2 <- transform(mk.results.plot.2, figura = ifelse( Sen_slope > 0, 1, 
                                                                     ifelse( Sen_slope < 0, -1,
                                                                             0)), 
                                 significancia = ifelse( CORR_p.value < 0.05, 1, -1))
  indice.results[[j]] <-  mk.results.plot.2 
}
fdr.procedure.results <- lapply(indice.results, function(x){
  fdr(x[,c("CORR_p.value")], method = "original")})
indice.results <- do.call("rbind", indice.results)
write.csv(indice.results, "G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/3rd_review/data_NDEFMA/9_trend_detection/3rd_review/after_pettitt_test/MK.TEST.RESULTS.csv", quote = F)

