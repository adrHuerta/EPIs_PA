rm(list = ls())

setwd("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/7_serial_correlation")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(geoR)
library(lattice)
library(psych)
library(corrplot)

source('/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/functions/corr.test.adr.R')


load("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/4_extreme_computation/data.indices.Rdata")
load("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/4_extreme_computation/coordenadas.indices.Rdata")
ls()
#
# data.indices <- data.indices[c("R1mm","PRCPTOT","SDII",
#                                "CWD","CWDm","CDD","CDDm",
#                                "RX1day","RX5day","R95pTOT","R95p")]
data.indices <- data.indices[c("R1mm","CWD","CWDm","CDD","CDDm",
                               "R95p","R95pTOT",
                               "RX1day","RX5day","SDII","PRCPTOT")]


data.indices.wind <- lapply(data.indices, function(x){
  res <- x
  return(res)
})

n_estaciones <- length(colnames(data.indices.wind[[1]]))
#names <- colnames(data.indices.wind[[1]])
names <- as.character(coordenadas.indices$NOM1)
colnames(data.indices.wind[[1]]) == coordenadas.indices$COD1

#--------------------AUTOCORRELATION----------------------------------
par(mfrow=c(3,7), mgp=c(2,0.6,0) ,mar=c(3,3,2,0.7), lty="solid" ,oma=c(1.5,1,1,1),las=1)


PRCPTOT.acf <- as.data.frame(matrix(NA, nrow = n_estaciones , ncol = 5))
colnames(PRCPTOT.acf) <- c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  #  jpeg(paste(names[i],"_PRCPTOT_",".jpg",sep=""))
  pp <- acf(as.vector(data.indices.wind$PRCPTOT[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a <- Box.test(as.numeric(data.indices.wind$PRCPTOT[,i]), lag = 1,type = c("Ljung-Box"))
  PRCPTOT.acf$STATISTIC[i]<-a$statistic
  PRCPTOT.acf$PARAM[i]<-a$parameter
  PRCPTOT.acf$P.VALUE[i]<-a$p.value
  PRCPTOT.acf$METHOD[i]<-a$method
  PRCPTOT.acf$DATA[i]<-a$data.name
}
rownames(PRCPTOT.acf) <- names
write.csv(PRCPTOT.acf,"PRCPTOT.acf.csv",quote=F)



R11mm.acf <- as.data.frame(matrix(NA,nrow = n_estaciones, ncol = 5))
colnames(R11mm.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$R1mm[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$R1mm[,i]), lag = 1,type = c("Ljung-Box"))
  R11mm.acf$STATISTIC[i]<-a$statistic
  R11mm.acf$PARAM[i]<-a$parameter
  R11mm.acf$P.VALUE[i]<-a$p.value
  R11mm.acf$METHOD[i]<-a$method
  R11mm.acf$DATA[i]<-a$data.name
}
rownames(R11mm.acf) <- names
write.csv(R11mm.acf,"R1mm.acf.csv",quote=F)


SDII.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(SDII.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$SDII[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$SDII[,i]), lag = 1,type = c("Ljung-Box"))
  SDII.acf$STATISTIC[i]<-a$statistic
  SDII.acf$PARAM[i]<-a$parameter
  SDII.acf$P.VALUE[i]<-a$p.value
  SDII.acf$METHOD[i]<-a$method
  SDII.acf$DATA[i]<-a$data.name
}
rownames(SDII.acf)<-names
write.csv(SDII.acf,"SDII.acf.csv",quote=F)


RX1DAY.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(RX1DAY.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$RX1day[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$RX1day[,i]), lag = 1,type = c("Ljung-Box"))
  RX1DAY.acf$STATISTIC[i]<-a$statistic
  RX1DAY.acf$PARAM[i]<-a$parameter
  RX1DAY.acf$P.VALUE[i]<-a$p.value
  RX1DAY.acf$METHOD[i]<-a$method
  RX1DAY.acf$DATA[i]<-a$data.name
}
rownames(RX1DAY.acf)<-names
write.csv(RX1DAY.acf,"RX1DAY.acf.csv",quote=F)

RX5DAY.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(RX5DAY.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$RX5day[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$RX5day[,i]), lag = 1,type = c("Ljung-Box"))
  RX5DAY.acf$STATISTIC[i]<-a$statistic
  RX5DAY.acf$PARAM[i]<-a$parameter
  RX5DAY.acf$P.VALUE[i]<-a$p.value
  RX5DAY.acf$METHOD[i]<-a$method
  RX5DAY.acf$DATA[i]<-a$data.name
}
rownames(RX5DAY.acf)<-names
write.csv(RX5DAY.acf,"RX5DAY.acf.csv",quote=F)


R95pTOT.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(R95pTOT.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$R95pTOT[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$R95pTOT[,i]), lag = 1,type = c("Ljung-Box"))
  R95pTOT.acf$STATISTIC[i]<-a$statistic
  R95pTOT.acf$PARAM[i]<-a$parameter
  R95pTOT.acf$P.VALUE[i]<-a$p.value
  R95pTOT.acf$METHOD[i]<-a$method
  R95pTOT.acf$DATA[i]<-a$data.name
}
rownames(R95pTOT.acf)<-names
write.csv(R95pTOT.acf,"R95pTOT.acf.csv",quote=F)


CDD.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(CDD.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$CDD[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$CDD[,i]), lag = 1,type = c("Ljung-Box"))
  CDD.acf$STATISTIC[i]<-a$statistic
  CDD.acf$PARAM[i]<-a$parameter
  CDD.acf$P.VALUE[i]<-a$p.value
  CDD.acf$METHOD[i]<-a$method
  CDD.acf$DATA[i]<-a$data.name
}
rownames(CDD.acf)<-names
write.csv(CDD.acf,"CDD.acf.csv",quote=F)

CDDm.acf <- as.data.frame(matrix(NA,nrow = n_estaciones,ncol = 5))
colnames(CDDm.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$CDDm[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a <- Box.test(as.numeric(data.indices.wind$CDDm[,i]), lag = 1,type = c("Ljung-Box"))
  CDDm.acf$STATISTIC[i] <- a$statistic
  CDDm.acf$PARAM[i] <- a$parameter
  CDDm.acf$P.VALUE[i] <- a$p.value
  CDDm.acf$METHOD[i] <- a$method
  CDDm.acf$DATA[i] <- a$data.name
}
rownames(CDDm.acf) <- names
write.csv(CDDm.acf,"CDDm.acf.csv", quote=F)


CWD.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(CWD.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$CWD[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$CWD[,i]), lag = 1,type = c("Ljung-Box"))
  CWD.acf$STATISTIC[i]<-a$statistic
  CWD.acf$PARAM[i]<-a$parameter
  CWD.acf$P.VALUE[i]<-a$p.value
  CWD.acf$METHOD[i]<-a$method
  CWD.acf$DATA[i]<-a$data.name
}
rownames(CWD.acf)<-names
write.csv(CWD.acf,"CWD.acf.csv",quote=F)

CWDm.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(CWDm.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$CWDm[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$CWDm[,i]), lag = 1,type = c("Ljung-Box"))
  CWDm.acf$STATISTIC[i]<-a$statistic
  CWDm.acf$PARAM[i]<-a$parameter
  CWDm.acf$P.VALUE[i]<-a$p.value
  CWDm.acf$METHOD[i]<-a$method
  CWDm.acf$DATA[i]<-a$data.name
}
rownames(CWDm.acf)<-names
write.csv(CWDm.acf,"CWDm.acf.csv",quote=F)

R95p.acf<-as.data.frame(matrix(NA,nrow=n_estaciones,ncol=5))
colnames(R95p.acf)<-c("STATISTIC","PARAM","P.VALUE","METHOD","DATA")
for (i in 1:n_estaciones){
  
  pp <- acf(as.vector(data.indices.wind$R95p[,i]), ci.type = "ma",lag.max=20, na.action=na.pass, ylim = c(-1, 1), main = "", xlab = "", xaxt='n', plot = FALSE)
  plot(pp, ci.type = "ma" , ylim = c(-1, 1), main = "", xlab = "", ylab = "", type = "o")
  grid()
  title(names[i], line = 1)
  mtext("lag",cex = 0.7 , side = 1, line = 1.5) 
  mtext("R",cex = 0.7 , side = 2, line = 2.4) 
  
  a<-Box.test(as.numeric(data.indices.wind$R95p[,i]), lag = 1,type = c("Ljung-Box"))
  R95p.acf$STATISTIC[i]<-a$statistic
  R95p.acf$PARAM[i]<-a$parameter
  R95p.acf$P.VALUE[i]<-a$p.value
  R95p.acf$METHOD[i]<-a$method
  R95p.acf$DATA[i]<-a$data.name
}
rownames(R95p.acf)<-names
write.csv(R95p.acf,"R95p.acf.csv",quote=F)

# c("R1mm","CWD","CWDm","CDD","CDDm",
#   "R95p","R95pTOT",
#   "RX1day","RX5day","SDII","PRCPTOT")

resumen.acf <- cbind(R11mm.acf$P.VALUE,
                     CWD.acf$P.VALUE,
                     CWDm.acf$P.VALUE,
                     CDD.acf$P.VALUE,
                     CDDm.acf$P.VALUE,
                     R95p.acf$P.VALUE,
                     R95pTOT.acf$P.VALUE,
                     RX1DAY.acf$P.VALUE,
                     RX5DAY.acf$P.VALUE,
                     SDII.acf$P.VALUE,
                     PRCPTOT.acf$P.VALUE)
                     


#colnames(resumen.acf) <- c("PRCPTOT","R1mm","SDII","RX1day","RX5day","R95pTOT","CDD","CDDm","CWD","CWDm","R95p")   #,"R95p")
# colnames(resumen.acf) <- c("R1mm","PRCPTOT","SDII",
#                            "CWD","CWDm","CDD","CDDm",
#                            "RX1day","RX5day","R95pTOT","R95p")
colnames(resumen.acf) <- c("R1mm","CWD","CWDm","CDD","CDDm",
                           "R95p","R95pTOT",
                           "RX1day","RX5day","SDII","PRCPTOT")

rownames(resumen.acf) <- names
resumen.acf[resumen.acf>0.05] <- NA; resumen.acf[!is.na(resumen.acf)] <- 1
for (i in 1:11){  #AQUI SE MODIFICO
  resumen.acf[,i][resumen.acf[,i]==1]<-1*i}

#plot de a?os donde las estaciones tiene al menos el 95% de datos
dotplot(resumen.acf, pch = 20, col = "black", xlab = "Extreme indices", ylab = "Stations (Code)",
        cex = 2, scales = list( x = list(at=c(1,2,3,4,5,6,7,8,9,10,11),labels=colnames(resumen.acf))), 
        xlim = c(0.5, 11.5))


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
#
rownames(resumen.acf) <- as.character(sapply(tolower(rownames(resumen.acf)), simpleCap))

resumen.adr <- reshape2::melt(resumen.acf)
resumen.adr$Var1 <- factor(resumen.adr$Var1, levels = rev(levels(resumen.adr$Var1)))
resumen.adr$value[!is.na(resumen.adr$value)] <- 1
ggplot(resumen.adr) + 
  geom_point(aes(Var2,Var1, shape = factor(value)), size = 4) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, hjust = 1, colour = "black")) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1, colour = "black")) + 
  xlab("") + ylab("Stations")  + theme(legend.position="none") + 
  theme(panel.border = element_rect(colour = "black")) + 
  theme(panel.grid.minor = element_line(colour="gray", size=0.5))
  
ggsave("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_V6/IJC/figures_2/Figure_02.pdf", 
       width = 5, height = 3)


