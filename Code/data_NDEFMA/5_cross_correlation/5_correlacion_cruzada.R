rm(list = ls())

setwd("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/5_cross_correlation")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(geoR)
library(lattice)
library(psych)
library(corrplot)

source('G:/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/corr.test.adr.R')


load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\data.indices.Rdata")
load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\coordenadas.indices.Rdata")
ls()

# data.indices <- data.indices[c("R1mm","PRCPTOT","SDII",
#                                "CWD","CWDm","CDD","CDDm",
#                                "RX1day","RX5day","R95pTOT","R95p")]
data.indices <- data.indices[c("R1mm","CWD","CWDm","CDD","CDDm",
                               "R95p","R95pTOT",
                               "RX1day","RX5day","SDII","PRCPTOT")]

data.indices.wind <- lapply(data.indices, function(x){
  res <- window(x, start = "1971-01-01", end = "2013-01-01")
  return(res)
})


n_estaciones <- length(colnames(data.indices.wind[[1]]))
names <- colnames(data.indices.wind[[1]])


#----------------- MATRIX DE CORRELACION ----------------------------------
par(mfrow=c(3,2), mgp=c(1,1,1) ,mar=c(3,3,2,4), lty="solid" ,oma=c(1.5,1,1,1),las=1)

PRCPTOT.index.wind <- coredata(data.indices.wind$PRCPTOT)
correlation <- corr.test.adr(PRCPTOT.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.PRCPTOT.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.PRCPTOT.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("PRCPTOT", cex = 0.5 ,line = -4, side = 3)
dev.off()

R11mm.index.wind <- coredata(data.indices.wind$R1mm)
correlation<-corr.test.adr(R11mm.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.R11mm.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.R11mm.index.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("R1mm", cex = 0.5 ,line = -4, side = 3)
dev.off()

SDII.index.wind<-coredata(data.indices.wind$SDII)
correlation<-corr.test.adr(SDII.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.SDII.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.SDII.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("SDII", cex = 0.5 ,line = -4, side = 3)
dev.off()

RX1DAY.index.wind<-coredata(data.indices.wind$RX1day)
correlation<-corr.test.adr(RX1DAY.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.RX1DAY.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.RX1DAY.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("RX1DAY", cex = 0.5 ,line = -4, side = 3)
dev.off()

RX5DAY.index.wind<-coredata(data.indices.wind$RX5day)
correlation<-corr.test.adr(RX5DAY.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.RX5DAY.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.RX5DAY.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("RX5DAY", cex = 0.5 ,line = -4, side = 3)
dev.off()

R95pTOT.index.wind<-coredata(data.indices.wind$R95pTOT)
correlation<-corr.test.adr(R95pTOT.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.R95pTOT.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.R95pTOT.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("R95pTOT", cex = 0.5 ,line = -4, side = 3)
dev.off()

CDD.index.wind<-coredata(data.indices.wind$CDD)
correlation<-corr.test.adr(CDD.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.CDD.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.CDD.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("CDD", cex = 0.5 ,line = -4, side = 3)
dev.off()

CDDm.index.wind <- coredata(data.indices.wind$CDDm)
correlation <- corr.test.adr(CDDm.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.CDDm.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.CDDm.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("CDDm", cex = 0.5 ,line = -4, side = 3)
dev.off()

CWD.index.wind<- coredata(data.indices.wind$CWD)
correlation<-corr.test.adr(CWD.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.CWD.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.CWD.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("CWD", cex = 0.5 ,line = -4, side = 3)
dev.off()

CWDm.index.wind <- coredata(data.indices.wind$CWDm)
correlation <- corr.test.adr(CWDm.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.CWDm.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.CWDm.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("CWDm", cex = 0.5 ,line = -4, side = 3)
dev.off()

R95p.index.wind <- coredata(data.indices.wind$R95p)
correlation <- corr.test.adr(R95p.index.wind,use="pairwise",method=("spearman"),alpha=.05)
write.csv(correlation$r,"corr.R95p.csv",quote=F) #the original p values are reported below the diagonal and the adjusted above the diagonal
pdf("corr.R95p.pdf",paper="a4","landscape")
corrplot(correlation$r, p.mat = correlation$p, sig.level = 0.05,insig = "blank",tl.cex=0.3,type="lower",cl.ratio=0.15,tl.srt=0.1,tl.col = "black")
mtext("R95p", cex = 0.5 ,line = -4, side = 3)
dev.off()
