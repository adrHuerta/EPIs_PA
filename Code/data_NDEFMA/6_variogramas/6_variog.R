rm(list = ls())

setwd("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/6_variogramas")
library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(geoR)
library(lattice)
library(sp)
library(rgdal)


#load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\data.indices.Rdata")

load("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/4_extreme_computation/data.indices.Rdata")
load("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/4_extreme_computation/coordenadas.indices.Rdata")
ls()

# data.indices <- data.indices[c("R1mm","PRCPTOT","SDII",
#                                "CWD","CWDm","CDD","CDDm",
#                                "RX1day","RX5day","R95pTOT","R95p")]
data.indices <- data.indices[c("R1mm","CWD","CWDm","CDD","CDDm",
                               "R95p","R95pTOT",
                               "RX1day","RX5day","SDII","PRCPTOT")]

coordenadas.indices$COD1 == colnames(data.indices$R1mm)
coordenadas.indices$COD1 == colnames(data.indices$RX1day)

data.indices.wind <- lapply(data.indices, function(x){
  res <- window(x, start = "1971-01-01", end = "2013-01-01")
  return(res)
})

n_estaciones <- length(colnames(data.indices.wind[[1]]))
names <- colnames(data.indices.wind[[1]])


utm<-"+init=epsg:32719"# EPSG:32718: WGS 84 / UTM zone 18S
wgs<-"+proj=longlat +datum=WGS84"


dd.points <- SpatialPoints(coordenadas.indices[,c("XX", "YY")])
proj4string(dd.points) <- CRS(wgs)
dd.points.utm <- spTransform(dd.points, CRS(utm))
cordenadas.selec <- data.frame(coordenadas.indices,LONGITUD = as.data.frame(dd.points.utm)[,1], LATITUD = as.data.frame(dd.points.utm)[,2]) 

#-------------------- VARIOGRAMAS ----------------------------

lon.lat <- cordenadas.selec[,c("LONGITUD","LATITUD")]/1000

data.variog <- lapply(data.indices.wind, function(x){
  x <- t(as.data.frame(x))
  res <- cbind(lon.lat, x)
  colnames(res) <- c("LONGITUD","LATITUD", seq(1:dim(data.indices.wind[[1]])[1]))
  return(res)
  })
# 
# data1 <- cbind(lon.lat,t(as.data.frame(PRCPTOT.index.wind)))
# data2 <- cbind(lon.lat,t(as.data.frame(R11mm.index.wind)))
# data3 <- cbind(lon.lat,t(as.data.frame(R10mm.index.wind)))
# data4 <- cbind(lon.lat,t(as.data.frame(SDII.index.wind)))
# data5 <- cbind(lon.lat,t(as.data.frame(RX1DAY.index.wind)))
# data6 <- cbind(lon.lat,t(as.data.frame(R95pTOT.index.wind)))
# data7 <- cbind(lon.lat,t(as.data.frame(CDD.index.wind)))
# data8 <- cbind(lon.lat,t(as.data.frame(CDDm.index.wind)))
# data9 <- cbind(lon.lat,t(as.data.frame(CWD.index.wind)))
# data10 <- cbind(lon.lat,t(as.data.frame(CWDm.index.wind)))
# data11 <- cbind(lon.lat,t(as.data.frame(R95p.index.wind)))

# 
# lista1<-list();lista2<-list();lista3<-list();lista4<-list();lista5<-list();lista6<-list();lista7<-list();lista8<-list();lista9<-list();lista10<-list();lista11<-list()
# for (i in 1:30){
#   lista1[[i]]<-as.geodata(data1[,c("LONGITUD","LATITUD",as.character(i))])
#   lista2[[i]]<-as.geodata(data2[,c("LONGITUD","LATITUD",as.character(i))])
#   lista3[[i]]<-as.geodata(data3[,c("LONGITUD","LATITUD",as.character(i))])
#   lista4[[i]]<-as.geodata(data4[,c("LONGITUD","LATITUD",as.character(i))])
#   lista5[[i]]<-as.geodata(data5[,c("LONGITUD","LATITUD",as.character(i))])
#   lista6[[i]]<-as.geodata(data6[,c("LONGITUD","LATITUD",as.character(i))])
#   lista7[[i]]<-as.geodata(data7[,c("LONGITUD","LATITUD",as.character(i))])
#   lista8[[i]]<-as.geodata(data8[,c("LONGITUD","LATITUD",as.character(i))])
#   lista9[[i]]<-as.geodata(data8[,c("LONGITUD","LATITUD",as.character(i))])
#   lista10[[i]]<-as.geodata(data8[,c("LONGITUD","LATITUD",as.character(i))])
#   lista11[[i]]<-as.geodata(data8[,c("LONGITUD","LATITUD",as.character(i))])
# }
data.variog.geo <- lapply(data.variog, function(x){
  res <- list()
  for(i in 1:dim(data.indices.wind[[1]])[1]){
    res[[i]] <- as.geodata(x[,c("LONGITUD","LATITUD",as.character(i))])
    }
  return(res)
  })


cairo_pdf("/media/adrian/TOSHIBA EXT/TRABAJOS/DECADE/paper_DECADE/paper_V6/IJC/figures_2/Figure_03.pdf", width = 7, height = 5)


par(mfrow=c(4,3), mgp=c(1.5,0.6,0) ,mar=c(2.5,2.7,1.3,0.7), lty="solid" ,oma=c(1.5,1,1,1),las=1)
#i = 6
for(i in 1:11){

aft <- data.variog.geo[[i]]
length(aft)
pl.bs <- variog(aft[[1]], option = "bin", breaks = seq(0,300,50) )#, max.dist=300)
plot(pl.bs, scaled = T , type = "n", xaxt='n',  yaxt='n',ylab = "Y(h)" , xlab = "Distance (km)", 
     ylim = c(0,4), xlim = c(30,265), cex.lab = 0.9)
grid()
par(new = T)
plot(pl.bs, type="l",col = "gray", main = names(data.variog.geo)[i], 
     scaled = T, lwd = 0.5, ylim = c(0,4),ylab = "" , xlab = "", 
     xlim = c(30,265))

#aft[[1]] <- NULL
lapply(aft, function(x){
  res <- variog(x, option = "bin", breaks = seq(0,300,50))#, max.dist = 300)
  lines(res ,type = "l" , col = "gray", scaled = T, lwd = 0.5)
  })
# i_low= c(6)
# i_hi = c(38)
# i=6
# for(i in 36:40){
# 
#   res <- variog(aft[[i]], option = "bin")#, breaks = seq(0,300,50))#, max.dist = 300)
#   # vario.0 <- variog(aft[[i]],  dir=2.5)
#   # vario.0 <- variog(aft[[i]],  dir=3.1)
#   # vario.0 <- variog(aft[[i]],  dir=0)
#   lines(res ,type = "l" , col = "red", scaled = T, lwd = 2)
# }
# plot(res, type="l", scaled = T)
# lines(vario.0, scaled=T)
par(new = T)
clim <- cbind(lon.lat,rowMeans(t(window(data.indices.wind[[i]], start = "1971-01-01", end = "2013-01-01")),na.rm=T)); clim<-as.geodata(clim)
vario1 <- variog(clim, option = "bin", breaks = seq(0,300,50))#, max.dist=300 )#, max.dist = 4)
plot(vario1, col="black", type="l", ylim = c(0,4) ,ylab = "" , xlab = "",
     xlim = c(30,265), lwd=3, scaled = T)

}

dev.off()

#######
library(sp)
library(gstat)


#shp.puno <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\titicaca_basin.shp", "titicaca_basin") 
shp.puno <- readOGR("/media/adrian/TOSHIBA EXT/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/titicaca_basin.shp", "titicaca_basin") 
shp.puno <- spTransform(shp.puno, CRS(utm))

#shp.lago <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\LAGOS.shp", "LAGOS")
shp.lago <- readOGR("/media/adrian/TOSHIBA EXT/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/LAGOS.shp", "LAGOS")
shp.lago <- spTransform(shp.lago, CRS(utm))

#years in which reach to the lowest and highest
i = 6 # 1975XX <-- 1976
i = 38 # 2007 <-- 2008

xlow <- data.variog[[6]][, c(6)]
xhigh <- data.variog[[6]][, c(38)]

data_events = cbind(lon.lat, xlow, xhigh)
colnames(data_events) = c("X", "Y", "y1976", "y2008")
coordinates(data_events) = c("X", "Y")
proj4string(data_events) <- CRS(utm)
sp.theme(TRUE)
spplot(data_events, 1:2, 
       cex = 3, 
       ylim = c(8050000, 8450000), 
       xlim=c(250000, 550000),
       lwd=1,
       pch=19) + 
  latticeExtra::layer(sp.polygons(shp.puno, fill=NA, col="gray20")) + 
  latticeExtra::layer(sp.polygons(shp.lago, fill=NA, col="gray20"))

#clouds

xlow.c <- variog(aft[[6]], op="cloud")
xlow.oro <- variog(aft[[6]])
xlow.mod <- variog(aft[[6]], option = "bin", breaks = seq(0,300,50))
xhigh.c <- variog(aft[[38]], op="cloud")
xhigh.oro <- variog(aft[[38]])
xhigh.mod <- variog(aft[[38]], option = "bin", breaks = seq(0,300,50))

par(mfrow=c(1,2), mgp=c(1.5,0.6,0) ,mar=c(2.5,2.7,1.3,0.7), lty="solid" ,oma=c(1.5,1,1,1),las=1)


plot(xlow.c, col="blue", pch=20, scaled=T, cex=1, ylim=c(0, 9.5), main="y1976", xlim = c(30,265), xlab="Distance (km)", ylab="Y(h)")
lines(xlow.oro, col="blue", type="l", lwd=1.5, scaled=T)
lines(xlow.mod, col="blue", type="l", lwd=4, scaled=T)

plot(xhigh.c, col="red", pch=20, scaled=T, cex=1, ylim=c(0, 9.5), main="y2008", xlim = c(30,265), xlab="Distance (km)", ylab="Y(h)")
lines(xhigh.oro, col="red", type="l", lwd=1.5, scaled=T  )
lines(xhigh.mod, col="red", type="l", lwd=4, scaled=T  )



xhigh <- data.variog[[6]][, c(1, 2, 38)]
xhigh = xhigh[complete.cases(xhigh), ]
colnames(xhigh) = c("X", "Y", "y2007")
coordinates(xhigh) = c("X", "Y")
proj4string(xhigh) <- CRS(utm)
g <- gstat(id='y2007',formula=y2007~1,data=xhigh)
expvar <- variogram(g)
expvar <- variogram(g,cutoff=200, width=20,map=TRUE)
plot(expvar)


xhigh <- data.variog[[6]][, c(1, 2, 6)]
xhigh = xhigh[complete.cases(xhigh), ]
colnames(xhigh) = c("X", "Y", "y1975")
coordinates(xhigh) = c("X", "Y")
proj4string(xhigh) <- CRS(utm)
g <- gstat(id='y1975',formula=y1975~1,data=xhigh)
expvar <- variogram(g)
expvar <- variogram(g,cutoff=200, width=20,map=TRUE)
plot(expvar)
