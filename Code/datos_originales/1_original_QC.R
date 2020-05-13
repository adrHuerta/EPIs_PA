rm(list = ls())

#setwd("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\datos_originales")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(raster)
library(rts)
library(rasterVis)
library(maptools)
library(mapproj)
library(rgdal)


#DATOS DENTRO DE LA CUENCA:
shp.all.basin <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\data_shpe\\all_gauges_into_basin.shp", "all_gauges_into_basin") 
shp.all.basin <- as.data.frame(shp.all.basin,  xy=FALSE)
rownames(shp.all.basin) <- as.character(shp.all.basin[,3])


shp.puno <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\titicaca_basin.shp", "titicaca_basin") 
shp.puno <- fortify(shp.puno)
layer.puno <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "black", size = 0.9, fill = NA) )


ggplot() +layer.puno  +
  geom_point(data = shp.all.basin, aes(x = LONGITUD, y = LATITUD)) + 
  geom_text(data = shp.all.basin, aes(x = LONGITUD, y = LATITUD, label =  NOMBRE))

##### NO USAR #####

#################################################

datos_qc <- read.zoo("G:\\TRABAJOS\\PROMAG\\R\\SCRIPTS\\data_prec\\PAPER\\DATOS\\DECADE_paper_pp_afterQC.csv",header=T,sep=",",format="%d/%m/%Y")
seleccionados.qc <- c("PUNO","LLALLY","PAMPAHUTA","CHUQUIBAMBILLA","AYAVIRI","MACUSANI","PROGRESO","LAMPA",
                   "AZANGARO","LOS.UROS","ARAPA","MU?ANI","HUANCANE","HUARAYA.MOHO","CAPACHICA","PUCARA",
                   "TARACO", "ISLA.SOTO","ISLA.TAQUILE","MA?AZO","SANTA.ROSA","ANANEA","COJATA",
                   "MAZO.CRUZ","ILAVE","JULI","PIZACOMA","TAHUACO...YUNGUYO","DESAGUADERO","LARAQUERI","CRUCERO",
                   "PUTINA","CUYO.CUYO","ICHU?A","CAPAZO")

seleccionados.cor <- c("PUNO","LLALLY","PAMPAHUTA","CHUQUIBAMBILLA","AYAVIRI","MACUSANI","PROGRESO","LAMPA",
                      "AZANGARO","LOS UROS","ARAPA","MU?ANI","HUANCANE","HUARAYA MOHO","CAPACHICA","PUCARA",
                      "TARACO", "ISLA SOTO","ISLA TAQUILE","MA?AZO","SANTA ROSA","ANANEA","COJATA",
                      "MAZO CRUZ","ILAVE","JULI","PIZACOMA","TAHUACO - YUNGUYO","DESAGUADERO","LARAQUERI","CRUCERO",
                      "PUTINA","CUYO CUYO","ICHUNA","CAPAZO")
#CABANILLAS RINCON DE LA CRUZ LIMBANI JULIACA 
#"TAMBOPATA"  "SAN.GABAN"  "QUINCEMIL" "ISLA.SUANA"  

#######################################

datos_qc <- datos_qc[,  seleccionados.qc ]
names <- colnames(datos_qc) 

shp.all.basin <- shp.all.basin[seleccionados.cor,]
ggplot() + 
  geom_point(data = shp.all.basin, aes(x = x, y = y)) + 
  geom_text(data = shp.all.basin, aes(x = x, y = y, label =  NOMBRE))

# for (i in 1:length(names)){
#  mypath <- file.path(paste(names[i],".pdf", sep=""))
#  pdf(file = mypath , paper="USr",width =13)
#  plot(window(datos_qc[,i], start = "1960-01-01",end = "2013-12-31"), type = "p", cex = 0.1)
#  dev.off()
# }

#qc <- 2 con inhomgeneidad <- primer periodo con un tipo diferente de registro que el ultimo periodo
#qc <- 4 a partir del a?o 2000 existe valors no ceros de pp, tendencia incremento
#qc <- 5 al inicio malos datos, luego atipicos en el a?o ~85
#qc <- 6 serie coompletmante mala, poca variabilidad
#qc <- 7 serie coompletmante mala, practicamente una mitad es buena, el resto es muy mala
#qc <- 8 periodo de inhomogeneidad en entre los 80
#qc <- 9 periodo de inhomogeneidad antes de los 70
#qc <- 10 serie completamente mala errores de medici?n
#qc <- 11 serie completamente mala errores de medici?n, inhomogeneidades
#qc <- 12 serie completamente mala errores de medici?n, inhomogeneidades
#qc <- 13 serie con leves inhomogeneidades a partir de 90 (quiebre)
#qc <- 15 serie con inhomogeneidades a partir de 80 y 90
#qc <- 16 serie con leve inhomogeneidades al inicio de la serie
#qc <- 17 serie con inhomogeneidades (huecos)
#qc <- 18 muy corta
#qc <- 19 inhomogeneidad y atipicos
#qc <- 20 relativamente buena, pero con atipicos y 10 a?os de vacio
#qc <- 21 inhomgeneidad al inicio, pero con ciertos atipicos
#qc <- 22 completamente mala, presenta cocodrilo!
#qc <- 23 inhomogeneidad en la mitad de la serie, muy diferente a la parte final
#qc <- 24 completamente mala, datos muy diferentes en dos periodo
#qc <- 25 inhomogeneidad en entre los 70 y 80
#qc <- 26 leve inhomogeneidad al incio, pero buena 
#qc <- 27 cocodrilo en la seriem no util
#qc <- 28 al final un cocodrilo, inhomogeneidades
#qc <- 29 little bit of inhomogeneidt really good
#qc <- 30 bueno a partir de los 70's
#qc <- 31 cocodrilo en el medio de la serie
#qc <- 32 bueno a partir de los 70's con un poquito de inhomgeneidad en los 90
#qc <- 33 serie completamente mala, presente alta inhomogeneidad
#qc <- 34 cocodrilo en casi 30 a?os de toda la serie
#qc <- 35 cocodrilo en la serie de tiempo, mas de la mita de la serie

##### USAR #####


########################### ESTACIONES SELECCIONADAS PARA EL PAPER ###############################################

estaciones.seleccionadas <- c("DESAGUADERO","HUARAYA MOHO","JULI","LARAQUERI","PAMPAHUTA","PUCARA","PUNO",            #MUY BUENAS
                              "AYAVIRI","AZANGARO","CABANILLAS","CHUQUIBAMBILLA","ICHUNA","ISLA TAQUILE","PUTINA",    #REGULARMENTE BUENAS
                              "CAPACHICA","HUANCANE","LAMPA","MA?AZO","SANTA ROSA","TAHUACO - YUNGUYO", "TARACO",     #CASI REGULARMENTE BUENAS
                              "CAPAZO","MAZO CRUZ")               #MALAS PERO SUGERIDAS POR MIS COLEGAS

estaciones.seleccionadas.df <- c("DESAGUADERO","HUARAYA MOHO","JULI","LARAQUERI","PAMPAHUTA","PUCARA","PUNO",            #MUY BUENAS
                              "AYAVIRI","AZANGARO","CABANILLAS","CHUQUIBAMBILLA","ICHU?A","ISLA TAQUILE","PUTINA",    #REGULARMENTE BUENAS
                              "CAPACHICA","HUANCANE","LAMPA","MA?AZO","SANTA ROSA","TAHUACO - YUNGUYO", "TARACO",     #CASI REGULARMENTE BUENAS
                              "CAPAZO","MAZO CRUZ")               #MALAS PERO SUGERIDAS POR MIS COLEGAS


shp.all.basin <- shp.all.basin[estaciones.seleccionadas, ]
ggplot() + layer.puno + 
  geom_point(data = shp.all.basin, aes(x = LONGITUD, y = LATITUD), size = 5) + 
  geom_text(data = shp.all.basin, aes(x = LONGITUD, y = LATITUD, label =  NOMBRE))

########################### RECOPILANDO LA INFORMACI?N ###############################################
#INFORMACION DE MI TESIS

d.paper <- read.zoo("DECADE_paper_pp_afterQC.csv", header = T, format = "%d/%m/%Y", sep = ",")
d.decade.original <- read.csv("data_DECADE.csv", header = T)

#merging cabanillas data
cabanillas <- subset(d.decade.original, ESTACION ==  "CABANILLAS")
tt <- ISOdate(year = cabanillas[,4], month = cabanillas[,5], day = cabanillas[,6])
CABANILLAS <- zoo(cabanillas[,c(7)], tt)
CABANILLAS <- zoo(CABANILLAS, as.Date(format(time(CABANILLAS), "%Y-%m-%d")))
d.paper <- cbind(d.paper, CABANILLAS)

#sort by cod
codigos.nombres <- data.frame(COD = rep(NA, 23), NOMB = estaciones.seleccionadas.df)
for(i in 1:23){
  codigos.nombres$COD[i] <- subset(d.decade.original, ESTACION ==  estaciones.seleccionadas.df[i])[1,1]
}

codigos.nombres <- codigos.nombres[order(codigos.nombres$COD),]
#encodeString(as.character(codigos.nombres$NOM))

estaciones.seleccionadas.df.df <- c("PUNO","PAMPAHUTA","CHUQUIBAMBILLA","AYAVIRI","LAMPA","CABANILLAS","AZANGARO",
                                 "HUANCANE","HUARAYA.MOHO","CAPACHICA","PUCARA","TARACO","ISLA.TAQUILE","MA?AZO",
                                 "SANTA.ROSA","MAZO.CRUZ","JULI","TAHUACO...YUNGUYO","DESAGUADERO","LARAQUERI","PUTINA",
                                 "ICHU?A","CAPAZO")
d.paper <- d.paper[, estaciones.seleccionadas.df.df]
#View(cbind(colnames(d.paper), codigos.nombres))    #comprobando todo is ok
#View(cbind(shp.all.basin[order(shp.all.basin$CODIGO),],  codigos.nombres ))
shp.all.basin <- shp.all.basin[order(shp.all.basin$CODIGO),]

#DATOS ORDENAS POR POR CODIGO
colnames(d.paper) <- codigos.nombres$COD
coordenadas.d.paper <- cbind(codigos.nombres, shp.all.basin)
coordenadas.d.paper <- coordenadas.d.paper[, c(3,1,2,4,2,6,7,8,9)]
colnames(coordenadas.d.paper) <- c("ID", "COD1", "NOM1", "COD2", "NOM2", "XX", "YY", "ALT", "DEP") 

#last qc
# 
# for (i in 1:23){
#   mypath <- file.path(paste(coordenadas.d.paper$COD1[i],coordenadas.d.paper$NOM1[i],"_QCf_.pdf", sep=""))
#   pdf(file = mypath , paper="USr",width =13)
#   plot(window(d.paper[,i], start = "1960-01-01",end = "2013-12-31"), type = "p", cex = 0.1)
#   dev.off()
# }

ggplot() + layer.puno + 
  geom_point(data = coordenadas.d.paper, aes(x = XX, y = YY), size = 5) + 
  geom_text(data = coordenadas.d.paper, aes(x = XX, y = YY, label =  NOM1))

plot(window(d.paper[,4], start = "1983-10-01", end = "1986-06-01"), type = "p", cex = 0.1)
plot(window(d.paper[,4], start = "1984-10-01", end = "1985-06-01"), type = "p", cex = 0.1)
window(d.paper[,4], start = "1984-10-01", end = "1985-06-01")[window(d.paper[,4], start = "1984-10-01", end = "1985-06-01") > 40] <- NA
plot(d.paper[,4], type = "p", cex = 0.1)


save(d.paper, file = "d.paper_dataset.Rdata")
save(coordenadas.d.paper, file = "coordenadas.d.paper_dataset.Rdata")
