####### assessemnt the avalaible data set in the rainy season (NDEFMA) #################

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


load("d.paper_dataset.Rdata")
load("coordenadas.d.paper_dataset.Rdata")



theme_paper <- theme_bw() + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 10 + 5),
                                  axis.title = element_text(size = 10 + 5), plot.title = element_text( size = 17))



d.paper <- window(d.paper, start = as.Date("1930-11-01"), end = as.Date("2013-12-31"))

pp.jfmand.a_hyd <- d.paper[months(time(d.paper)) %in%
                             c("enero","febrero","marzo","abril","noviembre","diciembre")]
pp.jfmand.a_hyd <- lag(pp.jfmand.a_hyd, k = -61)


nyears <- 2013-1931+1
n_estaciones <- dim(pp.jfmand.a_hyd)[2]


#contado los valores por a?o:
count_hy <- matrix(NA, nrow = nyears, ncol = n_estaciones)
for (i in 1:n_estaciones){
  count_hy[,i] <- apply.yearly(pp.jfmand.a_hyd[,i], function(x){
    x[x >= 0] <- 1
    return(sum(x, na.rm = T))})
}
colnames(count_hy) <- colnames(pp.jfmand.a_hyd)

#haciendo NA a?os con datos faltantes mayores al 10% en los meses (EFMA-ND) (31+28+31+30+30+31)*0.90 = 162.9 == 163
count_Va <- count_hy; count_Va[count_Va < 163] <- NA
count_Va[count_Va>0]<-1;#count_Va[count_Va==0]<-NA HACIENDO A?OS VALIDOS IGUAL A 1 PARA SUMAR Y VER COMO UN TODO, Y PARA AGREGAR Y VER POR ESTACION


plot.count_Va <- as.data.frame(cbind(rowSums(count_Va, na.rm = T),c(1931:2013)))
plot.1 <- ggplot(plot.count_Va, aes(x = V2 , y =V1 )) + geom_line() + geom_point(size = 2) + theme_bw() + xlab("Years") + ylab("Number of stations with full years") + scale_y_continuous(limits=c(0,20) , breaks=seq(0,20,by=5)) + scale_x_continuous(limits=c(1930,2015), breaks=seq(1930,2015,by=10))
plot.1 + theme_paper 
#3*6
plot.2 <- ggplot(plot.count_Va, aes(x = V2 , y =V1 )) + geom_line() + geom_point(size = 3 ) + theme_bw() + xlab("") + ylab("Number of stations with full years") + scale_y_continuous(limits=c(0,25) , breaks=seq(0,25,by=5)) + scale_x_continuous(limits=c(1971,2013), breaks=seq(1930,2015,by=10))
plot.2 + theme_paper 


for (i in 1:n_estaciones){ count_Va[,i][!is.na(count_Va[,i])]<- i }; rownames(count_Va) <- c(1931:2013)
plot.2.count_Va <- melt(t(as.data.frame(count_Va)))
plot.3 <- ggplot(plot.2.count_Va, aes( x = Var2 , y = value, group = Var1 )) + geom_line() + geom_point() + theme_bw() + xlab("") + ylab("Estaciones") + scale_y_discrete(labels = colnames(pp.jfmand.a_hyd)) + scale_x_continuous(limits=c(1930,2015), breaks=seq(1930,2015,by=10))
plot.4 <- ggplot(plot.2.count_Va, aes( x = Var2 , y = value, group = Var1 )) + geom_line() + geom_point() + theme_bw() + xlab("") + ylab("Estaciones") + scale_y_discrete(labels = colnames(pp.jfmand.a_hyd)) + scale_x_continuous(limits=c(1971,2013), breaks=seq(1930,2015,by=10))
plot.3
plot.4

#de acuerdo a eso eliminamos las estaciones:
#d.paper[,-c(7,11,13,14,15,18)]
#coordenadas.d.paper[-c(7,11,13,14,15,18),]


rm(list = ls())

load("d.paper_dataset.Rdata")
d.paper <- d.paper[,-c(7,11,13,14,15,18)]
load("coordenadas.d.paper_dataset.Rdata")
coordenadas.d.paper <- coordenadas.d.paper[-c(7,11,13,14,15,18),]


theme_paper <- theme_bw() + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 10 + 5),
                                  axis.title = element_text(size = 10 + 5), plot.title = element_text( size = 17))



d.paper <- window(d.paper, start = as.Date("1930-11-01"), end = as.Date("2013-12-31"))

pp.jfmand.a_hyd <- d.paper[months(time(d.paper)) %in%
                             c("enero","febrero","marzo","abril","noviembre","diciembre")]
pp.jfmand.a_hyd <- lag(pp.jfmand.a_hyd, k = -61)


nyears <- 2013-1931+1
n_estaciones <- dim(pp.jfmand.a_hyd)[2]


#contado los valores por a?o:
count_hy <- matrix(NA, nrow = nyears, ncol = n_estaciones)
for (i in 1:n_estaciones){
  count_hy[,i] <- apply.yearly(pp.jfmand.a_hyd[,i], function(x){
    x[x >= 0] <- 1
    return(sum(x, na.rm = T))})
}
colnames(count_hy) <- colnames(pp.jfmand.a_hyd)

#haciendo NA a?os con datos faltantes mayores al 10% en los meses (EFMA-ND) (31+28+31+30+30+31)*0.90 = 162.9 == 163
count_Va <- count_hy; count_Va[count_Va < 163] <- NA
count_Va[count_Va>0]<-1;#count_Va[count_Va==0]<-NA HACIENDO A?OS VALIDOS IGUAL A 1 PARA SUMAR Y VER COMO UN TODO, Y PARA AGREGAR Y VER POR ESTACION


plot.count_Va <- as.data.frame(cbind(rowSums(count_Va, na.rm = T),c(1931:2013)))
plot.1 <- ggplot(plot.count_Va, aes(x = V2 , y =V1 )) + geom_line() + geom_point(size = 2) + theme_bw() + xlab("Years") + ylab("Number of stations with full years") + scale_y_continuous(limits=c(0,20) , breaks=seq(0,20,by=5)) + scale_x_continuous(limits=c(1930,2015), breaks=seq(1930,2015,by=10))
plot.1 + theme_paper 
#3*6
plot.2 <- ggplot(plot.count_Va, aes(x = V2 , y =V1 )) + geom_line() + geom_point(size = 3 ) + theme_bw() + xlab("") + ylab("Number of stations with full years") + scale_y_continuous(limits=c(0,25) , breaks=seq(0,25,by=5)) + scale_x_continuous(limits=c(1971,2013), breaks=seq(1930,2015,by=10))
plot.2 + theme_paper 


for (i in 1:n_estaciones){ count_Va[,i][!is.na(count_Va[,i])]<- i }; rownames(count_Va) <- c(1931:2013)
plot.2.count_Va <- melt(t(as.data.frame(count_Va)))
plot.3 <- ggplot(plot.2.count_Va, aes( x = Var2 , y = value, group = Var1 )) + geom_line() + geom_point() + theme_bw() + xlab("") + ylab("Estaciones") + scale_y_discrete(labels = colnames(pp.jfmand.a_hyd)) + scale_x_continuous(limits=c(1930,2015), breaks=seq(1930,2015,by=10))
plot.4 <- ggplot(plot.2.count_Va, aes( x = Var2 , y = value, group = Var1 )) + geom_line() + geom_point() + theme_bw() + xlab("") + ylab("Estaciones") + scale_y_discrete(labels = colnames(pp.jfmand.a_hyd)) + scale_x_continuous(limits=c(1971,2013), breaks=seq(1930,2015,by=10))
plot.3
plot.4

save(d.paper, coordenadas.d.paper, file = "G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\data_clean.RData")
save(d.paper, coordenadas.d.paper, file = "G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\datos_originales\\data_clean.RData")

#HACIENDO NA AQUELLOS A?OS QUE NO TIENEN LOS DIAS COMPLETOS
a?os <- as.character(c(1931:2013))
pp_hydro_l <- matrix(ncol = n_estaciones, nrow = dim(pp.jfmand.a_hyd)[1])
for (i in 1:n_estaciones){
  serie <- pp.jfmand.a_hyd[,i]
  for (j in 1:length(a?os)){
    count <- sum(!is.na(serie[format(index(serie),"%Y") == a?os[j]]))
    if (count < 163){
      serie[format(index(serie),"%Y") == a?os[j]] <- NA
    }
  }
  pp_hydro_l[,i]<-serie
}
colnames(pp_hydro_l) <-  colnames(pp.jfmand.a_hyd)
pp_hydro_l <- zoo(pp_hydro_l, time(pp.jfmand.a_hyd))


save(pp_hydro_l, file = "G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\pp_hydro_l_dataset.Rdata")


#########################################################################################################################
####### assessemnt the avalaible data set in the rayni season (NDEFM) #################

rm(list = ls())

#setwd("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\datos_originales")

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


load("d.paper_dataset.Rdata")
load("coordenadas.d.paper_dataset.Rdata")



theme_paper <- theme_bw() + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 10 + 5),
                                  axis.title = element_text(size = 10 + 5), plot.title = element_text( size = 17))



d.paper <- window(d.paper, start = as.Date("1930-11-01"), end = as.Date("2013-12-31"))

pp.jfmand.a_hyd <- d.paper[months(time(d.paper)) %in%
                             c("enero","febrero","marzo","noviembre","diciembre")]
pp.jfmand.a_hyd <- lag(pp.jfmand.a_hyd, k = -61)


nyears <- 2013-1931+1
n_estaciones <- dim(pp.jfmand.a_hyd)[2]


#contado los valores por a?o:
count_hy <- matrix(NA, nrow = nyears, ncol = n_estaciones)
for (i in 1:n_estaciones){
  count_hy[,i] <- apply.yearly(pp.jfmand.a_hyd[,i], function(x){
    x[x >= 0] <- 1
    return(sum(x, na.rm = T))})
}
colnames(count_hy) <- colnames(pp.jfmand.a_hyd)

#haciendo NA a?os con datos faltantes mayores al 10% en los meses (EFM-ND) (31+28+31+30+31)*0.90 = 135.9 == 136
count_Va <- count_hy; count_Va[count_Va < 136] <- NA
count_Va[count_Va>0]<-1;#count_Va[count_Va==0]<-NA HACIENDO A?OS VALIDOS IGUAL A 1 PARA SUMAR Y VER COMO UN TODO, Y PARA AGREGAR Y VER POR ESTACION


plot.count_Va <- as.data.frame(cbind(rowSums(count_Va, na.rm = T),c(1931:2013)))
plot.1 <- ggplot(plot.count_Va, aes(x = V2 , y =V1 )) + geom_line() + geom_point(size = 2) + theme_bw() + xlab("Years") + ylab("Number of stations with full years") + scale_y_continuous(limits=c(0,20) , breaks=seq(0,20,by=5)) + scale_x_continuous(limits=c(1930,2015), breaks=seq(1930,2015,by=10))
plot.1 + theme_paper 
#3*6
plot.2 <- ggplot(plot.count_Va, aes(x = V2 , y =V1 )) + geom_line() + geom_point(size = 3 ) + theme_bw() + xlab("") + ylab("Number of stations with full years") + scale_y_continuous(limits=c(0,25) , breaks=seq(0,25,by=5)) + scale_x_continuous(limits=c(1971,2013), breaks=seq(1930,2015,by=10))
plot.2 + theme_paper 


for (i in 1:n_estaciones){ count_Va[,i][!is.na(count_Va[,i])]<- i }; rownames(count_Va) <- c(1931:2013)
plot.2.count_Va <- melt(t(as.data.frame(count_Va)))
plot.3 <- ggplot(plot.2.count_Va, aes( x = Var2 , y = value, group = Var1 )) + geom_line() + geom_point() + theme_bw() + xlab("") + ylab("Estaciones") + scale_y_discrete(labels = colnames(pp.jfmand.a_hyd)) + scale_x_continuous(limits=c(1930,2015), breaks=seq(1930,2015,by=10))
plot.4 <- ggplot(plot.2.count_Va, aes( x = Var2 , y = value, group = Var1 )) + geom_line() + geom_point() + theme_bw() + xlab("") + ylab("Estaciones") + scale_y_discrete(labels = colnames(pp.jfmand.a_hyd)) + scale_x_continuous(limits=c(1971,2013), breaks=seq(1930,2015,by=10))
plot.3
plot.4

#HACIENDO NA AQUELLOS A?OS QUE NO TIENEN LOS DIAS COMPLETOS
a?os <- as.character(c(1931:2013))
pp_hydro_l <- matrix(ncol = n_estaciones, nrow = dim(pp.jfmand.a_hyd)[1])
for (i in 1:n_estaciones){
  serie <- pp.jfmand.a_hyd[,i]
  for (j in 1:length(a?os)){
    count <- sum(!is.na(serie[format(index(serie),"%Y") == a?os[j]]))
    if (count < 163){
      serie[format(index(serie),"%Y") == a?os[j]] <- NA
    }
  }
  pp_hydro_l[,i]<-serie
}
colnames(pp_hydro_l) <-  colnames(pp.jfmand.a_hyd)
pp_hydro_l <- zoo(pp_hydro_l, time(pp.jfmand.a_hyd))


save(pp_hydro_l, file = "pp_hydro_l_dataset.Rdata")

rm(list = ls())

load("d.paper_dataset.Rdata")
d.paper <- d.paper[,-c(7,11,14,15,18)]
load("coordenadas.d.paper_dataset.Rdata")
coordenadas.d.paper <- coordenadas.d.paper[-c(7,11,14,15,18),]

save(d.paper, coordenadas.d.paper, file = "data_clean.RData")