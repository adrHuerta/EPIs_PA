rm(list = ls())

#setwd("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\5th_review\\datos_originales")
#setwd("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/5th_review/datos_originales")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(raster)
library(rts)
library(rasterVis)
library(maptools)
library(mapproj)
library(rgdal)

#load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\data_NDEFMA\\data_clean.RData")
load("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/data_NDEFMA/data_clean.RData")
ls()


#############NEW####################

d.paper <- d.paper[, -match(c("880","776"),
                            colnames(d.paper))]

coordenadas.d.paper <- coordenadas.d.paper[-match(c("880","776"),
                                                 coordenadas.d.paper$COD1), ]

coordenadas.d.paper$COD1 == colnames(d.paper)

#####################################

theme_paper <- theme_bw() + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 9),
                                  axis.title = element_text(size = 9), plot.title = element_text( size = 11),
                                  axis.text.x = element_text(size = 7, colour = "black"),
                                  axis.text.y = element_text(size = 7, colour = "black")) + 
  theme(panel.grid = element_blank())


####### gis information ###########


#raster.dem <- "G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\dem_area_plot.tiff"
raster.dem <- "/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/dem_area_plot.tiff"
r <- as.data.frame(raster(raster.dem),  xy = T)
r$dem_area_plot[r$dem_area_plot <= 0] <- NA

# 
#raster.dem1 <- "G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\dem_lake_basin.tif"
raster.dem1 <- "/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/dem_lake_basin.tif"

r1 <- as.data.frame(raster(raster.dem1),  xy = T)
r1$dem_lake_basin[r1$dem_lake_basin <= 0] <- NA

#shp.bol <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\bolivia\\BOL_adm0.shp", "BOL_adm0") 
shp.bol <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/bolivia/BOL_adm0.shp", "BOL_adm0") 
shp.bol <- fortify(shp.bol)

#shp.chl <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\chile\\CHL_adm0.shp", "CHL_adm0") 
shp.chl <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/chile/CHL_adm0.shp", "CHL_adm0") 
shp.chl <- fortify(shp.chl)

#shp.par <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\paraguay\\PRY_adm0.shp", "PRY_adm0") 
shp.par <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/paraguay/PRY_adm0.shp", "PRY_adm0") 
shp.par <- fortify(shp.par)

#shp.per <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\PER_adm0.shp", "PER_adm0") 
shp.per <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/PER_adm0.shp", "PER_adm0") 
shp.per <- fortify(shp.per)

#shp.puno <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\titicaca_basin.shp", "titicaca_basin") 
shp.puno <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/titicaca_basin.shp", "titicaca_basin") 
shp.puno <- fortify(shp.puno)

#shp.lago <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\LAGOS.shp", "LAGOS")
shp.lago <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/LAGOS.shp", "LAGOS")
shp.lago <- fortify(shp.lago)

layer.puno <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "black", fill = NA, size = 0.5) )
layer.puno1 <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "black",fill = NA, size = 0.25) )
layer.lago <- c( geom_polygon(data=shp.lago, aes(x=long, y=lat, group = group),colour="lightblue" ,fill='lightblue'))
layer.per <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "black", fill = NA, size = 0.25 ) )
layer.per1 <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "black",fill = NA, size = 0.25) )
layer.bol <- c( geom_polygon(data=shp.bol, aes(x=long, y=lat, group = group), colour = "black", fill = NA, size = 0.25 ) )
layer.chile <- c( geom_polygon(data=shp.chl, aes(x=long, y=lat, group = group), colour = "black", fill = NA, size = 0.25) )
layer.par <- c( geom_polygon(data=shp.par, aes(x=long, y=lat, group = group), colour = "black",fill = NA, size = 0.25) )



mapa.peru <- ggplot() + 
 geom_raster(data = r, aes(x = x, y = y, fill = dem_area_plot)) +
 scale_fill_continuous(guide = F,limits = c(0,6500), low = "white", high = "gray20",  na.value= "lightblue", "Elevation\n(msnm)") +
  layer.per +
  annotate(geom="text", x = -75, y = -10, label = "PERU", size = 1.8) +
  annotate(geom="text", x = -65, y = -7, label = "BRAZIL", size = 1.8) +
  annotate(geom="text", x = -74.5, y = -20, label = "PACIFIC OCEAN", size = 1.8) +
  layer.bol + 
  annotate(geom="text", x = -65, y = -18, label = "BOLIVIA", size = 1.8) +
  layer.chile +
  annotate(geom="text", x = -69, y = -23, label = "CHILE", size = 1.8) +
  layer.par +
  layer.puno1 +
  layer.lago +
  annotate(geom="point", x = -77.028, y = -12.043, size = 1.75, colour = "darkblue") +
  annotate(geom="point", x = -70.023, y = -15.843, size = 1.75, colour = "darkblue") +
  annotate(geom="point", x = -68.15, y = -16.5, size = 1.75, colour = "darkblue") +
  annotate(geom="text", x = -78.6, y = -12.043, label = "Lima", size = 1.7, colour = "darkblue") +
  annotate(geom="text", x = -70.2, y = -15.15, label = "Puno", size = 1.7, colour = "darkblue") +
  annotate(geom="text", x = -66.3, y = -16.5, label = "La Paz", size = 1.7, colour = "darkblue") +
  #annotate(geom="text", x = -62, y = -24, label = "Argentina", size = 3) +
  geom_rect(aes(xmin =-71.5 , ymin = -17.75, xmax = -68.25, ymax =  -13.75),color = "darkred", size = 0.5, fill = NA) +
  #scale_x_continuous(limits = c(-80, -60), expand = c(0,0)) +
  #scale_y_continuous(limits = c(-25, -5), expand = c(0,0)) + theme_paper +
  coord_quickmap(xlim = c(-80, -60) , ylim = c(-25, -5), expand = F) + xlab("") + ylab("") + theme_paper +
  annotate(geom="text", x = -78.5, y = -6, label = "a)", size = 4, fontface="bold")
#  coord_map(xlim = c(, ) , ylim = c(-14.25, ))
# scale_x_continuous(limits = c(-80, -60), expand = c(0,0)) +
# scale_y_continuous(limits = c(-25, -5), expand = c(0,0)) + theme_paper +

# coord_map(xlim = c(-80, -60) , ylim = c(-25, -5)) + xlab("") + ylab("") + theme_paper +
# annotate(geom="text", x = -77.5, y = -6, label = "a)", size = 8, fontface="bold")

base <- ggplot()  +
  geom_raster(data = r1, aes(x = x, y = y, fill = dem_lake_basin)) +
  #geom_polygon(data = shp.per, aes(x = long, y = lat, group = group), size = 0.01, alpha = I(0), colour = "black") +
  scale_fill_continuous(guide = F,limits = c(0,6500), low = "white", high = "gray30",  na.value= "lightblue", "Elevation\n(msnm)")                                                                        

mapa <- base + layer.per1 +  layer.puno + layer.lago +
  annotate(geom="text", x = -69.4, y = -15.8, label = "Titicaca Lake", size = 2.5, colour = "black") +
    coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) +
  xlab("") + ylab("") + theme_paper

####### monthly regimen and annual values of precipitation #################

mr <- window(d.paper, start = "1971-01-01", end = "2013-12-31") 

mr.mm <- apply(mr, 2, function(x){
  x <- zoo(x, time(mr))
  y <- apply.monthly(x, function(z){
    if(sum(is.na(z)) <= 3){
      xf <- sum(z, na.rm = T)
      return(xf)
    } else if(sum(is.na(z)) > 3){
      xf <- NA
      return(xf)
    }
  })
})
rownames(mr.mm) <- NULL
mr.mm <- zoo(mr.mm , seq(as.Date("1971-01-01"), as.Date("2013-12-01"), by = "month"))

clim.values <- function(z){
#  mm <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  mm <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  clim.mm <- lapply(mm, function(x){
    n <- z[format(time(z), "%m") == x] 
    return(mean(n, na.rm = T))})
  
  m <- as.numeric(clim.mm)
}

climatologias <- apply(mr.mm, 2, function(w){
  w <- zoo(w, time(mr.mm))
  h <- clim.values(w)
  return(h)
})


#climatologias <- as.data.frame(climatologias)
rownames(climatologias) <- c(1:12)

clim.melt <- melt(climatologias)
clim.melt$Var1 <- factor(clim.melt$Var1, levels=unique(clim.melt$Var1))
clim.melt$Var1 <- factor(clim.melt$Var1, levels = c(8,9,10,11,12,1,2,3,4,5,6,7))

clim.aver <- melt(t(apply(climatologias, 1, mean)))
clim.aver$Var2 <- factor(clim.aver$Var2, levels=unique(clim.melt$Var1))
clim.aver$Var2<- factor(clim.aver$Var2, levels = c(8,9,10,11,12,1,2,3,4,5,6,7))


recta <- data.frame(x = 3.9, y = 9.1)


a.a <- ggplot() + 
  geom_line(data = clim.melt, aes(x = Var1, y = value, group = Var2), size = 0.5, colour = "gray50") + 
  scale_y_continuous(limits = c(0,250)) +
  geom_line(data = clim.aver, aes(x = Var2, y = value, group = Var1), size = 1.5, colour = "black") + 
  ggtitle("Seasonal rainfall") + xlab("Months") + ylab("Total precipitation (mm)") + theme_paper  +
  geom_rect(data = recta, aes(xmin=x, xmax= y, ymin=-Inf, ymax=+Inf), fill='black', alpha=0.2)
a.a <- a.a + 
  theme(legend.justification=c(0,0), legend.position=c(0.01,0.01), plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x = 1, y = 240, label = "c)", size = 4, fontface="bold") + 
  theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 9),
        axis.title = element_text(size = 9), plot.title = element_text( size = 11),
        axis.text.x = element_text(size = 9, colour = "black"),
        axis.text.y = element_text(size = 9, colour = "black"))




an.d.oro <- apply(mr.mm , 2, function(x){
  x <- zoo(x, time(mr.mm))
  y <- apply.yearly(x, sum)
})
an.d.oro <- zoo(an.d.oro, seq(as.Date("1981-01-01"), as.Date("2010-12-01"), by = "year"))
an.d.oro.p <- apply(an.d.oro, 2, mean, na.rm = T)

an.d.oro.p <- data.frame(pp.a = an.d.oro.p, coordenadas.d.paper)
an.d.oro.p$cut.pp <- cut(as.numeric(an.d.oro.p$pp.a), breaks = c(-Inf,600,700,800,Inf),  labels = c("< 600", "600-700","700-800","> 800"),include.lowest = T)
#puntos

proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
an.d.oro.p <- transform(an.d.oro.p, NOM1_cp = proper(NOM1))

a.p <- mapa + 
  geom_point(data = an.d.oro.p, aes(x = XX, y = YY, colour = cut.pp), size = 6) + 
  geom_text_repel(data = an.d.oro.p, aes(x = XX, y = YY, label = NOM1_cp),
                  size = 2, 
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.25, "lines"),
                  segment.color = 'grey50',
                  segment.size = 0.2) + 
  scale_color_manual(values = c("red","orange","springgreen","slateblue"), "Total \nprecipitation \n(mm)")

a.p <- a.p + 
  theme(legend.justification=c(0,0), 
        legend.position=c(0.01,0.01), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 7),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size = 6),
        legend.background = element_rect(fill=alpha('blue', 0), colour = "black", size = 0.1),
        legend.key = element_blank()) + 
  guides(colour = guide_legend(override.aes = list(size=4)))  +
  annotate(geom="text", x = -71.3, y = -13.9, label = "b)", size = 4, fontface="bold")
library(grid)

# #MODO PARA GUARDAR EN PDF <- 8 * 20
# vp1 <- viewport(width = 0.6, height = 0.6, x = 0.28, y = 0.68)
# vp2 <- viewport(width = 0.45, height = 0.4, x = 0.28, y = 0.20)
# vp6 <- viewport(width = 0.7, height = 1, x = 0.73, y = 0.5)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2,2)))

library(gridExtra)

mapa.peru <- mapa.peru + theme(plot.margin = unit(c(0.5,0.5,0,1.5), "cm"))
a.a <- a.a + theme(plot.margin = unit(c(0,0,1,1.5), "cm"))
g <- grid.arrange(arrangeGrob(mapa.peru, a.a), a.p, ncol = 2)
ggsave("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_V6/IJC/figures_2/Figure_01.pdf",
       g,
       width = 8, height = 5)

