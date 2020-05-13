rm(list = ls())
setwd("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/5th_review/data_NDEFMA/9.5_intensity_and_frequency_analysis")
#setwd("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/5th_review/data_NDEFMA/9.5_intensity_and_frequency_analysis")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(lattice)
library(scales)
library(rgdal)

#source('/media/buntu/TOSHIBA EXT1/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/g_legend.R')
#source('/media/buntu/TOSHIBA EXT1/TRABAJOS/PROMAG/R/SCRIPTS/data_prec/PAPER/scripts/fdr.R')

load("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/5th_review/data_NDEFMA/8_break_detection/3rd_review/indices.aftP.Rdata")
#load("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/5th_review/data_NDEFMA/8_break_detection/3rd_review/indices.aftP.Rdata")
ls()

coordenadas.indices <- coor.ind.aftP

r <- data.indices.aftP$PRCPTOT
p <- data.indices.aftP$SDII
t <- data.indices.aftP$R1mm

df_prt <- list()

for(i in 1:dim(p)[2])
  {
   
  res <- data.frame(p = coredata(p[,i]), r = coredata(r[,i]), t = coredata(t[,i])) 
  res <- res[complete.cases(res), ] 
  res_fit <- lm(r ~ t + p, res)
  res_ab <- c(res_fit$coefficients[2]*sd(res$t), res_fit$coefficients[3]*sd(res$p))
  
  df_prt[[i]] <- res_ab
  }

df_prt <- do.call("rbind", df_prt)
colnames(df_prt) <- c("St", "Sp") 
df_prt <- transform(df_prt, cc = ifelse(St > Sp,1, 2))
df_prt$NN = coordenadas.indices$NOM1
df_prt$St_Sp_dif <- df_prt$St - df_prt$Sp

aa <- ggplot() + 
  geom_point(data = df_prt, aes(x = St, y = Sp, colour = factor(cc)), size = 5) + 
  scale_colour_manual("",labels = c("Frequency dominated" ,  "Intensity dominated"), values = c("red", "blue")) + 
  geom_abline(intercept = 0) +
  scale_x_continuous(limits = c(50, 150), expand = c(0,0)) + 
  scale_y_continuous(limits = c(50, 150), expand = c(0,0)) + 
  annotate("text", x = 55, y = 144, label = "a)", size = 5) + 
  theme_bw() +  theme(legend.position="bottom") + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 10 + 5),
                     axis.title = element_text(size = 10 + 5), plot.title = element_text( size = 17))+ 
  theme(legend.text=element_text(size=10),
        legend.title = element_text(size = 7),
        legend.key.width = unit(0.25, "cm"))
ggplot() + 
  geom_point(data = df_prt, aes(x = St, y = Sp, colour = St_Sp_dif), size = 6) + 
  geom_abline(intercept = 0) +
  scale_x_continuous(limits = c(50, 150), expand = c(0,0)) + 
  scale_y_continuous(limits = c(50, 150), expand = c(0,0)) + 
  annotate("text", x = 55, y = 144, label = "a)", size = 6) + 
  theme_bw() +  theme(legend.position="none") + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 10 + 5),
                                                      axis.title = element_text(size = 11), plot.title = element_text( size = 17))

##############

shp.per <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/PER_adm0.shp", "PER_adm0") 
shp.per <- readOGR("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/PER_adm0.shp", "PER_adm0") 
shp.per <- fortify(shp.per)

shp.puno <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/titicaca_basin.shp", "titicaca_basin") 
shp.puno <- readOGR("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/titicaca_basin.shp", "titicaca_basin") 
shp.puno <- fortify(shp.puno)

shp.lago <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/LAGOS.shp", "LAGOS")
shp.lago <- readOGR("G:/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/LAGOS.shp", "LAGOS")
shp.lago <- fortify(shp.lago)


layer.puno <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "gray40", fill = NA) )
layer.puno1 <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "gray40", fill = NA) )
layer.lago <- c( geom_polygon(data=shp.lago, aes(x=long, y=lat, group = group),colour="lightblue" ,fill='lightblue'))
layer.per <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "gray80",  fill = NA) )
layer.per1 <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "gray80", fill = NA) )


coordenadas.indices$CC <- df_prt$cc
coordenadas.indices$St_Sp_dif <- df_prt$St_Sp_dif

bb <- ggplot() + 
  layer.per +
  layer.lago + 
  layer.puno +
  annotate("text", x = -71.2, y = -14, label = "b)", size = 5) + 
  geom_point(data = coordenadas.indices, aes(x = XX, y = YY, colour = factor(CC)), size = 4) +
  scale_colour_manual("",values = c("red", "blue")) + 
  coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  guides(fill=FALSE) + 
  theme(legend.position="bottom", 
        strip.background = element_blank())  + 
  theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 11),
                                                      axis.title = element_text(size = 11), plot.title = element_text( size = 17))

ggplot() + 
  layer.per +
  layer.lago + 
  layer.puno +
  annotate("text", x = -71.2, y = -14, label = "b)", size = 4) + 
  geom_point(data = coordenadas.indices, aes(x = XX, y = YY, colour = St_Sp_dif), size =1) +
  scale_colour_gradient2() + 
  coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  guides(fill=FALSE) + 
  theme(legend.position="bottom", 
        strip.background = element_blank())  + 
  theme_bw()  + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 11),
                                                      axis.title = element_text(size = 11), plot.title = element_text( size = 17))


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(aa)

p3 <- grid.arrange(arrangeGrob(aa + theme(legend.position="none", text = element_text(size=5),
                                          axis.text.x = element_text(size = 8,colour="black"),
                                          axis.text.y = element_text(size = 8,colour="black"),
                                          axis.title.x = element_text(size = 11),
                                          axis.title.y = element_text(size = 11)) ,
                               bb + theme(legend.position="none", text = element_text(size=15),
                                          axis.text.x = element_text(size = 8,colour="black"),
                                          axis.text.y = element_text(size = 8,colour="black"),
                                          axis.title.x = element_text(size = 11),
                                          axis.title.y = element_text(size = 11)) +
                                 theme(plot.margin = unit(c(0,0,0,-1), "cm"))
                                 ,
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))

ggsave("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_V6/IJC/figures_2/Figure_05.pdf",
       plot = p3,
       width = 5.5, height = 3)

