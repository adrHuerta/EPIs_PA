rm(list = ls())

#setwd("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\9_trend_detection\\3rd_review\\after_pettitt_test")
setwd("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/3rd_review/data_NDEFMA/9_trend_detection/3rd_review/after_pettitt_test")

library(reshape2)
library(ggplot2)
library(raster)
library(rts)
library(rasterVis)
library(maptools)
library(mapproj)
library(rgdal)
library(gridExtra)

#shp.per <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\PER_adm0.shp", "PER_adm0") 
shp.per <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/PER_adm0.shp", "PER_adm0") 
shp.per <- fortify(shp.per)

#shp.puno <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\titicaca_basin.shp", "titicaca_basin") 
shp.puno <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/titicaca_basin.shp", "titicaca_basin") 
shp.puno <- fortify(shp.puno)

#shp.lago <- readOGR("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\gis_data\\LAGOS.shp", "LAGOS")
shp.lago <- readOGR("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_origen/gis_data/LAGOS.shp", "LAGOS")
shp.lago <- fortify(shp.lago)

layer.puno <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "gray40", fill = NA) )
layer.puno1 <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "gray40",fill = NA) )
layer.lago <- c( geom_polygon(data=shp.lago, aes(x=long, y=lat, group = group),colour="lightblue" ,fill='lightblue'))
layer.per <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "gray80", fill = NA) )
layer.per1 <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "gray80", fill = NA) )


data <- read.csv("MK.TEST.RESULTS.csv", header = T)
data <- data[,c("COD2","NOM1","XX","YY","index", "Sen_slope","CORR_p.value","FDR")]
data <- transform(data, pval_NO_fdr = ifelse(CORR_p.value >= 0.05, 0, 1),
                        pval_fdr = ifelse(FDR >= 0.05, 0, 1), 
                        sign_trend = ifelse(Sen_slope > 0, 1, ifelse(Sen_slope < 0, -1, 0)))

####
data.slope <- data
data.slope$index <- factor(data.slope$index, levels = 
                                   c("R1mm","CWD","CWDm","CDD","CDDm",
                                     "R95p","R95pTOT",
                                     "RX1day","RX5day","SDII","PRCPTOT"))

data.slope.r <- by(data.slope, data.slope$index, function(x){
  res <- x[,c("COD2","NOM1","Sen_slope","pval_NO_fdr","pval_fdr","CORR_p.value","FDR")]
  res$Sen_slope <- round(res$Sen_slope, 2)
  res$CORR_p.value <- round(res$CORR_p.value, 2)
  res$FDR <- round(res$FDR, 2)
  return(res)
})

data.slope.r <- do.call("rbind", data.slope.r)
write.csv(data.slope.r, "data.slope.r.csv", quote = F)
#### table frequency

data.table.trend <- data
data.table.trend$index <- factor(data.table.trend$index, levels = 
                                   c("R1mm","CWD","CWDm","CDD","CDDm",
                                     "R95p","R95pTOT",
                                     "RX1day","RX5day","SDII","PRCPTOT"))


incresing.df <- by(data.table.trend, data.table.trend$index, function(x){
  res.x <- subset(x, sign_trend == 1)
  data.frame(Total = length(res.x$sign_trend),
             Loca_Significance = sum(res.x$pval_NO_fdr),
             Global_Significance = sum(res.x$pval_fdr),
             stationary = (length(res.x$sign_trend)-sum(x$pval_NO_fdr)))
})

decresing.df <- by(data.table.trend, data.table.trend$index, function(x){
  res.x <- subset(x, sign_trend == -1)
  data.frame(Total = length(res.x$sign_trend),
             Loca_Significance = sum(res.x$pval_NO_fdr),
             Global_Significance = sum(res.x$pval_fdr),
             stationary = (length(res.x$sign_trend)-sum(x$pval_NO_fdr)) )
  })

incresing.df <- do.call("rbind", incresing.df)
decresing.df <- do.call("rbind", decresing.df)

incresing.df <- data.frame(round(incresing.df, 1), TY = "INCREASING")
decresing.df <- data.frame(round(decresing.df, 1),TY = "DECREASING")
write.csv(rbind(incresing.df, decresing.df), "table_3_trend_overview.csv", quote = F)


##################
data.figures <- data
data.figures$index <- factor(data.figures$index, levels = 
                               c("R1mm","CWD","CWDm","CDD","CDDm",
                                 "R95p","R95pTOT",
                                 "RX1day","RX5day","SDII","PRCPTOT"))
data.figures <- transform(data.figures, col_pval_NO_fdr = ifelse(pval_NO_fdr == 1 & sign_trend == 1, 
                                                                 1, ifelse(pval_NO_fdr == 1 & sign_trend == -1, -1, 0)))
data.figures <- transform(data.figures, col_pval_fdr = ifelse(pval_fdr == 1 & sign_trend == 1, 
                                                              2.5, ifelse(pval_fdr == 1 & sign_trend == -1, 2.5, 0.5)))
data.figures$sign_trend <- factor(data.figures$sign_trend, levels = c(1,0,-1), 
                                  labels = c("Positive trend", "No trend", "Negative trend"))

ggplot() + 
  layer.per +
  layer.lago + 
  layer.puno +
  geom_point(data = data.figures, aes(x = XX, y = YY,
                                      colour = as.factor(sign_trend), 
                                      fill = as.factor(col_pval_NO_fdr),  
                                      stroke = as.numeric(col_pval_fdr),
                                      shape = as.factor(sign_trend)), size = 4) +
  facet_wrap(~index) + 
  scale_shape_manual("",values = c(24,21,25)) + 
  scale_colour_manual("",values = c("blue","black","red")) + 
  scale_fill_manual("",values = c("red",NA,"blue")) + 
  coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 11, face = "bold")) + 
  guides(fill=FALSE) + 
  theme(legend.position = c(0.87, 0.18),
        strip.background = element_blank()) + 
  theme(axis.text.x=element_text(colour="black"),
    axis.text.y=element_text(colour="black"))
ggsave("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_V6/IJC/figures_2/Figure_04.pdf", width = 8, height = 11)
# 
# ggplot() + 
#   layer.per +
#   layer.lago + 
#   layer.puno +
#   geom_point(data = subset(data.figures, index == "R1mm" | index == "CWD" | index == "CWDm" | index == "CDD" | index == "CDDm"), aes(x = XX, y = YY,
#                                       colour = as.factor(sign_trend), 
#                                       fill = as.factor(col_pval_NO_fdr),  
#                                       stroke = as.numeric(col_pval_fdr),
#                                       shape = as.factor(sign_trend)), size = 4) +
#   facet_wrap(~index) + 
#   scale_shape_manual("",values = c(24,21,25)) + 
#   scale_colour_manual("",values = c("blue","black","red")) + 
#   scale_fill_manual(values = c("red",NA,"blue")) + 
#   coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
#   theme_bw() + 
#   theme(strip.text.x = element_text(size = 14, face = "bold")) + 
#   guides(fill=FALSE) + 
#   theme(legend.position = c(0.85, 0.25),
#         strip.background = element_blank()) 
# ggsave("trend_plot_1group.pdf", width = 6, height = 6)
# 
# ggplot() + 
#   layer.per +
#   layer.lago + 
#   layer.puno +
#   geom_point(data = subset(data.figures, index == "R95p" | index == "R95pTOT"), aes(x = XX, y = YY,
#                                                                                     colour = as.factor(sign_trend), 
#                                                                                     fill = as.factor(col_pval_NO_fdr),
#                                                                                     stroke = as.numeric(col_pval_fdr),
#                                                                                     shape = as.factor(sign_trend)), size = 4.5) +
#   facet_wrap(~index) + 
#   scale_shape_manual(values = c(24,21,25)) + 
#   scale_colour_manual(values = c("blue","black","red")) + 
#   scale_fill_manual(values = c("red",NA,"blue")) + 
#   coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
#   theme_bw() + 
#   theme(strip.text.x = element_text(size = 14, face = "bold")) + 
#   theme(legend.position="none",
#         strip.background = element_blank())
# ggsave("trend_plot_2group.pdf", width = 5, height = 5)
# 
# 
# 
# 
# ggplot() + 
#   layer.per +
#   layer.lago + 
#   layer.puno +
#   geom_point(data = subset(data.figures, index == "RX1day" | index == "RX5day" | index == "SDII" | index == "PRCPTOT"), aes(x = XX, y = YY,
#                                                                                     colour = as.factor(sign_trend), 
#                                                                                     fill = as.factor(col_pval_NO_fdr),
#                                                                                     stroke = as.numeric(col_pval_fdr),
#                                                                                     shape = as.factor(sign_trend)), size = 3.5) +
#   facet_wrap(~index, ncol  = 4) + 
#   scale_shape_manual(values = c(24,21,25)) + 
#   scale_colour_manual(values = c("blue","black","red")) + 
#   scale_fill_manual(values = c("red",NA,"blue")) + 
#   coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
#   theme_bw() + 
#   theme(strip.text.x = element_text(size = 14, face = "bold")) + 
#   theme(legend.position="none", 
#         strip.background = element_blank()) 
# ggsave("trend_plot_3group.pdf", width = 8, height = 4)
# 
# 
# # ###################################### PRCPTOT #################
# # 
# # percpt <- subset(data, index == "R1mm")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,3,24), guide = F) +
# #   scale_size_area(max_size = 20, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# #   
# # 
# # ###################################### RX1day
# # 
# # percpt <- subset(data, index == "RX1day")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,24), guide = F) +
# #   scale_size_area(max_size = 20, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# # 
# # ###################################### RX5day
# # 
# # percpt <- subset(data, index == "RX5day")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # RX5day.plot <- ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,3,24), guide = F) +
# #   scale_size_area(max_size = 20, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# # 
# # ###################################### CDD
# # 
# # percpt <- subset(data, index == "CDD")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,3,24), guide = F) +
# #   scale_size_area(max_size = 20, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# # 
# # ###################################### CDDm
# # 
# # percpt <- subset(data, index == "CDDm")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # CDDm.plot <- ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,24), guide = F) +
# #   scale_size_area(max_size = 20,guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# # ###################################### CWD
# # 
# # percpt <- subset(data, index == "CWD")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,3,24), guide = F) +
# #   scale_size_area(max_size = 20, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# # ###################################### CWDm
# # 
# # percpt <- subset(data, index == "CWDm")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # CWDm.plot <- ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,3,24), guide = F) +
# #   scale_size_area(max_size = 20, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# # 
# # ###################################### PRCPTOT
# # 
# # percpt <- subset(data, index == "PRCPTOT")
# # percpt <- transform(percpt, sen_abs = abs(Sen_slope))
# # percpt <- transform(percpt, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt <- transform(percpt, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # PRCPTOT.plot <- ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,24), guide = F) +
# #   scale_size_area(max_size = 20, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("")
# # 
# # multiplot(PRCPTOT.plot, RX5day.plot, CDDm.plot, CWDm.plot, cols = 2)
# # grid.arrange( PRCPTOT.plot, RX5day.plot, CDDm.plot, CWDm.plot, cols = 2)
# # 
# # vp1 <- viewport(width = 0.55, height = 1, x = 0.25, y = 0.7)
# # vp6 <- viewport(width = 0.55, height = 1, x = 0.25, y = 0.25)
# # vp2 <- viewport(width = 0.55, height = 1, x = 0.75, y = 0.25)
# # vp3 <- viewport(width = 0.55, height = 1, x = 0.75, y = 0.7)
# # grid.newpage()
# # pushViewport(viewport(layout = grid.layout(2,2)))
# # print(PRCPTOT.plot, vp = vp3)
# # print(PRCPTOT.plot, vp = vp1)
# # print(PRCPTOT.plot, vp = vp2)
# # 
# # print(PRCPTOT.plot, vp = vp6)
# 
# ###########
# 
# 
# percpt1 <- subset(data, index == "PRCPTOT")
# percpt2 <- subset(data, index == "RX5day")
# percpt3 <- subset(data, index == "CDDm")
# percpt4 <- subset(data, index == "CWDm")
# # 
# # percpt1[,4] <- percpt1[,4]/40
# # percpt2[,4] <- percpt2[,4]/10
# # percpt4[,4] <- percpt4[,4]*2
# # 
# # percpt1.all <- rbind(percpt1, percpt2, percpt3, percpt4)
# # 
# # percpt1.all <- transform(percpt1.all, sen_abs = abs(Sen_slope))
# # percpt1.all <- transform(percpt1.all, p.value.fig = ifelse(CORR_p.value >= 0.05,0,1))
# # percpt1.all <- transform(percpt1.all, sen_fig = ifelse(Sen_slope > 0, 1,ifelse(Sen_slope < 0 ,-1,0)))
# # 
# # percpt1.all$index <- factor(percpt1.all$index, levels = c("PRCPTOT", "RX5day","CDDm","CWDm"))
# # 
# # ggplot() + 
# #   layer.per +
# #   layer.lago + 
# #   layer.puno  +
# #   geom_point(data = percpt1.all, aes(x = XX,  y = YY, fill = as.factor(p.value.fig), size = sen_abs, shape = as.factor(sen_fig))) +
# #   scale_fill_manual(values = c("white", "black"), guide = F) + 
# #   scale_shape_manual(values = c(25,3,24), guide = F) +
# #   scale_size_area(max_size = 15, guide = F) + theme_bw() +
# #   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) + xlab("") + ylab("") +
# #   facet_wrap(~ index) + 
# #   theme(strip.background = element_blank(), strip.text.x =  element_text(size=15, face = "bold")) +
# #   theme(legend.position="none", plot.margin=unit(c(-1, 0, -2, 0), "cm"))+
# #   theme(axis.title.x=element_blank(), title = element_blank())
# # ggsave("plot_final.pdf", width = 8, height = 11)
# 
# ###################
# 
# summary(percpt1)
# percpt1$Sen_slope_cut <- cut(percpt1$Sen_slope, breaks = c(-Inf,-2, -1, -0.1, 0.1, 1, 2 ,Inf))
# percpt1 <- transform(percpt1, CORR_p.value.stroke = ifelse(CORR_p.value > 0.05, 0, 2))
# 
# llabells <- c("<2", 
#               "-2-1",
#               "-1--0.1",
#               "-0.1-0.1", 
#               "0.1-1 ", 
#               "1-2", 
#               ">2")
# 
# prcpto.pl <- ggplot() + 
#   layer.per +
#   layer.lago + 
#   layer.puno  +
#   geom_point(data = percpt1, aes(x = XX,  y = YY, 
#                                  fill = as.factor(Sen_slope_cut),
#                                  stroke = CORR_p.value.stroke), shape = 21, size = 11) + 
#   scale_fill_manual("mm", values = c("tomato4","tomato2","rosybrown2",
#                                "white",
#                                "slategray2","steelblue2","steelblue4"), 
#                     labels = llabells) + 
#   theme_bw() +
#   theme(legend.justification=c(0,0), legend.position=c(0,0)) +
#   coord_quickmap(xlim = c(-71.25, -68.5) , ylim = c(-17.75,  -14), 
#                  expand = F) + xlab("") + ylab("") + 
#   ggtitle("PRCPTOT") + 
#   guides(fill=guide_legend(
#     keywidth=0.2,
#     keyheight=0.2,
#     override.aes = list(size=4))) +
#   theme(plot.title = element_text(lineheight=14, face="bold"))
# 
# 
# 
# 
# ######
# summary(percpt2)
# 
# percpt2$Sen_slope_cut <- cut(percpt2$Sen_slope, breaks = c(-Inf,-0.4, -0.2, -0.1, 0.1, 0.2, 0.4 ,Inf))
# percpt2 <- transform(percpt2, CORR_p.value.stroke = ifelse(CORR_p.value > 0.05, 0, 2))
# 
# llabells <- c("<-0.4", 
#               "-0.4--0.2",
#               "-0.2--0.1",
#               "-0.1-0.1", 
#               "0.1-0.2 ", 
#               "0.2-0.4", 
#               ">0.4")
# 
# rx5day.pl <- ggplot() + 
#   layer.per +
#   layer.lago + 
#   layer.puno  +
#   geom_point(data = percpt2, aes(x = XX,  y = YY, 
#                                  fill = as.factor(Sen_slope_cut),
#                                  stroke = CORR_p.value.stroke), shape = 21, size = 11) + 
#   scale_fill_manual("mm", values = c("tomato4","tomato2","rosybrown2",
#                                      "white",
#                                      "slategray2","steelblue2","steelblue4"), 
#                     labels = llabells) + 
#   theme_bw() +
#   theme(legend.justification=c(0,0), legend.position=c(0,0)) +
#   coord_quickmap(xlim = c(-71.25, -68.5) , ylim = c(-17.75,  -14), 
#                  expand = F) + xlab("") + ylab("") + 
#   ggtitle("RX5day") + 
#   guides(fill=guide_legend(
#     keywidth=0.2,
#     keyheight=0.2,
#     override.aes = list(size=4))) +
#   theme(plot.title = element_text(lineheight=14, face="bold"))
# 
# 
# 
# summary(percpt3)
# 
# percpt3$Sen_slope_cut <- cut(percpt3$Sen_slope*100, breaks = c(-Inf,-2, -1, -0.1, 0.1, 1, 2 ,Inf))
# percpt3 <- transform(percpt3, CORR_p.value.stroke = ifelse(CORR_p.value > 0.05, 0, 2))
# 
# llabells <- c("<-2", 
#               "-2--1",
#               "-1--0.1",
#               "0.1-1", 
#               "1-2", 
#               ">2")
# 
# 
# cddm.plt <- ggplot() + 
#   layer.per +
#   layer.lago + 
#   layer.puno  +
#   geom_point(data = percpt3, aes(x = XX,  y = YY, 
#                                  fill = as.factor(Sen_slope_cut),
#                                  stroke = CORR_p.value.stroke), shape = 21, size = 11) + 
#   scale_fill_manual("days", values = c("tomato4","tomato2","rosybrown2",
#                                      "slategray2","steelblue2","steelblue4"), 
#                     labels = llabells) + 
#   theme_bw() +
#   theme(legend.justification=c(0,0), legend.position=c(0,0)) +
#   coord_quickmap(xlim = c(-71.25, -68.5) , ylim = c(-17.75,  -14), 
#                  expand = F) + xlab("") + ylab("") + 
#   ggtitle("CDDm") + 
#   guides(fill=guide_legend(
#     keywidth=0.2,
#     keyheight=0.2,
#     override.aes = list(size=4))) +
#   theme(plot.title = element_text(lineheight=14, face="bold"))
# 
# 
# 
# summary(percpt4)
# 
# percpt4$Sen_slope_cut <- cut(percpt4$Sen_slope*100, breaks = c(-Inf,-1.5, -1, -0.1, 0.1, 1, 1.5 ,Inf))
# percpt4 <- transform(percpt4, CORR_p.value.stroke = ifelse(CORR_p.value > 0.05, 0, 2))
# 
# llabells <- c("<-1.5", 
#               "-1.5--1",
#               "-0.1-0.1", 
#               "0.1-1 ", 
#               "1-1.5", 
#               ">1.5")
# 
# cwdm.plt <- ggplot() + 
#   layer.per +
#   layer.lago + 
#   layer.puno  +
#   geom_point(data = percpt4, aes(x = XX,  y = YY, 
#                                  fill = as.factor(Sen_slope_cut),
#                                  stroke = CORR_p.value.stroke), shape = 21, size = 11) + 
#   scale_fill_manual("days", values = c("tomato4","tomato2",
#                                        "white",
#                                        "slategray2","steelblue2","steelblue4"), 
#                     labels = llabells) + 
#   theme_bw() +
#   theme(legend.justification=c(0,0), legend.position=c(0,0)) +
#   coord_quickmap(xlim = c(-71.25, -68.5) , ylim = c(-17.75,  -14), 
#                  expand = F) + xlab("") + ylab("") + 
#   ggtitle("CWDm") + 
#   guides(fill=guide_legend(
#     keywidth=0.2,
#     keyheight=0.2,
#     override.aes = list(size=4))) +
#   theme(plot.title = element_text(lineheight=14, face="bold"))
# 
# 
# ##################
# 
# 
# 
# library(grid)
# 
# vp1 <- viewport(width = 0.5, height = 0.55, x = 0.23, y = 0.24)
# vp2 <- viewport(width = 0.5, height = 0.55, x = 0.72, y = 0.24)
# vp3 <- viewport(width = 0.5, height = 0.55, x = 0.23, y = 0.74)
# vp4 <- viewport(width = 0.5, height = 0.55, x = 0.72, y = 0.74)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2,2)))
# 
# cairo_pdf("trend_summary.pdf", width = 8.27, height = 11)
# 
# print(cwdm.plt, vp = vp4) #4
# print(rx5day.pl, vp = vp2) #2
# print(prcpto.pl, vp = vp3) #3
# print(cddm.plt, vp = vp1) #1
# 
# dev.off()
