rm(list = ls())
setwd("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/10_relation_detection")

library(ggplot2)
library(raster)
library(rts)
library(rasterVis)
library(maptools)
library(mapproj)
library(rgdal)
 
fis <- as.list(dir(pattern = ".csv"))

fis <- lapply(fis, function(x){
  res <- read.csv(x, header = T)
  res <- data.frame(res, var = gsub("_TNC.csv", "", x))
  return(res)
  })
fis <- do.call("rbind", fis)

#write.csv(fis, "resumen.csv")
#------------------------------#

setwd("/home/adrian/Documents/DECADE_paper/code/5th_review/data_NDEFMA/10_relation_detection/graphs")

data <- fis
data.1 <- subset(data, type == "H")
# data.1$var <- factor(data.1$var, levels = c("R1mm","PRCPTOT","SDII",
#                                             "CWD","CWDm","CDD","CDDm",
#                                             "RX1day","RX5day","R95pTOT","R95p"))
data.1$var <- factor(data.1$var, levels = c("R1mm","CWD","CWDm","CDD","CDDm",
                                           "R95p","R95pTOT",
                                           "RX1day","RX5day","SDII","PRCPTOT"))
#data.1$names.modos <- factor(data.1$names.modos, levels = c("Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi","PDOi"))
data.1$names.modos <- factor(data.1$names.modos, levels = c("PDOi","Ei","Ci","MEi","SOi","TNAi","TSAi","TN_TSi"),
                             labels = c("PDOi","Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi"))

data.by.index <- by(data.1, data.1$var, function(x){
  
  res <- x[,c("names.modos","pvalue","p.value.FDR")]
  
  })

data.by.index <- as.list(data.by.index)
names(data.by.index)

results.corre <- list()

for(i in 1:11){
  xy <- data.by.index[[i]]
 # xy$names.modos <- factor(xy$names.modos, levels = c("Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi","PDOi"))
  xy$names.modos <- factor(xy$names.modos, levels = c("PDOi","Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi"))
  
  res.mod <- by(xy, xy$names.modos, function(z){
    
    res <- z[,c("pvalue","p.value.FDR")]
    res <- transform(res, pvalue.C = ifelse(pvalue >= 0.05, 0,1), pvalue.FDR.C = ifelse(p.value.FDR >= 0.05,0,1))
    res.r <- colSums(res)
    return(res.r[c("pvalue.C", "pvalue.FDR.C")])
    })
  
 # results.corre[[i]] <- data.frame(do.call("rbind" ,res.mod), vari = names(data.by.index)[i], modo = c("Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi","PDOi")) 
  results.corre[[i]] <- data.frame(do.call("rbind" ,res.mod), vari = names(data.by.index)[i], modo = c("PDOi","Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi")) 
 
}

results.corre <- do.call("rbind", results.corre)
rownames(results.corre) <- NULL

results.corre.pval <- data.frame(results.corre[,c("pvalue.C","vari","modo")], sig = "local")
results.corre.FDR <- data.frame(results.corre[,c("pvalue.FDR.C","vari","modo")], sig = "global")

colnames(results.corre.pval) <- c("pval", "var", "modo", "sig")
colnames(results.corre.FDR) <- c("pval", "var", "modo", "sig")

df <- rbind(results.corre.pval, results.corre.FDR)
#df$modo <- factor(df$modo, levels = c("Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi","PDOi"))
df$modo <- factor(df$modo, levels = c("PDOi","Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi"))

ggplot() + 
  geom_bar(data = df, aes(x = modo, y = pval, fill = sig), stat="identity", position="dodge") + 
  scale_fill_manual(guide = F, "", values = c("blue","red")) + 
  facet_wrap(~ var) + theme_bw() + xlab("") + ylab("")

df.sub <- subset(df, modo == "Ei" |
                     modo == "Ci" |
                     modo == "PDOi" |
                     modo == "TNAi" |
                     modo == "TSAi" |
                     modo == "TN-TSi")
#df.sub$modo <- factor(df.sub$modo, levels = c("Ei","Ci","MEi","PDOi","SOi","TNAi","TSAi","TN-TSi"))
df.sub$modo <- factor(df.sub$modo, levels = c("PDOi","Ei","Ci","MEi","SOi","TNAi","TSAi","TN-TSi"))


ggplot() + 
  geom_bar(data = df.sub, aes(x = var, y = pval, fill = sig), width=.8, stat="identity", position="dodge") + 
  scale_fill_manual("", values = c("blue","red"), labels = c("Significant correlation at the local 5% level", "Significant correlation at the global 5% level")) + 
  facet_wrap(~ modo, ncol = 2) + theme_bw() + xlab("") + ylab("") +
  theme(strip.background = element_blank(), strip.text.x =  element_text(size=9, face = "bold")) +
  theme(axis.title.x=element_blank(), title = element_blank()) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8, colour = "black"), 
        axis.text.y = element_text(hjust = 1, size = 9, colour = "black")) + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.7,"line")) 


ggsave("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_V6/IJC/figures_2/Figure_06.pdf", 
       width = 6, height = 4)

####

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
layer.puno1 <- c( geom_polygon(data=shp.puno, aes(x=long, y=lat, group = group), colour = "gray40", fill = NA) )
layer.lago <- c( geom_polygon(data=shp.lago, aes(x=long, y=lat, group = group),colour="lightblue" ,fill='lightblue'))
layer.per <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "gray80",  fill = NA) )
layer.per1 <- c( geom_polygon(data=shp.per, aes(x=long, y=lat, group = group), colour = "gray80", fill = NA) )



#####
#Ci
data.1.Ci <- subset(data.1, names.modos == "Ci") 
data.1.Ci <- data.1.Ci[,c("NOMBRE", "LONGITUD", "LATITUD", "var" ,"cor","pvalue","p.value.FDR")]
data.1.Ci <- subset(data.1.Ci, var == "PRCPTOT" | var == "CWD" | var == "CWDm" )
data.1.Ci <- transform(data.1.Ci, sign_cor = ifelse(cor > 0, 1, ifelse(cor < 0, -1, 0) ),
                       pval_NO_fdr = ifelse(pvalue >= 0.05, 0, 1), 
                       pval_fdr = ifelse(p.value.FDR >= 0.05, 0, 1))

data.1.Ci <- transform(data.1.Ci, col_pval_NO_fdr = ifelse(pval_NO_fdr == 1 & sign_cor == 1, 
                                                                 1, ifelse(pval_NO_fdr == 1 & sign_cor == -1, -1, 0)))
data.1.Ci <- transform(data.1.Ci, col_pval_fdr = ifelse(pval_fdr == 1 & sign_cor == 1, 
                                                              2, ifelse(pval_fdr == 1 & sign_cor == -1, 2, 0.5)))

data.1.Ci$sign_cor <- factor(data.1.Ci$sign_cor, levels = c(1,-1), 
                             labels = c("Positive correlation", "Negative correlation"))



ggplot() + 
  layer.per +
  layer.lago + 
  layer.puno +
  geom_point(data = data.1.Ci, aes(x = LONGITUD, y = LATITUD, colour = as.factor(sign_cor), 
                                    fill = as.factor(col_pval_NO_fdr), 
                                stroke = as.numeric(col_pval_fdr),
                                    shape = as.factor(sign_cor)), size = 5) +
  facet_wrap(~var, ncol  = 3) + 
  scale_shape_manual("", values = c(24,25)) + 
  scale_colour_manual("", values = c("blue","red")) + 
  scale_fill_manual("", values = c("red",NA,"blue")) + 
  coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 11, face = "bold")) + 
  guides(fill=FALSE) + 
  theme(legend.position="bottom", 
        strip.background = element_blank()) + 
  theme(legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))

ggsave("/media/buntu/TOSHIBA EXT1/TRABAJOS/DECADE/paper_DECADE/paper_V6/IJC/figures_2/Figure_07.pdf",
       width = 6, height = 4)



#Ei
data.1.Ci <- subset(data.1, names.modos == "Ei") 
data.1.Ci <- data.1.Ci[,c("NOMBRE", "LONGITUD", "LATITUD", "var" ,"cor","pvalue","p.value.FDR")]
#data.1.Ci <- subset(data.1.Ci, var == "PRCPTOT" | var == "CWD" | var == "CWDm" )
data.1.Ci <- transform(data.1.Ci, sign_cor = ifelse(cor > 0, 1, ifelse(cor < 0, -1, 0) ),
                       pval_NO_fdr = ifelse(pvalue >= 0.05, 0, 1), 
                       pval_fdr = ifelse(p.value.FDR >= 0.05, 0, 1))

data.1.Ci <- transform(data.1.Ci, col_pval_NO_fdr = ifelse(pval_NO_fdr == 1 & sign_cor == 1, 
                                                           1, ifelse(pval_NO_fdr == 1 & sign_cor == -1, -1, 0)))
data.1.Ci <- transform(data.1.Ci, col_pval_fdr = ifelse(pval_fdr == 1 & sign_cor == 1, 
                                                        2, ifelse(pval_fdr == 1 & sign_cor == -1, 2, 0)))

data.1.Ci$sign_cor <- factor(data.1.Ci$sign_cor, levels = c(1,-1), 
                             labels = c("Positive correlation", "Negative correlation"))


#data.1.Ci <- subset(data.1.Ci, var == "CDDm" )

ggplot() + 
  layer.per +
  layer.lago + 
  layer.puno +
  geom_point(data = data.1.Ci, aes(x = LONGITUD, y = LATITUD, colour = as.factor(sign_cor), 
                                   fill = as.factor(col_pval_NO_fdr), 
                                   stroke = as.numeric(col_pval_fdr),
                                   shape = as.factor(sign_cor)), size = 5) +
  facet_wrap(~var, ncol  = 3) + 
  scale_shape_manual("", values = c(24,25)) + 
  scale_colour_manual("", values = c("blue","red")) + 
  scale_fill_manual("", values = c("red",NA,"blue")) + 
  coord_quickmap(xlim = c(-71.35, -68.4) , ylim = c(-17.55, -13.95)) + xlab("") + ylab("") + 
  theme_bw() + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  guides(fill=FALSE) + 
  theme(legend.position="bottom", 
        strip.background = element_blank()) 


###################

data.all <- data 
data.all <- data.all[,c("NOMBRE", "LONGITUD", "LATITUD", "var" ,"cor","pvalue","p.value.FDR",  "type", "names.modos")]
data.all <- transform(data.all, sign_cor = ifelse(cor > 0, 1, ifelse(cor < 0, -1, 0) ),
                       pval_NO_fdr = ifelse(pvalue >= 0.05, 0, 1), 
                       pval_fdr = ifelse(p.value.FDR >= 0.05, 0, 1))

write.csv(data.all, "data.all.correlation.csv", quote = F)
# ############################################################
# 
# #PRCPTOT
# 
# prddf <- subset(data.1, names.modos == "Ci" | names.modos == "MEi" | names.modos == "SOi")
# prddf <- subset(prddf, var == "PRCPTOT" | var == "CWD" | var == "CWDm")
# prddf$var <- as.character(prddf$var)
# prddf$names.modos <- as.character(prddf$names.modos)
# 
# 
# 
# df <- prddf[,c("var","names.modos","NOMBRE","LONGITUD","LATITUD","cor","pvalue")]
# 
# df$r.cut <- cut(df$cor, breaks = c(-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7))
# df <- transform(df, pval.shape = ifelse( pvalue > 0.05, 1, -1))
# df <- transform(df, rf = ifelse( cor >= 0, 1, -1))
# df <- transform(df, rf.pval = ifelse(pval.shape == -1 & rf == 1, 1,
#                                                ifelse(pval.shape == -1 & rf == -1, -1,0)))
# df <- transform(df, rf.p.size = ifelse(rf.pval == 0, 1,0))
# df$names.modos <- factor(df$names.modos)
# df$var <- factor(df$var, levels = c("PRCPTOT","CWD","CWDm"))
# 
# colfunc <- colorRampPalette(c("red","white","blue"))
# 
# 
# ggplot() +
#   layer.per +
#   layer.lago + 
#   layer.puno  +
#  # shp.zone1 +
#   geom_point(data = df, aes(x = LONGITUD, y = LATITUD, fill = r.cut, shape = as.factor(rf.pval), size = as.factor(rf.p.size)), colour = "black") + 
#   scale_fill_manual("R",values = c("darkred", "red", "orange","white","lightblue","blue","black"), guide = F) +
#   scale_size_manual("", values = c(5,5), guide = F) +
#   scale_shape_manual("", values = c(25,21,24), guide = F) + 
#  # scale_colour_manual("", values = c("black","gray40"), guide = F) + 
#   facet_grid(var~names.modos) + theme_bw()+ xlab("") + ylab("") +
#   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75))
# 
# #legend
# 
# ggplot() + 
#   geom_point(data = df, aes(x = LONGITUD, y = LATITUD, fill = r.cut), shape = 22, colour = "black", size = 10) + 
#   scale_fill_manual("R",values = c("darkred", "red", "orange","white","lightblue","blue","black")) 
# 

####################################################


prddf <- subset(data.1, names.modos == "Ci")
prddf <- subset(prddf, var == "PRCPTOT" | var == "CWD" | var == "CWDm")
prddf$var <- factor(prddf$var, levels = c("PRCPTOT","CWD","CWDm"))
prddf$names.modos <- as.character(prddf$names.modos)



dffff <- prddf[,c("var","names.modos","NOMBRE","LONGITUD","LATITUD","cor","pvalue")]

dffff$cor.cut <- cut(dffff$cor, breaks = c(-0.6, -0.4, -0.2, -0.1, 0.1 ,0.2)) 
dffff <- transform(dffff, fill.pval = ifelse( pvalue < 0.05, 2,0))
llabells <- c("-0.6--0.4", "-0.4--0.2","-0.2--0.1","-0.1-0.1","0.1-0.2")
#df$r.cut <- cut(df$cor, breaks = c(-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7))
#df <- transform(df, cor.abs = abs(cor))
# df <- transform(df, cor.sig = ifelse( cor >= 0, 1, -1))
# df <- transform(df, fill.pval = ifelse( pvalue < 0.05, 1,0))

# 
# df <- transform(df, rf.p.size = ifelse(rf.pval == 0, 1,0))
# df$names.modos <- factor(df$names.modos)
# df$var <- factor(df$var, levels = c("PRCPTOT","CWD","CWDm"))
# 
# colfunc <- colorRampPalette(c("red","white","blue"))



ggplot() + 
  layer.per +
  layer.lago + 
  layer.puno  +
  geom_point(data = dffff, aes(x = LONGITUD,  y = LATITUD, 
                                 fill = as.factor(cor.cut),
                                 stroke = fill.pval), shape = 21, size = 10) + 
  scale_fill_manual("",values = c("tomato4","tomato2","rosybrown2",
                                       "white",
                                       "steelblue2"), 
                    labels = llabells) + 
  theme_bw() +
  theme(legend.justification=c(0,0), legend.position=c(0,0)) +
  coord_quickmap(xlim = c(-71.25, -68.5) , ylim = c(-17.75,  -14), 
                 expand = F) + xlab("") + ylab("") + 
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.2,
    override.aes = list(size=6))) + 
  facet_wrap(~var, ncol = 3) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 15, face="bold"))

ggsave("correlation2_summary.pdf", width = 13, height = 6)


# 
# ggplot() +
#   layer.per +
#   layer.lago + 
#   layer.puno  +
#   # shp.zone1 +
#   geom_point(data = df, aes(x = LONGITUD, y = LATITUD,  size = cor.abs, shape = as.factor(cor.sig), fill = as.factor(fill.pval)), colour = "gray20") + 
#   coord_quickmap(xlim = c(-71.5, -68.25) , ylim = c(-17.75, -13.75)) +
#   scale_shape_manual("", values = c(25,24), guide = F) +
#   scale_fill_manual("R",values = c("white","black"), guide = F) +
#   scale_size_area(max_size = 8, guide = F) + theme_bw() +
#   facet_wrap(~var, ncol = 3) + theme_bw()+ xlab("") + ylab("") +
#   theme(strip.background = element_blank(), strip.text.x =  element_text(size=12, face = "bold")) +
#   theme(axis.title.x=element_blank(), title = element_blank())
