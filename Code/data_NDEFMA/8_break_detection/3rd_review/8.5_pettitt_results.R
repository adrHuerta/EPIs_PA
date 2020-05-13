rm(list = ls())

setwd("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\8_break_detection\\3rd_review")

library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(geoR)
library(lattice)
library(psych)
library(corrplot)
library(trend)

theme_paper <- theme_bw() + theme(panel.border = element_rect(colour = "black"), axis.text = element_text(size = 10 + 5),
                                  axis.title = element_text(size = 10 + 5), plot.title = element_text( size = 17))



load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\data.indices.Rdata")
load("G:\\TRABAJOS\\DECADE\\paper_DECADE\\paper_origen\\3rd_review\\data_NDEFMA\\4_extreme_computation\\coordenadas.indices.Rdata")
ls()


#viendo los resultados de PEtitt en PRCPTOT y R1mm, se evaluo cuales fueron significatios
# a pesar de que tuvieran tendencia

# # CORR_p.value  Sen_slope  NOM1	COD2	XX	YY	index	FDR
# 0.03530386  2.85	CHUQUIBAMBILLA	764	-70.728333	-14.796389	PRCPTOT	0.264778952
# 0.019206966	-4.625714286	CAPAZO	158326	-69.745556	-17.186389	PRCPTOT	0.264778952
# 0.025259233	-0.357142857	HUARAYA MOHO	787	-69.491389	-15.389722	R1mm	0.232214754
# 0.030961967	-0.242424242	DESAGUADERO	883	-69.0404	-16.5688	R1mm	0.232214754

#pettitt PRCPTOT - original
###SIG: 764, 158326
#pettitt PRCPTOT - aftTrend
###SIG:  -

#pettitt R1mm - original
###SIG: -
#pettitt R1mmv - aftTrend
###SIG: -

#FDR NULL 

#CONCLUSIONES:
# NIGUNA ESTACION FUE ELIMINADA, YA QUE LOS UNICOS QUIBRES (prcptot)
# FUERON INFLUENCIADOS POR TENDENCIA

###############NEW DATASET
coor.ind.aftP <- coordenadas.indices
data.indices.aftP <- data.indices
save(coor.ind.aftP, data.indices.aftP, file = "indices.aftP.Rdata")
