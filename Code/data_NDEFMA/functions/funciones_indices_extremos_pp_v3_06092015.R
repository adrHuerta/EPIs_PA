#INDICES DE EXTREMOS DE PRECIPITACIÓN
#Adrian Huerta

#Rx1day, Monthly maximum 1-day precipitation:
#Let RRij be the daily precipitation amount on day i in period j. 
#The maximum 1-day value for period j are: Rx1dayj = max (RRij)
RX1DAY <- function(a){     # REVISADO
  RX1DAY <- suppressWarnings(max(a,na.rm=T))
  if (RX1DAY == -Inf){RX1DAY <- NA}
  return(RX1DAY)
}

#z <- zoo(rnorm(1000)*10, as.Date(1:1000))
#rollapply(z, 5, sum)
#Rx5day, Monthly maximum 5-day precipitation:
#Let RRij be the daily precipitation amount for the five-day interval k in period j. 
#where k is defined by the last day. The maximum five-day values for period j are RX5dayj = max (RRij)
#si al menos un dia esta vacio, la suma es NA
RX5DAY <- function(a){     # REVISADO
  max.rep.5 <- rollapply(a, 5, sum)
  RX5DAY <- suppressWarnings(max(max.rep.5,na.rm=T))
  if (RX5DAY == -Inf){RX5DAY <- NA}
  return(RX5DAY)
}


#SDII Simple pricipitation intensity index:
#Let RRwj be the daily precipitation amount on wet days,w (RR >= 1mm) in period j.
#If W represents number of wet days in j, then:
SDII <- function(a){          # REVISADO
  a[a < 1] <- NA
  w <- sum(!is.na(a))
  SDII <- sum(a, na.rm = T)/w
  if (w == 0){SDII <- NA}
  return(SDII)
}

#R1mm Annual count of days when PRCP >= 1mm: 
#Let RRij be the daily precipitation amount on day i in period j.
#Count the number of days where: RRij >= 1mm 
R11mm <- function(a){              # REVISADO!
  if (sum(a, na.rm = T) == 0){
    if ((0 %in% a) == T){
      R11mm <- 0
    } else { R11mm <- NA }
  } else if (sum(a,na.rm = T) != 0) {
    a[a < 1] <- NA
    R11mm <- sum(!is.na(a))
  }
  return(R11mm)
}

#R10mm Annual count of days when PRCP >= 10mm: 
#Let RRij be the daily precipitation amount on day i in period j.
#Count the number of days where: RRij >= 10mm 
R10mm<-function(a){              # REVISADO
  if (sum(a, na.rm = T) == 0){
      if ((0 %in% a) == T){
        R10mm <- 0
          } else { R10mm <- NA }
      } else if (sum(a,na.rm = T) != 0) {
      a[a < 10 ] <- NA
      R10mm <- sum(!is.na(a))
  }
  return(R10mm)
}

#R20mm Annual count of days when PRCP??? 20mm:
#Let RRij be the daily precipitation amount on day i in period j.
#Count the number of days where: RRij ??? 20mm
R20mm <- function(a){
  if (sum(a, na.rm = T) == 0){
    if ((0 %in% a) == T){
      R20mm <- 0
    } else { R20mm <- NA }
}else if (sum(a, na.rm = T) != 0) {
  a[a < 20]<-NA
  R20mm <- sum(!is.na(a))
}
  return(R20mm)
}

#Rnnmm Annual count of days when PRCP >= nnmm, nn is a user defined threshold: 
#Let RRij be the daily precipitation amount on day i in period j. 
#Count the number of days where: RRij >= nnmm
Rnnmm<-function(a, nn = 1){
if (sum(a, na.rm = T) == 0) {
  Rnnmm <- NA 
} else if (sum(a, na.rm = T) != 0) {
  a[a < nn] <- NA  #nn value
  Rnnmm <- sum(!is.na(a))
}
  return(Rnnmm)
}

#CDD. Maximum length of dry spell, maximum number of consecutive days with RR < 1mm: 
#Let RRij be the daily precipitation amount on day i in period j. 
#Count the largest number of consecutive days where:  RRij < 1mm
CDD <- function(a){      # REVISADO
  a <- as.numeric(a)
  a[a >= 1] <- 1
  a[a < 1] <- 0 #lo que va a contar
  CDD <- suppressWarnings(max((!a) * unlist(lapply(rle(a)$lengths, seq_len)),na.rm=T)) #lo que va a contar!
  if (CDD == -Inf){CDD <- NA}
  return(CDD)
}

#CWD. Maximum length of wet spell, maximum number of consecutive days with RR ??? 1mm: 
#Let RRij be the daily precipitation amount on day i in period j. 
#Count the largest number of consecutive days where:  RRij >= 1mm
CWD <- function(a){      # REVISADO
  a <- as.numeric(a)
  a[a < 1] <- 0.1
  a[a >= 1] <- 0 #lo que va a contar
  CWD <- suppressWarnings(max((!a) * unlist(lapply(rle(a)$lengths, seq_len)),na.rm=T)) #lo que va a contar!
  if (CWD == -Inf ){CWD <- NA}
  return(CWD)
}

#PRCPTOT. Annual total precipitation in wet days (RR >= 1.0mm) : 
#Let RRij be the daily precipitation amount on day i in period j.
#If I represents the number of days in j, then 
PRCPTOT <- function(a){       # REVISADO
  if (sum(a, na.rm = T) == 0){
    if ((0 %in% a) == T){
      PRCPTOT <- 0
    } else { PRCPTOT <- NA }
  } else if (sum(a,na.rm = T) != 0){
    a[a < 1] <- NA
    PRCPTOT <- sum(a, na.rm = T)
  }  
  return(PRCPTOT)
}

#R95pTOT. Annual total PRCP when RR > 95p. 
#Let RRwj be the daily precipitation amount on a wet day w (RR ??? 1.0mm) 
#in period i and let RRwn95 be the 95th percentile of precipitation on wet days in the 1961-1990 period. 
#If W represents the number of wet days in the period, then:
R95pTOT <- function(a){         # REVISADO
  if (sum(a, na.rm = T) == 0){
    if ((0 %in% a) == T){
      sumP95 <- 0
    } else { sumP95 <- NA }
  } else if (sum(a,na.rm = T) != 0){
    a[is.na(a) | a < 1] <-0
    a[a <= P95[i]] <- 0
    sumP95 <- sum(a)
  }  
  if(is.na(P95[i]) == TRUE){sumP95 <- NA}
  return(sumP95)
}

#CDDm. Mean length of dry spell, maximum number of consecutive days with RR < 1mm: 
#Let RRij be the daily precipitation amount on day i in period j. 
#Count the largest number of consecutive days where:  RRij < 1mm
CDDm <- function(a){      # REVISADO!
  if (all(is.na(a)) == TRUE){
    CDDmm <- NA
    return(CDDmm)
  } else {
    a <- as.numeric(a)
    a[a >= 1] <- NA
    a[a < 1] <- 0 #lo que va a contar
    seq.cons.values <- (!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
    no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
    vals.l <- split(seq.cons.values[no.na], idx[no.na])
    cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
    values.max.cons <- as.numeric(unlist(cons.vals.l))
    values.max.cons <- values.max.cons[values.max.cons != 1]
    CDDmm <- suppressWarnings(mean(values.max.cons))
    
    if (is.nan(CDDmm) == T){ 
      CDDmm <- 0 
    }
    return(CDDmm)
  }
}

#CDDm. Mean length of dry spell, maximum number of consecutive days with RR < 1mm: 
#Let RRij be the daily precipitation amount on day i in period j. 
#Count the largest number of consecutive days where:  RRij < 1mm
CWDm <- function(a){      # REVISADO!
  if (all(is.na(a)) == TRUE){
    CWDmm <- NA
    return(CWDmm)
    } else {
      a <- as.numeric(a)
      a[a < 1] <- NA
      a[a >= 1] <- 0 #lo que va a contar
      seq.cons.values <-(!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
      no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
      vals.l <- split(seq.cons.values[no.na], idx[no.na])
      cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
      values.max.cons <- as.numeric(unlist(cons.vals.l))
      values.max.cons <- values.max.cons[values.max.cons != 1]
      CWDmm <- suppressWarnings(mean(values.max.cons))
      if (is.nan(CWDmm) == T){ 
        CWDmm <- 0 }
      return(CWDmm)
    }
}


#Precp95p

Precp95p <- function(a){
  a <- as.numeric(a)
if ( sum(a,na.rm=T) == 0){
  if ( (0 %in% a) == T){
    a[a < 1] <- NA
    Precp95p <- quantile(a, probs = c(0.95), na.rm = T, type = 7)
  } else { Precp95p <- NA }
} else if ( sum(a,na.rm=T) != 0) {
  a[a < 1] <- NA
  Precp95p <- quantile(a, probs = c(0.95), na.rm = T, type = 7)
}
return(Precp95p)
}


#(R95p) Number of events above Prec95p 
#REVISADO PERO FALTA REALIZAR LA MOD SEGUN EL PAPER


#(R95p) Number of events above Prec95p 
#REVISADO PERO FALTA REALIZAR LA MOD SEGUN EL PAPER

#(R95p) Number of events above Prec95p 
#REVISADO PERO FALTA REALIZAR LA MOD SEGUN EL PAPER

R95p <- function(a){         
  if (sum(a,na.rm = T) == 0){
    if ((0 %in% a) == T){
      a <- as.numeric(a)
      a[a < Px95[i]] <- NA
      a[a >= Px95[i]] <- 0 #lo que va a contar
      seq.cons.values <-(!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
      no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
      vals.l <- split(seq.cons.values[no.na], idx[no.na])
      cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
      axl <- as.numeric(unlist(cons.vals.l))
      a12 <- subset(axl, axl == 1 | axl == 2)
      a34 <- subset(axl, axl == 3 | axl == 4)
      a56 <- subset(axl, axl == 5 | axl == 6)
      a78 <- subset(axl, axl == 7 | axl == 8)
      # values.max.cons <- values.max.cons[values.max.cons != 1]
      R95pmm <- suppressWarnings(length(a12) + length(a34)*2 + length(a56)*3 + length(a78)*4)
      
    } else { R95pmm <- NA }
  } else if (sum(a,na.rm = T) != 0){
    
    a <- as.numeric(a)
    a[a < Px95[i]] <- NA
    a[a >= Px95[i]] <- 0 #lo que va a contar
    seq.cons.values <-(!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
    no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
    vals.l <- split(seq.cons.values[no.na], idx[no.na])
    cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
    axl <- as.numeric(unlist(cons.vals.l))
    a12 <- subset(axl, axl == 1 | axl == 2)
    a34 <- subset(axl, axl == 3 | axl == 4)
    a56 <- subset(axl, axl == 5 | axl == 6)
    a78 <- subset(axl, axl == 7 | axl == 8)
    # values.max.cons <- values.max.cons[values.max.cons != 1]
    R95pmm <- suppressWarnings(length(a12) + length(a34)*2 + length(a56)*3 + length(a78)*4)
    
  }  
  if(is.na(Px95[i]) == TRUE){R95pmm <- NA}
  return(R95pmm)
}

# 
# R95p<-function(a){         
#   if (sum(a,na.rm=T)==0){
#     if ((0 %in% a)==T){
#       a[a<P95[i]]<-NA
#       R95pmm<-sum(!is.na(a))
#     } else { R95pmm<-NA }
#   } else if (sum(a,na.rm=T)!=0){
#     a[a<P95[i]]<-NA
#     R95pmm<-sum(!is.na(a))
#   }  
#   if(is.na(P95[i])==TRUE){R95pmm<-NA}
#   return(R95pmm)
# }
# R95p <- function(a){   
#     a <- as.vector(a)
#   if (sum(a, na.rm = T) == 0){
#     if ((0 %in% a) == T){
#       a[a < P95[i]] <- NA
#       a[a >= P95[i]] <- 0 #lo que va a contar
#       seq.cons.values <-(!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
#       no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
#       vals.l <- split(seq.cons.values[no.na], idx[no.na])
#       cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
#       values.max.cons <- as.numeric(unlist(cons.vals.l))
#       values.max.cons <- values.max.cons[values.max.cons != 1]
#       R95pmm <- suppressWarnings(mean(values.max.cons))
#       
#     } else { R95pmm <- NA }
#   } else if (sum(a,na.rm = T) != 0){
#     a[a < P95[i]] <- NA
#     a[a >= P95[i]] <- 0 #lo que va a contar
#     seq.cons.values <-(!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
#     no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
#     vals.l <- split(seq.cons.values[no.na], idx[no.na])
#     cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
#     values.max.cons <- as.numeric(unlist(cons.vals.l))
#     values.max.cons <- values.max.cons[values.max.cons != 1]
#     R95pmm <- suppressWarnings(mean(values.max.cons))
#   }  
#   if(is.na(P95[i]) == TRUE){ R95pmm <- NA}
#   return(R95pmm)
# }
# 
# # R95p <- function(a){
# #   a <- as.vector(a)
# #   if (sum(a, na.rm = T) == 0){
# #     if ((0 %in% a) == T){
# #       a[a < P95[i]] <- NA
# #       a[a >= P95[i]] <- 0 #lo que va a contar
# #       seq.cons.values <-(!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
# #       no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
# #       vals.l <- split(seq.cons.values[no.na], idx[no.na])
# #       cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
# #       axl <- as.numeric(unlist(cons.vals.l))
# #       a1 <- axl[axl == 1]; n1 <- length(a1)
# #       a2 <- axl[axl == 2]; n2 <- length(a2)
# #       a3 <- axl[axl == 3]; n3 <- length(a3)*2
# #       a4 <- axl[axl == 4]; n4 <- length(a4)*2
# #       a5 <- axl[axl == 5]; n5 <- length(a5)*3
# #       # values.max.cons <- values.max.cons[values.max.cons != 1]
# #       R95pmm <- suppressWarnings(n1+n2+n3+n4+n5)
# #       
# #     } else { R95pmm <- NA }
# #   } else if (sum(a,na.rm = T) != 0){
# #     a[a < P95[i]] <- NA
# #     a[a >= P95[i]] <- 0 #lo que va a contar
# #     seq.cons.values <-(!a) * unlist(lapply(rle(a)$lengths, seq_len)) #lo que va a contar!
# #     no.na <- !is.na(a); idx <- 1 + cumsum(is.na(a))
# #     vals.l <- split(seq.cons.values[no.na], idx[no.na])
# #     cons.vals.l <- lapply(vals.l,function(x.1) x.1[which.max(x.1)])
# #     axl <- as.numeric(unlist(cons.vals.l))
# #     a1 <- axl[axl == 1]; n1 <- length(a1)
# #     a2 <- axl[axl == 2]; n2 <- length(a2)
# #     a3 <- axl[axl == 3]; n3 <- length(a3)*2
# #     a4 <- axl[axl == 4]; n4 <- length(a4)*2
# #     a5 <- axl[axl == 5]; n5 <- length(a5)*3
# #     # values.max.cons <- values.max.cons[values.max.cons != 1]
# #     R95pmm <- suppressWarnings(n1+n2+n3+n4+n5)
# #     
# #   }  
# #   if(is.na(P95[i]) == TRUE){ R95pmm <- NA}
# #   return(R95pmm)
# # }
