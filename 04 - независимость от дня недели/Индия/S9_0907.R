### Lyas script
### Overlapping

#####################################
# CONSTANT                          #
#####################################
# Must be load file
bLoadFile <- TRUE
Sys.setenv(TZ="GMT")

# remove objects from workspace
if (bLoadFile) {
  list <- ls()
  rm(list=(list[!list %in% c("bLoadFile")]))
} else {
  list <- ls()
  rm(list=(list[!list %in% c("input_px", "input_px_week", "input_px_month", "input_eqy", "input_tot", "bLoadFile", "input_weekly")]))
}

# load required packages
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)
#library(chron)

RankRB_w <- function(x){
  # Computes the rank of an xts object of ranking factors
  # ranking factors are the factors that are ranked (i.e. asset returns)
  #
  # args:
  #   x = xts object of ranking factors
  #
  # Returns:
  #   Returns an xts object with ranks
  #   (e.g. for ranking asset returns, the asset with the greatest return
  #    receives a  rank of 1)  
  r <- as.xts(t(apply(-x, 1, rank, na.last = "keep")))
  return(r)
}

RankRB_l <- function(x){
  r <- as.xts(t(apply(x, 1, rank, na.last = "keep")))
  return(r)
}

LoadData <- function(sfile, f_col=3, f_dec = "."){
  # Load data from csv-file and return xts object
  # 
  # args:
  #  sfile = path to file "input.csv"
  #
  # Returns:
  #  returns an xts object
  print(paste0("LOAD File: ",sfile,", Time: ",Sys.time()))
  sf <- read.csv(file=sfile, sep=";", header=FALSE)
  t_count <- ceiling(ncol(sf) / f_col)
  rd <- data.frame(matrix(ncol=1,nrow=0))
  names(rd)[1] <- "Date"
  # ONLY for FAST Algorithm
  fast_koef <-125
  rd_count <- ceiling(t_count/fast_koef)
  rd.list <- list()
  rd_idx <- 0
  rd_idx_temp <- 0
  
  for (i in seq(t_count)) {
    if (ncol(sf)>=(i-1)*f_col+2) {
      rt1 <- sf[3:nrow(sf),c((i-1)*f_col+1,(i-1)*f_col+2)]
      rt1 <- rt1[!(factor(rt1[,1]) %in% ""),]
      names(rt1) <- c(as.character(sf[2,(i-1)*f_col+1]),as.character(sf[1,(i-1)*f_col+1]))
      if (is.na(as.Date(as.character(rt1[1,1]), format="%d.%m.%Y"))) {
        rt1$Date <- as.Date(as.numeric(as.character(rt1$Date)), origin="1899-12-30")
      } else {
        rt1$Date <- as.Date(as.character(rt1$Date), format="%d.%m.%Y")
      }
      #     rd <- merge(rt1, rd, by="Date", all=TRUE)
      
      if ((rd_idx==0) | (rd_idx_temp==fast_koef)) {
        rd_idx <- rd_idx+1
        rd_idx_temp <- 0
        rd.list[[rd_idx]] <- data.frame(matrix(ncol=1,nrow=0))
        names(rd.list[[rd_idx]])[1] <- "Date"
      }
      rd_idx_temp <- rd_idx_temp+1
      rd.list[[rd_idx]] <- merge(rt1, rd.list[[rd_idx]], by="Date", all=TRUE)
    }
  }
  for (i in seq(rd_count))
    rd <- merge(rd.list[[i]], rd, by="Date", all=TRUE)
  rd <- rd[seq(length(na.omit(rd[,1]))),]
  
  t_m <- as.matrix(rd[,2:ncol(rd)])
  rownames(t_m) <- format.Date(as.character(rd[,1]))
#  rownames(t_m) <- format.Date(as.Date(as.character(rd[,1]), tz="GMT"))
  t_m <- type.convert(t_m, dec=f_dec)
  return(as.xts(t_m))
}


LyasReturnsCountByYear <- function(xts.ret, min_date, max_date){
  f_AR <- matrix(nrow=0, ncol=2)
  
  for (idx_year in seq(min_date, max_date)) {
    cm <- colMeans(xts.ret[paste0("",idx_year)], na.rm=TRUE)
    mean_px <- mean(cm, na.rm=TRUE)
    f <- vector(mode="character", length=2)
    
    f[1] <- length(na.omit(cm))
    f[2] <- mean_px
    
    f <- t(as.matrix(f))
    colnames(f) <- c("Count", "Mean")
    rownames(f) <- as.character(idx_year)
    f_AR <- rbind(f_AR, f)
  }
  qs_file <- paste0("S7_Des_Stat.csv")
  print(paste0("SAVE: Des_Stat (",qs_file,")"))
  write.csv2(f_AR, file=qs_file)
  
}


LyasFixedNa <- function(px, rfr, isWinner) {
  # Fixed NA in Middle
  inp_conv <- px
  inp_new_b <- vector(mode="logical", length=ncol(inp_conv))
  inp_new_i <- vector(mode="integer", length=ncol(inp_conv))
  inp_new_rfr <- vector(mode="logical", length=ncol(inp_conv))
  for (y in seq(nrow(inp_conv))) {
    for (idx in seq(ncol(inp_conv[y]))) {
      if (is.na(inp_conv[y, idx]) && inp_new_b[idx]) {
        inp_new_i[idx] <- inp_new_i[idx]+1
        if (inp_new_rfr[idx]) {
          if (isWinner) {
            inp_conv[y, idx] <- as.numeric(inp_conv[y-1, idx])*as.numeric(1+rfr[y])
          } else {
            inp_conv[y, idx] <- as.numeric(inp_conv[y-1, idx])*as.numeric(1-rfr[y])
          }
        } else if (inp_new_i[idx]>6) {
          inp_new_rfr[idx] <- TRUE
          for (yy in seq(inp_new_i[idx]-1,0)) {
            if (isWinner) {
              inp_conv[y-yy, idx] <- as.numeric(inp_conv[y-yy-1, idx])*as.numeric(1+rfr[y-yy])
            } else {
              inp_conv[y-yy, idx] <- as.numeric(inp_conv[y-yy-1, idx])*as.numeric(1-rfr[y-yy])
            }
          }
        }
      } else {
        if (!is.na(inp_conv[y, idx])) {
          inp_new_b[idx] <- TRUE
          if (inp_new_i[idx]>0) {
            for (yy in seq(inp_new_i[idx])) {
              inp_conv[y-yy, idx] <- inp_conv[y-inp_new_i[idx]-1, idx]
            }
            inp_new_i[idx] <- 0
          }
        }
      }
    }
  }
  return (inp_conv)
}


################################################
# START
################################################

# CONST
date_min <- 2005
date_max <- 2016

# Indexs
if (bLoadFile) {
  sfile <- 'india.csv'
#  sfile <- 'px_test.csv'
  input_px <- LoadData(sfile,3,",")

}

# FIXED Month File
if (FALSE) {
  inp_conv <- input_px
  inp_new <- matrix(nrow=0, ncol=ncol(inp_conv))
  for (y in seq(2,nrow(inp_conv))) {
    if (.indexmon(inp_conv[y-1,])== .indexmon(inp_conv[y,])) {
      for (idx in seq(ncol(inp_conv[y-1]))) {
        if (!is.na(inp_conv[y-1, idx]))
          inp_conv[y,idx] = inp_conv[y-1, idx]
      }
    } else
      inp_new <- rbind(inp_new, as.matrix(inp_conv[y-1,]))
    if (y==nrow(inp_conv))
     inp_new <- rbind(inp_new, as.matrix(inp_conv[y,]))
  }
  input_px <- as.xts(inp_new);
}

# LIMIT
# input_px <- input_px['2002-12/2013-12']


#w_day <- 3 #Wednesday
for (w_day in seq(1,5)) {
  print(paste0("Process wday=", w_day))
  inp_conv <- input_px
  new_weekly <- matrix(nrow=0, ncol=ncol(inp_conv))
  errors <- matrix(nrow=0, ncol=3)
  for (idx_d in seq(1,nrow(inp_conv))) {
    if (.indexwday(inp_conv[idx_d,]) == w_day) {
      for(idx_px in seq(1, ncol(inp_conv))) {
#        print(paste0(idx_d, " - ", idx_px))
        bHave <- FALSE
        if (!is.na(inp_conv[idx_d, idx_px])) {
          bHave <- TRUE
        } else {
          date = .indexDate(inp_conv[idx_d,idx_px])
          for (idx_lag in seq(1, 7)) {
            if (nrow(inp_conv[as.Date(date+idx_lag), idx_px])>0)
              if (!is.na(inp_conv[as.Date(date+idx_lag), idx_px])) {
                inp_conv[idx_d,idx_px] <- inp_conv[as.Date(date+idx_lag), idx_px]
                bHave <- TRUE
                break
              }
          }
          if (bHave)
            errors <- rbind(errors, c(as.character(as.Date(date)), colnames(inp_conv[,idx_px]), inp_conv[idx_d, idx_px]))
          else
            errors <- rbind(errors, c(as.character(as.Date(date)), colnames(inp_conv[,idx_px]), ""))
        }
      }
      new_weekly <- rbind(new_weekly, as.matrix(inp_conv[idx_d,]))
    }
  }
  new_weekly <- as.xts(new_weekly)

  # SAVE
  returns <- new_weekly
  f_AR <- matrix(nrow=nrow(returns), ncol=0)
  f_AR <- cbind(f_AR, i=t(t(returns)))
  write.csv2(f_AR, file=paste0("S9_PXWeekly_WDay", w_day,".csv"))
  
  roc_1meth <- ROC(x = new_weekly, n=1, type = "discrete", na.pad = TRUE)
  # SAVE
  returns <- roc_1meth
  f_AR <- matrix(nrow=nrow(returns), ncol=0)
  f_AR <- cbind(f_AR, i=t(t(returns)))
  write.csv2(f_AR, file=paste0("S9_Ret_1method_WDay", w_day,".csv"))

  m <- matrix("",nrow=length(unique(errors[,1])), ncol=length(unique(errors[,2])), 
              dimnames=list(unique(errors[,1]), unique(errors[,2])))
  for (idx in seq(1, nrow(errors))) {
    if (errors[idx,3] != "")
      m[errors[idx,1], errors[idx, 2]] <- errors[idx,3]
    else
      m[errors[idx,1], errors[idx, 2]] <- "NO"
  }
  m <- apply(m, 2, function(x) gsub("\\.", ",", x))

  # SAVE
  returns <- m
  f_AR <- matrix(nrow=nrow(returns), ncol=0)
  f_AR <- cbind(f_AR, i=t(t(returns)))
  write.csv2(f_AR, file=paste0("S9_Errrors_WDay", w_day,".csv"))
  
}

roc_2meth <- ROC(x = input_px, n = 5, type = "discrete", na.pad = TRUE)

# SAVE
returns <- roc_2meth
f_AR <- matrix(nrow=nrow(returns), ncol=0)
f_AR <- cbind(f_AR, i=t(t(returns)))
write.csv2(f_AR, file=paste0("S9_Ret_2method.csv"))

