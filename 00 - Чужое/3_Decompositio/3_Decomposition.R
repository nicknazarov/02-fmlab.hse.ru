### Lyas script
### 2015-04-07
### 3. Decomposition

#####################################
# Load required packages            #
#####################################
#library(FinancialInstrument)
library(TTR)
#library(PerformanceAnalytics)
#library(quantmod)

#####################################
# CONSTANT                          #
#####################################
# Must be load file
#bLoadFile <- TRUE


#####################################
# Initialize                        #
#####################################
# Remove objects from workspace
list <- c("list", "exceptVar", ls())
exceptVar = c("")
rm(list=(list[!list %in% exceptVar]))

# Path for save Result Files
PathResult = "Result"
dir.create(PathResult, showWarnings = FALSE, recursive = FALSE, mode = "0777")
PathResult <- paste0(PathResult, "/")

#####################################
# Functions                         #
#####################################

Fixed_month <- function (inp) {
  # FIXED Month File
  # All days set to last day of Month (2015-01-29 -> 2015-01-31)
  inp_conv <- inp
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
  return(as.xts(inp_new))
}

Fixed_year <- function (inp) {
  # FIXED Year File
  inp_conv <- inp
  inp_new <- matrix(nrow=0, ncol=ncol(inp_conv))
  for (y in seq(2,nrow(inp_conv))) {
    if ((.indexyear(inp_conv[y-1,])== .indexyear(inp_conv[y,])) && 
          (.indexmon(inp_conv[y-1,])== .indexmon(inp_conv[y,]))) {
      for (idx in seq(ncol(inp_conv[y-1]))) {
        if (!is.na(inp_conv[y-1, idx]))
          inp_conv[y,idx] = inp_conv[y-1, idx]
      }
    } else
      inp_new <- rbind(inp_new, as.matrix(inp_conv[y-1,]))
    if (y==nrow(inp_conv))
      inp_new <- rbind(inp_new, as.matrix(inp_conv[y,]))
  }
  return(as.xts(inp_new))
}

# Load csv-file
LoadData <- function(sfile, f_col=3, f_dec=".", FixM=FALSE, FixY=FALSE){
  # Load data from csv-file and return xts object
  # 
  # args:
  #  sfile = Path to file "input.csv" (string)
  #  f_col = Count cols per one PX (number)
  #  f_dec = Character of float (character)
  #  FixM = Fixed Month (boolean)
  #  FixY = Fixed Year (boolean)
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
#  print(t_m)
  rownames(t_m) <- format.Date(as.character(rd[,1]))
  t_m <- type.convert(t_m, dec=f_dec)
  t_m <- as.xts(t_m)
  if (FixM)
    t_m <- Fixed_month(t_m)
  if (FixY)
    t_m <- Fixed_year(t_m)
  return(t_m)
}

getPortfolioDecomposition <- function(ret_stake, monthly.ret, n_period, m_invest, skip_month=0, portf_name = "Portf"){
  # Get Portfolio By Decomposition
  # 
  # args:
  #  ret_stake = Stake (xts)
  #  monthly.ret = Monthly Returns (xts)
  #  n_period = N-Period (number)
  #  m_invest = Period of investition (number)
  #  skip_month = Count of month on skip (number)
  #  portf_name = Name of column of portfolio
  #
  # Returns:
  #  returns an xts object of Portfolio
  if (nrow(ret_stake)-m_invest-skip_month>=1) {
    # Setup temp portfolio
    tmp_portfolio <- lag(ret_stake[,1], k=m_invest+skip_month, na.pad=FALSE)
    tmp_portfolio <- apply.daily( tmp_portfolio, function(x) NA)
    names(tmp_portfolio) <- paste0(portf_name, " n", n_period, " m", m_invest)
    
    for (m_idx in seq(1,nrow(ret_stake)-m_invest-skip_month)) {
      # Check out of Bounds
      if ((m_idx+m_invest+skip_month)<=nrow(ret_stake)) {
        tmp <- mean(
          # Summ by Row
          apply(
            # Every Row multiply by Monthly Return
            apply.daily( ret_stake[m_idx:(m_idx+m_invest-1),],
                         function(x) x*as.vector( monthly.ret[index(ret_stake[(m_idx+m_invest+skip_month),])]) ),
            1, sum)
        )
        # Set in tmp_portfolio
        tmp_portfolio[index(ret_stake[(m_idx+m_invest+skip_month),])] <- tmp
      }
    }
    # Add in portfolio
    returns[[n_period]]$portfolio <- merge(returns[[n_period]]$portfolio, tmp_portfolio)
    return(tmp_portfolio)
  } else
    return(NULL)
}
  


#####################################
# Start program                     #
#####################################

#sfile <- 'px_short_1.csv'
#input_px <- LoadData(sfile,3,",", TRUE)
sfile <- 'final.csv'
input_px <- LoadData(sfile,3,".", TRUE)

# LIMITS
#input_px <- input_px['2007-01/2008-01']
#input_px <- input_px[, seq(ncol(input_px)-3, ncol(input_px))]

# Calculate Montly Returns
monthly.returns <- ROC(x = input_px, n = 1, type = "discrete", na.pad = TRUE)

# 1. Calculate Average(Mean), Median, Standar Deviation of Monthly Returns
monthly.returns.mean <- apply(monthly.returns, 2, mean, na.rm=TRUE)
monthly.returns.median <- apply(monthly.returns, 2, median, na.rm=TRUE)
monthly.returns.sd <- apply(monthly.returns, 2, sd, na.rm=TRUE)

# SAVE Monthly returns
f_AR <- matrix(nrow=nrow(monthly.returns), ncol=0)
f_AR <- cbind(f_AR, t(t(monthly.returns)))
f_AR <- rbind(f_AR, monthly.returns.mean)
f_AR <- rbind(f_AR, monthly.returns.median)
f_AR <- rbind(f_AR, monthly.returns.sd)
write.csv2(f_AR, file=paste0(PathResult, "Monthly_returns.csv"))

# 2. Calculate Returns by n-period (n=1,3,6,9,12)
returns <- list()
# seq(3, 12, 3)
for (n_period in c(1:12)) {
  print(paste0("Calculate N-Period=", n_period))
  returns[[n_period]] <- list()
  returns[[n_period]]$ret <- ROC(x = input_px, n = n_period, type = "discrete", na.pad = TRUE)
  returns[[n_period]]$meanByMonth <- apply(returns[[n_period]]$ret, 1, mean, na.rm=TRUE)
  returns[[n_period]]$xtsmeanByMonth <- as.xts(returns[[n_period]]$meanByMonth)
  
  # 3. Calculate Stake
  #  returns[[n_period]]$stake <- (returns[[n_period]]$ret-returns[[n_period]]$meanByMonth)/ncol(returns[[n_period]]$ret)
  returns[[n_period]]$countpx <- returns[[n_period]]$ret
  returns[[n_period]]$countpx[!is.na(returns[[n_period]]$countpx)] <- 1
  returns[[n_period]]$countpx <- apply(returns[[n_period]]$countpx, 1, sum, na.rm=TRUE)
  returns[[n_period]]$countpx[returns[[n_period]]$countpx<=0] <- NA
  # 3. Calculate Stake
  returns[[n_period]]$stake <- (returns[[n_period]]$ret-returns[[n_period]]$meanByMonth) / returns[[n_period]]$countpx

  # 3.2. Calculate Stake With Koefficient
  returns[[n_period]]$stakeKoef1 <- returns[[n_period]]$stake * (returns[[n_period]]$stake > 0)
  returns[[n_period]]$stakeKoef2 <- 1/apply(returns[[n_period]]$stakeKoef1, 1, sum, na.rm=TRUE)
  returns[[n_period]]$stake2 <- returns[[n_period]]$stake * returns[[n_period]]$stakeKoef2
    
  # SAVE Stake1
  f_AR <- matrix(nrow=nrow(returns[[n_period]]$stake), ncol=0)
  f_AR <- cbind(f_AR, t(t(returns[[n_period]]$stake)))
  f_AR <- cbind(f_AR, "Count" = returns[[n_period]]$countpx)
  write.csv2(f_AR, file=paste0(PathResult, "Stake1_n",n_period,".csv"))

  # SAVE Stake2
  f_AR <- matrix(nrow=nrow(returns[[n_period]]$stake2), ncol=0)
  f_AR <- cbind(f_AR, "Koef"= returns[[n_period]]$stakeKoef2)
  f_AR <- cbind(f_AR, t(t(returns[[n_period]]$stake2)))
  write.csv2(f_AR, file=paste0(PathResult, "Stake2_n",n_period,".csv"))
  
  # SAVE N-Returns
  f_AR <- matrix(nrow=nrow(returns[[n_period]]$ret), ncol=0)
  f_AR <- cbind(f_AR, t(t(returns[[n_period]]$ret)))
  f_AR <- cbind(f_AR, "Rm" = returns[[n_period]]$meanByMonth)
  write.csv2(f_AR, file=paste0(PathResult, "Returns_n", n_period, ".csv"))
  
  for (m_period in c(1:12)) {
    print(paste0("M-Period=", m_period))
    returns[[n_period]]$Mret <- ROC(x = input_px, n = m_period, type = "discrete", na.pad = TRUE)
    
    for (i in c(1:ncol(returns[[n_period]]$ret))) {
      returns[[n_period]]$Mret[,i][is.na(lag(returns[[n_period]]$ret[,i], k=m_period))] <- NA
    }
    
    returns[[n_period]]$MmeanByMonth <- apply(returns[[n_period]]$Mret, 1, mean, na.rm=TRUE)
    returns[[n_period]]$MxtsmeanByMonth <- as.xts(returns[[n_period]]$MmeanByMonth)
    
# 4. Calculate returns of Portfolio
    if (nrow(returns[[n_period]]$stake)>0) {
      if (nrow(returns[[n_period]]$stake)>m_period) {
        returns[[n_period]]$Portf1 <- apply.daily(lag(returns[[n_period]]$stake2, k=m_period)*returns[[n_period]]$Mret, sum, na.rm = TRUE)

# 5. Part 
        returns[[n_period]]$A5parRm <- lag(returns[[n_period]]$xtsmeanByMonth, k=m_period)*
                                      returns[[n_period]]$MxtsmeanByMonth
        returns[[n_period]]$A5PartTicket <- (lag(returns[[n_period]]$ret, k=m_period)*returns[[n_period]]$Mret)-
                                  (lag(returns[[n_period]]$ret, k=m_period)^2)
        returns[[n_period]]$A5SumPartTicket <- apply.daily(returns[[n_period]]$A5PartTicket, sum, na.rm = TRUE)
#        returns[[n_period]]$A5Count <- ncol(returns[[n_period]]$A5PartTicket)

        returns[[n_period]]$A5Count <- returns[[n_period]]$A5PartTicket
        returns[[n_period]]$A5Count[!is.na(returns[[n_period]]$A5Count)] <- 1
        returns[[n_period]]$A5Count <- apply(returns[[n_period]]$A5Count, 1, sum, na.rm=TRUE)
        returns[[n_period]]$A5Count[returns[[n_period]]$A5Count<=0] <- NA

        returns[[n_period]]$A5parC <- (returns[[n_period]]$A5parRm - 
                                  (lag(returns[[n_period]]$xtsmeanByMonth, k=m_period)^2) - 
                                  (returns[[n_period]]$A5SumPartTicket/(returns[[n_period]]$A5Count^2))) *
                                  lag(as.xts(returns[[n_period]]$stakeKoef2), k=m_period)
        returns[[n_period]]$A5parO <- returns[[n_period]]$A5SumPartTicket *
                                      (returns[[n_period]]$A5Count-1) /
                                      (returns[[n_period]]$A5Count^2) *
                                      lag(as.xts(returns[[n_period]]$stakeKoef2), k=m_period)
        returns[[n_period]]$A5parS <- as.xts((returns[[n_period]]$A5Count-1) / returns[[n_period]]$A5Count *
                                      apply(lag(returns[[n_period]]$ret, k=m_period), 1, var, na.rm= TRUE)) *
                                      lag(as.xts(returns[[n_period]]$stakeKoef2), k=m_period)
        returns[[n_period]]$Portf2 <- returns[[n_period]]$A5parS + returns[[n_period]]$A5parO - returns[[n_period]]$A5parC
        returns[[n_period]]$PortfMean2 <- mean(returns[[n_period]]$Portf2, na.rm=TRUE)
        returns[[n_period]]$PortfSD2 <- sd(returns[[n_period]]$Portf2, na.rm=TRUE)
        nc <- nrow(na.omit(as.matrix(returns[[n_period]]$Portf2)))
        returns[[n_period]]$PortfZStat2 <- returns[[n_period]]$PortfMean2 / returns[[n_period]]$PortfSD2 / (nc^0.5)

    # Remove rows with NA
#      returns[[n_period]]$A5PartTicket <- na.omit(returns[[n_period]]$A5PartTicket)
#      returns[[n_period]]$A5parRm <- na.omit(returns[[n_period]]$A5parRm[index(returns[[n_period]]$A5PartTicket)])
#      returns[[n_period]]$A5parC <- na.omit(returns[[n_period]]$A5parC[index(returns[[n_period]]$A5PartTicket)])
#      returns[[n_period]]$A5parO <- na.omit(returns[[n_period]]$A5parO[index(returns[[n_period]]$A5PartTicket)])
#      returns[[n_period]]$A5parS <- na.omit(returns[[n_period]]$A5parS[index(returns[[n_period]]$A5PartTicket)])
#      returns[[n_period]]$Portf2 <- na.omit(returns[[n_period]]$Portf2[index(returns[[n_period]]$A5PartTicket)])
      # Set Names by Parameters
        names(returns[[n_period]]$A5parRm) <- c("AR1")
        names(returns[[n_period]]$A5parC) <- c("C")
        names(returns[[n_period]]$A5parO) <- c("O")
        names(returns[[n_period]]$A5parS) <- c("S")
        names(returns[[n_period]]$Portf1) <- c("P1")
        names(returns[[n_period]]$Portf2) <- c("P2")
#        names(returns[[n_period]]$PortfMean2) <- c("mean")
#        names(returns[[n_period]]$PortfSD2) <- c("sd")
#        names(returns[[n_period]]$PortfZStat2) <- c("zstat")

# SAVE Portfolio
        if (!is.null(nrow(returns[[n_period]]$A5PartTicket))) {
          f_AR <- matrix(nrow=nrow(returns[[n_period]]$A5PartTicket), ncol=0)
          f_AR <- cbind(f_AR, t(t(returns[[n_period]]$A5PartTicket)))
          f_AR <- cbind(f_AR, t(t(returns[[n_period]]$A5parRm)))
          f_AR <- cbind(f_AR, t(t(returns[[n_period]]$A5parC)))
          f_AR <- cbind(f_AR, t(t(returns[[n_period]]$A5parO)))
          f_AR <- cbind(f_AR, t(t(returns[[n_period]]$A5parS)))
          f_AR <- cbind(f_AR, t(t(returns[[n_period]]$Portf1)))
          f_AR <- cbind(f_AR, t(t(returns[[n_period]]$Portf2)))
          f_AR <- cbind(f_AR, "mean"=returns[[n_period]]$PortfMean2)
          f_AR <- cbind(f_AR, "sd"=returns[[n_period]]$PortfSD2)
          f_AR <- cbind(f_AR, "zstat"=returns[[n_period]]$PortfZStat2)
          write.csv2(f_AR, file=paste0(PathResult, "Portf_n",n_period,"_m",m_period,".csv"))
        }
      }
    }
  }
}
print(paste0("Finished script"))
