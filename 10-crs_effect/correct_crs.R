

#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран


rm(list=ls())



setwd("/home/nick/01-projects/02-fmlab.hse.ru/10-crs_effect/")
#source("R/reality_func2.R")
#RESULT_PATH <- "/home/nazarov/02-fmlab.hse.ru/07 - factor evaluation/results/"



#############################################################################
# Загрузка 
#############################################################################
ret_data <- read.csv(file="data/data.csv", header=TRUE, sep=";", dec=",", stringsAsFactors = F)

#ret_data$ННК.Актив._бывш_НК.Альянс_06
#as.numeric(sub(",", ".", ret_data$ННК.Актив._бывш_НК.Альянс_06, perl=TRUE))

ret_result <- function(dat){
  # dat - датафрейм с первым столбцом из дат и днемными доходностями
  # Даты сохраняем в отдельный вектор
  dates <- dat$date
  dat$date <- NULL
  # вместо пропусков пишем 0, потом меняем нули на 1
  dat[is.na(dat)] <- 0
  dat <- dat+1
  # df_ret - результирующая таблица
  df_ret <-data.frame(date=1, mean_return=1,stability_index=1)
  temp_row <- rep(1,ncol(dat))
  for(i in  1:nrow(dat) ){
    temp_row <- temp_row*as.numeric(dat[i,])
    df_ret[i,1] <- dates[i]
    df_ret[i,2] <-(mean(temp_row)-1)*100
    df_ret[i,3] <- abs(mean(temp_row)-1)/mean(apply(dat[1:i,],2,sd))
  }
  return (df_ret)
}

# Рассмотрим инвестирование на 1,2 и 3 месяца 
# отступ положим равным 91 дню
# сранивать будем по среднему уровню стабильности
# потом возьмем с нужным горизонтом инвестирования портфель с худшими crs
dat <- ret_data[-c(1,2,3), c(T, ret_data[1,]==1) ]
dat$date <- as.character(dat[,1])
dat[,1] <- NULL

min_crs <- ret_data[,-1]
min_crs <- min_crs[,order(min_crs[1,])]
min_crs <- min_crs[-c(1,2,3),1:28]
min_crs$date <- dat$date 

res_max <- ret_result(dat)
res_min <- ret_result(min_crs)

plot(res_max [,2])
plot(res_min [,2])
mean(res_min[,3],na.rm = T)
mean(res_max[,3],na.rm = T)

library(xlsx)
write.xlsx(res_max, 
           "max_crs.xls", sheetName="crs=1")
write.xlsx(res_min, 
           "worst_crs.xls", sheetName="1")
write.xlsx(min_crs, 
           "returns_worst_crs.xls", sheetName="1")

write.csv(x = res_max, file ="crs_1.csv",quote= F,row.names = F,sep = ";" )
write.csv(x = res_min, file ="crs_the_worst.csv",quote= F,row.names = F,sep = ";" )
