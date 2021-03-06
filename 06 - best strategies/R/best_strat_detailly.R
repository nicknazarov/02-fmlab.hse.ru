#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран
rm(list=ls())
library(XLConnect)
library(xlsx)
setwd("/home/nazarov/02-fmlab.hse.ru/05 - reverse/")
source("R/reality_func2.R")

#############################################################################
#Россия
#
price_d5<- XLConnect::readWorksheet( XLConnect::loadWorkbook("data/stocks_russia.xlsx"),sheet=1)
#price_d5<- readWorksheet(loadWorkbook("data/5 days brazil/brazil_price_Fri.xlsx"),sheet=1)
##price_d5<- readWorksheet(loadWorkbook("data/5 days malaysia/malaysia_price_Fri.xlsx"),sheet=1)
#price_d5<- readWorksheet(loadWorkbook("data/5 days bangkok/bangkok_price_Fri.xlsx"),sheet=1)
#price_d5 <- read.csv(file="/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days india/india_price_Fri.csv", header=TRUE)
#price_d5 <- read.csv(file="/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days taiwan/taiwan_price_Fri.csv", header=TRUE)
#price_d5<- readWorksheet(loadWorkbook("/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days korea/korea_price_Fri.xlsx"),sheet=1)
##price_d5<- readWorksheet(loadWorkbook("/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days indonesia/indonesia_price_Fri.xlsx"),sheet=1)
#price_d5<- read.csv(file="/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days phil/phil_price_Fri.csv", header=TRUE)
#price_d5<- read.csv(file="/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days china/china_price_Fri.csv", header=TRUE)

row.names(price_d5) <- price_d5[,1]
price_d5 <-price_d5[,-1]
#############################################################################
# Константы
# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 24 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=12
UP2=8
UP3=24
N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 

#Считываем файл с информацией о стратегиях
mylist <- readRDS("/home/nazarov/02-fmlab.hse.ru/06 - best strategies/results/russia_best.RDS") # читаем из файла что там есть 
result.data <- mylist[[1]]

#Сортируем по среднему и т-статистике
#head(result.data[order(abs(result.data[,1]),decreasing = T),])
order_by_mean <- result.data[order(abs(result.data[,1]),decreasing = T),]
order_by_t <- result.data[order(abs(result.data[,2]),decreasing = T),]
  
  
p1_mean <- order_by_mean[1,4] 
p2_mean <- order_by_mean[1,5] 
p3_mean <- order_by_mean[1,6] 
percent_mean <- order_by_mean[1,7] 
temp <- ret_with_date (p1_mean/4 , p2_mean , p3_mean/4 , STEP, N, price_d5, UP1, UP2, percent_mean) 
return.winner<- ret.winner_date(p1_mean/4 , p2_mean , p3_mean/4 , STEP, N, price_d5, UP1, UP2, percent_mean) 
return.loser<- ret.loser_date(p1_mean/4 , p2_mean , p3_mean/4 , STEP, N, price_d5, UP1, UP2, percent_mean) 


p1_t <- order_by_t[1,4] 
p2_t <- order_by_t[1,5] 
p3_t <- order_by_t[1,6] 

temp_for_T <-  ret(p1, p2, p3, STEP, N, price_d5, UP1, UP2, 0.1) 

temp_dat_frame <- data.frame(date= temp[[1]], ret=[[2]],win = return.winner, los =return.loser, delta=return.winner-return.loser)

#file <- paste(tempfile(), "xlsx", sep=".")
#write.xlsx(x, file, sheetName="best_mean")

temp

