

#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран
rm(list=ls())
library(XLConnect)
#library(DT)
#panderOptions('table.split.table', Inf) 
#library(ggplot2)
#library(scales)
setwd("/home/nazarov/02-fmlab.hse.ru/05 - reverse/")
source("R/reality_func2.R")


#############################################################################
# Параметры, которые зависят от изучаемой страны

#country_name_eng <- "India"
#country_name_eng <- "Russia"
#country_name_eng <- "brazil"
#country_name_eng <- "china"
#country_name_eng <- "malaysia"
#country_name_eng <- "bangkok"
#country_name_eng <- "india"
#country_name_eng <- "taiwan"
#country_name_eng <- "korea"
country_name_eng <- "indonesia"
#country_name_eng <- "phil"
#country_name_eng <- "china"


#N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 
#temp <-  ret(4, 0, 4, STEP, N, price_d5, UP1, UP2, 0.1) 

#india
#T <- 166 

#russia
#T <- 313

#brazil
#T <- 299

#china
#T <- 288

#malaysia
#T <- 305

#bangkok
#T <- 303

#india
#T <- 323

#taiwan
#T <- 294


#korea
#T <- 305

##indonesia
T <- 505


#phil
#T <- 282

#china
#T <- 520


#############################################################################
# Загрузка 
#############################################################################
#price_d5<- readWorksheet(loadWorkbook(paste("data","/stocks_china.xlsx",sep="")),sheet=1)
#price_d5<- read.csv("data/5 days malaysia/stocks_malaysia.csv")
#############################################################################

#price_d5<- readWorksheet(loadWorkbook("data/5 days brazil/brazil_price_Fri.xlsx"),sheet=1)

#price_d5<- readWorksheet(loadWorkbook("data/5 days malaysia/malaysia_price_Fri.xlsx"),sheet=1)

#price_d5<- readWorksheet(loadWorkbook("data/5 days bangkok/bangkok_price_Fri.xlsx"),sheet=1)

#price_d5 <- read.csv(file="/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days india/india_price_Fri.csv", header=TRUE)

#price_d5 <- read.csv(file="/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days taiwan/taiwan_price_Fri.csv", header=TRUE)

#price_d5<- readWorksheet(loadWorkbook("/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days korea/korea_price_Fri.xlsx"),sheet=1)

price_d5<- readWorksheet(loadWorkbook("/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days indonesia/indonesia_price_Fri.xlsx"),sheet=1)

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
# N - с учетом отступа
#n <- T-R+1
#############################################################################
# Процедура формирования портфелей, подсчета статистик и bootstrap
start_time <- Sys.time()

#realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
colnames(realityCheckData) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")
resultDataFull <- price_d5
N <- (nrow(resultDataFull)-(2+UP3*4))%/%STEP 

library(parallel)
#nn.p<-function()
#{
print("Параллельное выполнение")
cl <- makeCluster(getOption("cl.cores", 4)) # создание кластера из четырёх ядер процессора
clusterExport(cl,"infert") # передача данных внутрь кластера
clusterEvalQ(cl,source("R/reality_func2.R")) # загрузка функций в кластер
#clusterExport(cl, "UP1", "UP2", "UP3", "STEP", "resultDataFull", "N")
start_time <- Sys.time()
temp1 <- parLapply(cl,  c(0.5,0.3,0.2,0.1), function(percent, UP1, UP2, UP3, STEP, resultDataFull, N) # параллельная версия sapply
{    m <- 1  
     realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
     for (p1 in 1:UP1 ){
       for (p2 in 0:UP2 ){   
         for (p3 in 1:UP3 ){  
           #вектор дельт    
           temp <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           n <- length(temp)
           realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                                         mean(return.winner), mean(return.loser),length(temp[temp<0]))    
           # message(sprintf("%d", Sys.time()))
           #cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, mean(return.winner), mean(return.loser),length(temp[temp<0]), "\n")
           m <- m+1      
         }
       }
     }
     return (realityCheckData)
     
}, UP1, UP2, UP3, STEP, resultDataFull, N)
stopCluster(cl)


end_time <- Sys.time()


temp2 <-do.call("rbind", temp1)
colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")
#Сохранение результатов
short_res <- list(data=temp2, num=N)  # список ценных объектов
saveRDS(file = paste("/home/nazarov/02-fmlab.hse.ru/06 - best strategies/results/",country_name_eng,"_best",".RDS",sep=""),short_res) # сохраняем всё ценное в файл


##################################################################
##################################################################

rm(list=ls())
library(XLConnect)
#library(DT)
#panderOptions('table.split.table', Inf) 
#library(ggplot2)
#library(scales)
setwd("/home/nazarov/02-fmlab.hse.ru/05 - reverse/")
source("R/reality_func2.R")


#############################################################################
# Параметры, которые зависят от изучаемой страны
country_name_eng <- "malaysia"

#N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 
#temp <-  ret(4, 0, 4, STEP, N, price_d5, UP1, UP2, 0.1) 

#malaysia
T <- 537



#############################################################################
# Загрузка 
price_d5<- readWorksheet(loadWorkbook("data/5 days malaysia/malaysia_price_Fri.xlsx"),sheet=1)
#############################################################################

#price_d5<- readWorksheet(loadWorkbook("data/5 days malaysia/malaysia_price_Fri.xlsx"),sheet=1)

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
# N - с учетом отступа
#n <- T-R+1
#############################################################################
# Процедура формирования портфелей, подсчета статистик и bootstrap
start_time <- Sys.time()

#realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
#colnames(realityCheckData) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")
resultDataFull <- price_d5
N <- (nrow(resultDataFull)-(2+UP3*4))%/%STEP 

library(parallel)
#nn.p<-function()
#{
print("Параллельное выполнение")
cl <- makeCluster(getOption("cl.cores", 4)) # создание кластера из четырёх ядер процессора
clusterExport(cl,"infert") # передача данных внутрь кластера
clusterEvalQ(cl,source("R/reality_func2.R")) # загрузка функций в кластер
#clusterExport(cl, "UP1", "UP2", "UP3", "STEP", "resultDataFull", "N")
start_time <- Sys.time()
temp1 <- parLapply(cl,  c(0.5,0.3,0.2,0.1), function(percent, UP1, UP2, UP3, STEP, resultDataFull, N) # параллельная версия sapply
{    m <- 1  
     realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
     for (p1 in 1:UP1 ){
       for (p2 in 0:UP2 ){   
         for (p3 in 1:UP3 ){  
           #вектор дельт    
           temp <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           n <- length(temp)
           realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                                         mean(return.winner), mean(return.loser),length(temp[temp<0]))    
           # message(sprintf("%d", Sys.time()))
           #cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, mean(return.winner), mean(return.loser),length(temp[temp<0]), "\n")
           m <- m+1      
         }
       }
     }
     return (realityCheckData)
     
}, UP1, UP2, UP3, STEP, resultDataFull, N)
stopCluster(cl)


end_time <- Sys.time()


temp2 <-do.call("rbind", temp1)
colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")
#Сохранение результатов
short_res <- list(data=temp2, num=N)  # список ценных объектов
saveRDS(file = paste("/home/nazarov/02-fmlab.hse.ru/06 - best strategies/results/",country_name_eng,"_best",".RDS",sep=""),short_res) # сохраняем всё ценное в файл

















