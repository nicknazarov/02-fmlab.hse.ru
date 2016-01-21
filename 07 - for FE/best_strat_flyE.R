
#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран
rm(list=ls())
library(parallel)
#setwd("/home/nazarov/02-fmlab.hse.ru/05 - reverse/")
#source("R/reality_func2.R")
source("~/workdir/reality_func2.R")
#############################################################################
# Параметры, которые зависят от изучаемой страны
country_name_eng <- "china"
#china
T <- 520


#############################################################################
# Загрузка 
#############################################################################
price_d5<- read.csv(file="~/workdir/china_price_Fri.csv", header=TRUE)
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

resultDataFull <- price_d5
N <- (nrow(resultDataFull)-(2+UP3*4))%/%STEP 

print("Параллельное выполнение")
cl <- makeCluster(getOption("cl.cores", 24)) # создание кластера из четырёх ядер процессора
clusterExport(cl,"infert") # передача данных внутрь кластера
clusterEvalQ(cl,source("~/workdir/reality_func2.R")) # загрузка функций в кластер
start_time <- Sys.time()
temp1 <- parLapply(cl,  1:24, function(p3, UP1, UP2, UP3, STEP, resultDataFull, N) # параллельная версия sapply
{    m <- 1  
     realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
     for (percent in c(0.5,0.3,0.2,0.1) ){
       for (p1 in 0:UP1 ){   
         for (p2 in 1:UP2 ){  
           #вектор дельт    
           temp <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           n <- length(temp)
           realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                                         mean(return.winner), mean(return.loser),length(temp[temp<0]))    
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
results <- list(data=temp2, num=N)  # список ценных объектов
saveRDS(file = "~/workdir/china_best.RDS",results) # сохраняем всё ценное в файл







