

#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран
#
#Считаем ID по недельным ценам закрытия, а не месячным!!!
#
#
#
rm(list=ls())
library(XLConnect)
#library(DT)
#panderOptions('table.split.table', Inf) 
#library(ggplot2)
#library(scales)
#setwd("/home/nick/01-projects/02-fmlab.hse.ru/07 - factor evaluation/")
setwd("/home/nazarov/02-fmlab.hse.ru/07 - factor evaluation/")
source("R/reality_func2.R")


#############################################################################
# Параметры, которые зависят от изучаемой страны

#country_name_eng <- "india"
#
country_name_eng <- "russia"
#country_name_eng <- "brazil"
#country_name_eng <- "china"
#country_name_eng <- "malaysia"
#country_name_eng <- "bangkok"
#country_name_eng <- "india"
#country_name_eng <- "taiwan"
#country_name_eng <- "korea"
#country_name_eng <- "indonesia"
#country_name_eng <- "phil"
#country_name_eng <- "china"


#N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 
#temp <-  ret(4, 0, 4, STEP, N, price_d5, UP1, UP2, 0.1) 

#brazil
#T <- 299 old
#T <- 531

#№malaysia
#T <- 537

#bangkok
#T <- 303

#india
#T <- 323 uppps 305  true value 555

#taiwan
#T <- 294

#korea
##T <- 305 old
#T <- 537

##indonesia
#T <- 505


#phil
#T <- 282

#china
#T <- 520

#russia
#T <- 545

#############################################################################
# Загрузка 
#############################################################################
#price_d5<- readWorksheet(loadWorkbook(paste("data","/stocks_china.xlsx",sep="")),sheet=1)
#price_d5<- read.csv("data/5 days malaysia/stocks_malaysia.csv")
#############################################################################
price_d5 <- read.csv(file="data/stocks_russia.csv", header=TRUE)

#price_d5<- readWorksheet(loadWorkbook("data/stocks_russia.xlsx"),sheet=1)

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
# Создаем тестовое множество
for_test <- list(price_d5, 0.7*nrow(price_d5) )
price_d5  <- price_d5[1:(0.7*nrow(price_d5)),]

#############################################################################
# Константы
# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 24 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=12
UP2=8
UP3=12

N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 
#############################################################################
#TESTING


#############################################################################
temp_for_T <-  returnWrapper(4, 0, 4, STEP, N, price_d5, UP1, UP2, 0.1, 1) 
T <- length(temp_for_T)

# N - с учетом отступа
#n <- T-R+1
#############################################################################
# Процедура формирования портфелей, подсчета статистик и bootstrap
start_time <- Sys.time()

resultDataFull <- price_d5
#N <- (nrow(resultDataFull)-(2+UP3*4))%/%STEP 


start_time <- Sys.time()
m <- 1  
     #realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
     realityCheckData <- data.frame(1,1,1,1,1,1,1,1)

for (p3 in 1:12) {  
    for (percent in c(0.3) ){
        for (p1 in 1:UP1 ){   
            for (p2 in 0:UP2 ){  
             #вектор дельт 
              cat(p1, p2, p3,"\n")
             temp <- returnWrapper(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent,1) 
             #return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
             #return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
             n <- length(temp)
             #realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
             #                              mean(return.winner), mean(return.loser),length(temp[temp<0]))    
             realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                                           length(temp[temp<0]))  
             m <- m+1      
           }
         }
       }       
}
   

end_time <- Sys.time()
end_time

temp2 <-do.call("rbind", temp1)
#colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")
colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent", "Amount_of_negative")

#Сохранение результатов
short_res <- list(data=temp2, num=N, n_portf = T)  # список ценных объектов
saveRDS(file = paste("/home/nazarov/02-fmlab.hse.ru/06 - best strategies/results/",country_name_eng,"_best",".RDS",sep=""),short_res) # сохраняем всё ценное в файл

start_time
end_time









