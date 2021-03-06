#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран
#
#Считаем ID по недельным ценам закрытия, а не месячным!!!
#
rm(list=ls())
library(XLConnect)
library(parallel)

setwd("/home/nazarov/02-fmlab.hse.ru/08-bonds")
#setwd("/home/nazarov/02-fmlab.hse.ru/08 - bonds/")
source("R/reality_func2.R")
#RESULT_PATH <- "/home/nazarov/02-fmlab.hse.ru/08 - bonds/results/"
RESULT_PATH <- "/home/nazarov/02-fmlab.hse.ru/08-bonds/results/"
#############################################################################
#### Параметры, определяющие стратегию
#############################################################################
rankingFactor <- 0

#############################################################################
#### Параметры, получаемые из командной строки
#############################################################################
args <- commandArgs(trailingOnly = TRUE)
country_name_eng  <- args[1]
file_name <- args[2]

#############################################################################
#### Параметры, которые зависят от изучаемой страны
#############################################################################
#country_name_eng <- "russia_bonds"

#############################################################################
### Выбираем только пятницу
##############################################################################
#price_d5<- read.csv(file="/home/nazarov/02-fmlab.hse.ru/08-bonds/data/germany_bonds_daily.csv", header=TRUE, sep = ";",stringsAsFactors=FALSE, dec = ",")
#t <- sub(".", "/", price_d5[,1], fixed = T)
#t <- sub(".", "/", t, fixed = T)
#price_d5[,1] <- as.Date(t, format = "%d/%m/%Y" )
#price_d5$days <- weekdays(price_d5[,1])
#price_d5 <- price_d5[price_d5$days =="Пятница", ]
#price_d5$days <- NULL
#price_d5 <- price_d5[order(price_d5[,1], decreasing = F), ]
#write.table(price_d5, file="/home/nazarov/02-fmlab.hse.ru/08-bonds/data/germany_bonds.csv", append = FALSE, quote = F, sep = ";",
#            eol = "\n", na = "NA", dec = ".", row.names = F,
#            col.names = TRUE, qmethod = c("escape", "double"),
#            fileEncoding = "")
#############################################################################
### Загрузка 
#############################################################################
#price_d5<- readWorksheet(loadWorkbook("data/bonds.xls"),sheet=1)
#price_d5<- read.csv(file=paste0("/home/nazarov/02-fmlab.hse.ru/08-bonds/data/",file_name), header=TRUE)
price_d5<- read.csv(file=paste0("/home/nazarov/02-fmlab.hse.ru/08-bonds/data/",file_name),  header=TRUE, sep = ";", dec = ",")


row.names(price_d5) <- price_d5[,1]
price_d5 <-price_d5[,-1]

#############################################################################
#### Создаем тестовое множество
#############################################################################
#per_learn <- 1
#for_test <- list(price_d5, per_learn*nrow(price_d5) )
#price_d5  <- price_d5[1:floor(per_learn*nrow(price_d5)),]

#############################################################################
#### Константы
#############################################################################
# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 24 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=12
UP2=8
UP3=12
#N_test <- (nrow(for_test[[1]])-(2+UP3*4))%/%STEP 
N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 

#############################################################################
#### Определение количества портфелей
#############################################################################
temp_for_T <-  returnWrapper(4, 0, 4, STEP, N, price_d5, UP1, UP2, 0.1, 0) 
T <- length(temp_for_T)

# N - с учетом отступа
#n <- T-R+1
#############################################################################
#### Процедура формирования портфелей, подсчета статистик 
#############################################################################
start_time <- Sys.time()
resultDataFull <- price_d5

print("Параллельное выполнение")
cl <- makeCluster(getOption("cl.cores", 4)) # создание кластера из четырёх ядер процессора
clusterExport(cl,"infert") # передача данных внутрь кластера
clusterEvalQ(cl,source("R/reality_func2.R")) # загрузка функций в кластер
#clusterExport(cl, "UP1", "UP2", "UP3", "STEP", "resultDataFull", "N")
start_time <- Sys.time()
temp1 <- parLapply(cl,  1:4, function(temp_p3, UP1, UP2, UP3, STEP, resultDataFull, N, rankingFactor) # параллельная версия sapply
{    m <- 1  
#realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
realityCheckData <- data.frame(1,1,1,1,1,1,1,1)
low <- (temp_p3-1)*3+1
up <- temp_p3*3
for (p3 in low:up) {  
  for (percent in c(0.1, 0.2, 0.3, 0.4, 0.5) ){
    for (p1 in 1:UP1 ){   
      for (p2 in 0:UP2 ){  
        #вектор дельт   
        cat(p1,p2,p3, "/n")
        temp <- returnWrapper(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent, rankingFactor) 
        #return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
        #return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
        n <- length(temp)
        #realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
        #                              mean(return.winner), mean(return.loser),length(temp[temp<0]))    
         realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                              length(temp[temp < 0]))  
        m <- m+1      
      }
    }
  }       
}
return (realityCheckData)

}, UP1, UP2, UP3, STEP, resultDataFull, N, rankingFactor)
stopCluster(cl)

end_time <- Sys.time()
end_time

temp2 <-do.call("rbind", temp1)
#colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")
colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent", "Amount_of_negative")

#############################################################################
#### Вектор доходностей для тестового множества 
#############################################################################
#percent <- 0.3
#p1_sharp <- temp2 [order(temp2[,2], decreasing = TRUE), 4]/4 
#p2_sharp <- temp2 [order(temp2[,2], decreasing = TRUE), 5] 
#p3_sharp <- temp2 [order(temp2[,2], decreasing = TRUE), 6]/4

#out_of_sample_ret <- returnWrapper(p1_sharp[1], p2_sharp[1], p3_sharp[1],  ceiling(for_test[[2]]), N_test, for_test[[1]], UP1, UP2, percent,3) 
#tttt<- data.frame(out_of_sample_ret) 
#hist(out_of_sample_ret)
#mean(out_of_sample_ret)/p3_sharp[1]
#Сохранение результатов
#if(per_learn!=1){
#  results <- list(data=temp2, num=N, n_portf = T, for_test_data_rowNumber =for_test, out_of_sample_ret  = out_of_sample_ret  )  # список ценных объектов
#}else{
  results <- list(data=temp2, num=N, n_portf = T)  # список ценных объектов
#}

#saveRDS(file = paste(RESULT_PATH ,"result_",per_learn, "_",country_name_eng,"_f",rankingFactor,"_",Sys.time() ,".RDS",sep=""),results) # сохраняем всё ценное в файл
saveRDS(file = paste(RESULT_PATH ,"result_", "_",country_name_eng,"_f",rankingFactor,"_",Sys.time() ,".RDS",sep=""),results) # сохраняем всё ценное в файл


start_time
end_time

###############################################################################
# FOR TEST
###############################################################################
#12  1	24
#12  0	24

#out_of_sample_ret 

#i_start <- ceiling(for_test[[2]])
#tttt2 <- for_test[[1]]  

#source("R/reality_func2.R")
#temp_for_T <-  returnWrapper(3, 0, 6, STEP, N, price_d5, UP1, UP2, 0.3, 1) 

#names(temp_for_T) <- NULL
#str(temp_for_T )
#sum (temp_for_T )
#is.na(temp_for_T)

#for(i in 1:length(temp_for_T)){
#  if(is.nan(temp_for_T [i])) print(i)
#}

#source("R/reality_func2.R")
#temp_for_T <-  returnWrapper(10, 8, 11, i_start, N_test, for_test[[1]] , UP1, UP2, 0.3, 3) 
#mean(temp_for_T)/11

#source("R/reality_func2.R")
#tttt <- as.data.frame( returnWrapper (11, 0, 2, 1, nrow(price_d5) - 8, price_d5, UP1, UP2, 0.1, 0))

#mean(returnWrapper (11, 0, 2, 1, nrow(price_d5) - 8, price_d5, UP1, UP2, 0.1, 0))
#min (returnWrapper (11, 0, 2, 1, nrow(price_d5) - 8, price_d5, UP1, UP2, 0.1, 0))
#sum(returnWrapper (11, 0, 2, 1, nrow(price_d5) - 8, price_d5, UP1, UP2, 0.1, 0)<0)

#source("R/reality_func3.R")
#as.data.frame(ret_with_date (4, 1, 1, 1, N, price_d5, UP1, UP2, 0.1)) 

