
#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран
#
#Считаем ID по недельным ценам закрытия, а не месячным!!!
#

rm(list=ls())
library(parallel)
#if(!require(XLConnect)){
#    install.packages("XLConnect")
#    library(XLConnect)
#}
source("~/workdir/08-bonds/R/reality_func2.R")
RESULT_PATH <- "~/workdir/"
#rankingFactor <- 0

#############################################################################
#### Параметры, получаемые из командной строки
#############################################################################
args <- commandArgs(trailingOnly = TRUE)

result_file_name  <- args[1]
file_name <- args[2]
rankingFactor <- as.numeric(args[3])
flag_load <- args[4]
#############################################################################
# Параметры, которые зависят от изучаемой страны
#country_name_eng <- "europe_bonds"

#############################################################################
# Загрузка 

#price_d5<- readWorksheet(loadWorkbook("data/bonds.xls"),sheet=1)
#price_d5<- read.csv(file="~/workdir/europe_bonds.csv",  header=TRUE, sep = ";", dec = ",")
if(flag_load =="c"){
  price_d5<- read.csv(file=paste0("~/workdir/",file_name),  header=TRUE, sep = ";", dec = ",")
}
if(flag_load =="p"){
  price_d5<- read.csv(file=paste0("~/workdir/",file_name),  header=TRUE)
}


#/home/nazarov/10-FlyElephant/
#price_d5<- read.csv(file="/home/nazarov/10-FlyElephant/bonds.csv", header=TRUE)

row.names(price_d5) <- price_d5[,1]
price_d5 <-price_d5[,-1]
#############################################################################
# Создаем тестовое множество
#per_learn <- 1
#for_test <- list(price_d5, per_learn*nrow(price_d5) )
#price_d5  <- price_d5[1:floor(per_learn*nrow(price_d5)),]

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
#N_test <- (nrow(for_test[[1]])-(2+UP3*4))%/%STEP 
N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 

#############################################################################
temp_for_T <-  returnWrapper(4, 0, 4, STEP, N, price_d5, UP1, UP2, 0.1, 0) 
T <- length(temp_for_T)

# N - с учетом отступа
# n <- T-R+1
#############################################################################
# Процедура формирования портфелей, подсчета статистик
start_time <- Sys.time()

resultDataFull <- price_d5

print("Параллельное выполнение")
cl <- makeCluster(getOption("cl.cores", 4)) # создание кластера из четырёх ядер процессора
clusterExport(cl,"infert") # передача данных внутрь кластера
clusterEvalQ(cl,source("~/workdir/08-bonds/R/reality_func2.R")) # загрузка функций в кластер
#clusterExport(cl, "UP1", "UP2", "UP3", "STEP", "resultDataFull", "N")
start_time <- Sys.time()
temp1 <- parLapply(cl,  1:4, function(temp_p3, UP1, UP2, UP3, STEP, resultDataFull, N, rankingFactor) # параллельная версия sapply
{   
m <- 1  
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
                              length(temp[temp<0]))  
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
colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent", "Amount_of_negative")

##################################################################
#Сохранение результатов
results <- list(data=temp2, num=N, n_portf = T)  # список ценных объектов

#results <- "tratata"
#rankingFactor <- 0
#country_name_eng <- "russia_bonds"
#getwd()
saveRDS(file =result_file_name, results) # сохраняем всё ценное в файл

start_time
end_time
