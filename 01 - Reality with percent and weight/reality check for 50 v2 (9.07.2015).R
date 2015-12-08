


###########################################################################################
#1) Первая часть — очистка рабочего пространства, задание раб директории 
setwd("/home/nazarov/Рабочий стол/fmlab.hse.ru")
#setwd("J:/12 - ЛАФР/02 - Декомпозиция")
#clear working environment
rm(list=ls()) 

###########################################################################################
#2) Вторая часть -  подготовка данных для анализа

library(XLConnect)

# Загружаем ранее сохраненные цены закрытия, раюботаем с недельными данными, поэтому второй лист
#dataStocksRaw <- readWorksheet(loadWorkbook("/home/nazarov/Рабочий стол/fmlab.hse.ru/эмитенты КОТИРОВКИ.xlsx"),sheet=2, endCol = 165)
dataStocksRaw <- readWorksheet(loadWorkbook("эмитенты КОТИРОВКИ.xlsx"),sheet=2, endCol = 165, )

cleanData <-c()
for(k in 1:((length(colnames(dataStocksRaw))-2)/3)){
  cleanData[k] <-k*3
}

#Удаляем лишние столбцы
dataStocks <- dataStocksRaw[,-cleanData]

#Удаляем первую строку и переименовываем столбцы
dataStocks <- dataStocks[-1,]
i<-1
while(i <length(colnames(dataStocks))){
  temp <- colnames(dataStocks)[i]
  colnames(dataStocks)[i]  <-paste("Date",i)
  colnames(dataStocks)[i+1]  <- temp 
  i<-i+2
}

# Проверяем временные диапазоны
dateIndex <-c()
lastDate <-c()
temp<-1
for(k in 1:(ncol(dataStocks)/2)){
  dateIndex  [k] <-temp
  
  for(j in 1:(nrow(dataStocks))){
    if(is.na(dataStocks[j,temp] )){
      lastDate<-c(lastDate, dataStocks[j-1,temp])
      break
    }
    if(j==nrow(dataStocks)){
      lastDate<-c(lastDate, dataStocks[j,temp])
    }
  }
  
  temp <-k*2+1  
}


min(lastDate)
maxDate<-max(lastDate)

minDate<-max(unlist(dataStocks[1,dateIndex]))

# Формируем новую таблицу с данными в нужном диапазоне (minDate, maxDate)
# Добавляем первый столбец с датами в нужном диапазоне 
resultData<- data.frame()
i<-1
for(k in 1:nrow(dataStocks)){
  if(!is.na(dataStocks[k,1])){
    if(dataStocks[k,1]>=minDate & dataStocks[k,1]<=maxDate){
      resultData[i,1]<-dataStocks[k,1]
      i<-i+1
    } 
  }    
}


# Заполнение недельными ценами закрытия

for(j in 1:(ncol(dataStocks)/2)){
  i<-1
  for(k in 1:nrow(dataStocks)){
    if(!is.na(dataStocks[k,2*j])){
    
      if(dataStocks[k,2*j-1]>=minDate & dataStocks[k,2*j-1]<=maxDate){
        resultData[i,j+1]<-dataStocks[k,2*j]
        i<-i+1
      } 
    }    
  }
  
}

# Названия тикеров для столбцов
i<-2
j<-2
while(i <=length(colnames(dataStocks))){
  colnames(resultData)[j]  <-colnames(dataStocks)[i] 
  j<-j+1
  i<-i+2
}

# Удаляем компании с неполной информацией и сохраняем названия их тикеров
delCol<-c()
for(j in 2:ncol(resultData)){
  if(is.na(resultData[nrow(resultData),j])){
    delCol<-c(delCol,colnames(resultData)[j])
  }
}

colnames(resultData)[1]<-c("Date")
resultDataFull<-resultData[,!(names(resultData) %in% delCol)]

###########################################################################################
#3) Третья часть -  сохраняем итоговую таблицу в excel документ

wb <- loadWorkbook("stocks_52.xlsx", create = TRUE)
createSheet(wb, "stocks")
writeWorksheet(wb, resultDataFull, "stocks",)
saveWorkbook(wb)


###########################################################################################
#4) Четвертая часть -  reality check с разбивкой на процентили (50%, 30%, 20%, 10%)




# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 12 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=12
UP2=8
UP3=12

#UP1=2
#UP2=2
#UP3=2

# N - с учетом отступа
N <- (nrow(resultDataFull)-(2+UP3*4))%/%STEP 
#N 227
########################Константы для reality check
R <- 1
# 

#temp <- ret(4, 1, 4, STEP, N, resultDataFull[,-1], UP1, UP2, 0.3) 

#T <- 164    
T <- 74
N_rc <- 500
Q <- 0.1  
#n <- T-R+1

set.seed(42)
P_R_ind <- data.frame(P_R(R,T,Q))
for (i in 2:N_rc){
  P_R_ind[,i]  <-  P_R(R,T,Q)
}

V_star <- c()
f_bar  <- c() 

########################


m <- 1
realityCheckData <- data.frame(1,1,1,1,1,1,1)
colnames(realityCheckData) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent")



for (percent in c(0.5,0.3,0.2,0.1) ){ 
  for (p1 in 1:UP1 ){
    for (p2 in 0:UP2 ){   
      for (p3 in 1:UP3 ){  
        #вектор дельт    
        temp <- ret(p1, p2, p3, STEP, N, resultDataFull[,-1], UP1, UP2, percent) 
        n <- length(temp)
        realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent)    
        cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, "\n")
        
        #############################################################REALITY_CHECK##########################################################################      
        temp <- temp-0.06/12
        f_bar <- mean(temp)
        
        if(m==1){
          V_bar <- sqrt(n)*mean(temp)
          for (k in 1:N_rc){
            V_star[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
          }         
        }
        else {
          V_bar <- max(sqrt(n)*f_bar,V_bar)         
          for (k in 1:N_rc){
            t <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
            V_star[k] <-max(t,V_star[k]) 
          }
        }
        
        retBoot <- c(V_star,V_bar)
        realityCheckData[m, "pBoot"] <- 1-(rank(retBoot, ties.method = "first")[501]-1)/500
        #  list(V_bar,V_star)            
        ####################################################################################################################################################  
        
        m <- m+1      
      }
    }
  }
}






# Функция ret() возвращает вектор из дельт, а именно
# 1) считаем доходность для всех акций из портфеля по формуле
# ( Цена_закрытия p2 недель назад) - Цена_закрытия(p1*4 + p2 недель назад) )
# /  Цена_закрытия(p1*4 + p2 недель назад)
# 2) Записываем подсчитанные доходности в отдельную таблицу temp1
# 3) Сортируем столбцы исходной таблицы (с ценами закрытия)
# по доходностям и  записываем
# столбцы с ценами закрытия  в новом порядке в таблицу temp2
# 4)  Для таблицы temp3 считаем доходность по формуле  
# ( Цена_закрытия(спустя p3*4 недель) - Цена_закрытия_сегодня )
# / Цена_закрытия_сегодня
# 5) Весь портфель делим на две части - согласно параметру percent
# 6) Считаем среднемесячную доходность
# по формуле ( Суммарная доходность победителей / количество победителей -
# Суммарная доходность проигравших / количество проигравших ) / (количество 
# месяцев инвестирования p3)

ret <- function (p1, p2, p3, STEP, N, d, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
 
    if(percent==0.5){
      ans[m] <- (sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent)- 
                   sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent)))/p3
    }
    else{
      ans[m] <- (sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent)- 
                   sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent))/p3
      

    }

   # print(floor(length(temp3)*percent))
   # print(ceiling(length(temp3)*percent))            
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}


###########################################################################################
P_R <- function (R,T,q){
  
  t=R
  Tetta <- c()
  Tetta[t-R+1] <- sample(R:T, 1)
  
  while(1){
    t <- t+1
    if(t>T) break
    U <- runif(1, 0, 1)
    if(U<q) {
      Tetta[t-R+1] <- sample(R:T, 1)
    }
    
    if(U>=q) {
      Tetta[t-R+1] <- Tetta[t-R]+1  
      
      if(Tetta[t-R+1]>T) {
        Tetta[t-R+1] <- R
      } 
    }
  } 
  
  Tetta   
  
}


###########################################################################################
#5) Пятая часть -  сохранение результатов на жесткий диск



# сохранение результатов работы
stuff <- list(data=realityCheckData, num=N, V_bar=V_bar,V_star=V_star)  # список ценных объектов
saveRDS(file = "realityCheckData_percent_v2_09-07-2015 (Q=0.1).RDS",stuff) # сохраняем всё ценное в файл
mylist <- readRDS("realityCheckData_percent_v2_09-07-2015 (Q=0.1).RDS") # читаем из файла что там есть 
#res_sh <- head(mylist$data[order(-mylist$data[,2]) ,],20) # просматриваем лучшие результаты

#res_sh

###########################################################################################
# testing

setwd("~/Рабочий стол/Reality check/v6")


dd <- realityCheckData[ (realityCheckData$percent==0.5)  ,]
head( dd[order((- dd[,1]) )   ,],30)


head( realityCheckData[order(- realityCheckData[,1]) ,],30)
head( realityCheckData[order(- realityCheckData[,2]) ,],30)

head( realityCheckData[order(- realityCheckData[,1]) ,-8],20)


plot ( realityCheckData$mean)
plot( realityCheckData$pBoot) 
V_bar/sqrt(74)+0.06/12
ret_p <- c(V_star,V_bar)
1-(rank(ret_p, ties.method = "first")[501]-1)/500

res_sh <- head( realityCheckData[order(- realityCheckData[,1]) ,],20) # просматриваем лучшие результаты
res_sh




mylist <- readRDS("res_rts_20-05-2015.RDS") # читаем из файла что там есть
res_sh2 <- head(mylist$data[order(-mylist$data[,1]) ,],20) # просматриваем лучшие результаты
res_sh2
V_star <- mylist[[4]]
V_bar <- mylist[[3]]
ret_p <- c(V_star,V_bar)
1-(rank(ret_p, ties.method = "first")[501]-1)/500




mylist <- readRDS("res_rts_19-05-2015.RDS") # читаем из файла что там есть
res_sh3 <- head(mylist$data[order(-mylist$data[,1]) ,],20) # просматриваем лучшие результаты
res_sh3


 #ret(11, 2, 1, STEP, N, resultDataFull[,-1], UP1, UP2, 0.5) 



