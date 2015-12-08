


###########################################################################################
#1) Первая часть — очистка рабочего пространства, задание раб директории 
setwd("/home/nazarov/Рабочий стол/fmlab.hse.ru")

#clear working environment
rm(list=ls()) 

###########################################################################################
#2) Вторая часть -  подготовка данных для анализа

library(XLConnect)

# Загружаем ранее сохраненные цены закрытия, раюботаем с недельными данными, поэтому второй лист
dataStocksRaw <- readWorksheet(loadWorkbook("/home/nazarov/Рабочий стол/fmlab.hse.ru/эмитенты КОТИРОВКИ.xlsx"),sheet=2, endCol = 165)

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
############################

resultData<- data.frame()
for(k in 1:(ncol(dataStocks)/2)){
  
}





sort(as.Date(dataStocks[1,], format="%d.%m.%Y"))
dataStocks[1,1]
as.Date(dataStocks[1,1])
sort(as.Date(dataStocks[1,]))
tem
dataStocks[1,c(3,4)]   

max(as.vector(unlist(dataStocks[1,dateIndex])))

max(unlist(dataStocks[1,dateIndex]))


sort(dataStocks[1,dateIndex])


             
###########################################################################################
#3) Третья часть -  





# Убираем первую колонку с датами
data33 <- data33_plus[, -1]

# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 24 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=12
UP2=8
UP3=24

#UP1=2
#UP2=2
#UP3=2

# N - с учетом отступа
N <- (nrow(data33)-(2+UP3*4))%/%STEP 
#N 227
########################Константы для reality check
R <- 1
# temp <- ret(1, 1, 1, STEP, N, data33, UP1, UP2) 
#T <- 164    
T <- 170 
N_rc <- 500
Q <- 0.5   
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
resultData <- data.frame(1,1,1,1,1,1)
colnames(resultData) <-c("mean","t","p-value","hist_per","moment_per","invest_per")

shuff <- data.frame (p1=1,p2=1,p3=1,flag=0)

for (p1 in 1:UP1 ){
  for (p2 in 0:UP2 ){   
    for (p3 in 1:UP3 ){
      shuff[m,] <- c(p1,p2,p3,0)
      m <-m+1
    }
  }
}

set.seed(42)
shuff$flag <- rnorm(n= m-1, m=1, sd=1)        
shuff <- shuff[order(shuff$flag),]  

m <- 1     
while(m <= UP1*(UP2+1)*UP3){      
  #вектор дельт    
  p1 <- shuff$p1[m]
  p2 <- shuff$p2[m]
  p3 <- shuff$p3[m]
  temp <- ret(p1, p2, p3, STEP, N, data33, UP1, UP2) 
  n <- length(temp)
  resultData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4)    
  cat(m, mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, "\n")
  
  #############################################################REALITY_CHECK##########################################################################      
  temp <- temp-0.06/12
  #f_bar <- mean(temp)
  f_sharp <- mean(temp)/sd(temp)
  
  if(m==1){
    V_bar <- sqrt(n)*f_sharp
    for (k in 1:N_rc){
      #     V_star[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
      V_star[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]])/sd(temp[P_R_ind[,k]]) - f_sharp)
    }         
  }
  else {
    V_bar <- max(sqrt(n)* f_sharp ,V_bar)         
    for (k in 1:N_rc){
      #      t <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
      t <- sqrt(n)*(mean(temp[P_R_ind[,k]])/sd(temp[P_R_ind[,k]]) - f_sharp)
      V_star[k] <-max(t,V_star[k]) 
    }
  }
  
  retBoot <- c(V_star,V_bar)
  resultData[m, "pBoot"] <- 1-(rank(retBoot, ties.method = "first")[501]-1)/500
  #  list(V_bar,V_star)            
  ####################################################################################################################################################  
  
  m <- m+1      
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
# 5) Весь портфель делим на две части - первые 15 столбцов - группа победителей
# оставшиеся 16 - проигравшие
# 6) Считаем среднемесячную доходность
# по формуле ( Суммарная доходность победителей / количество победителей -
# Суммарная доходность проигравших / количество проигравших ) / (количество 
# месяцев инвестирования p3)


ret <- function (p1, p2, p3, STEP, N, d, UP1, UP2)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (d[i-p2,] - d[i-4*p1-p2,])/d[i-4*p1-p2,]
    
    temp2 <- d[,order(-temp1[1,])]
    
    temp3 <- (temp2[i+p3*4,]- temp2[i,])/temp2[i,]
    ans[m] <- (sum(temp3[,1:(ncol(temp3)%/%2)])/(ncol(temp3)%/%2)- 
                 sum(temp3[,(ncol(temp3)%/%2+1):(ncol(temp3))]) /(ncol(temp3)-ncol(temp3)%/%2))/p3
    
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
#4) Четвертая часть -  сохранение результатов на жесткий диск



# сохранение результатов работы
stuff <- list(data=resultData, num=N, V_bar=V_bar,V_star=V_star)  # список ценных объектов
saveRDS(file = "res_rts_26-06-2015 (Q=0.5) shuffle 4.RDS",stuff) # сохраняем всё ценное в файл
mylist <- readRDS("res_rts_26-06-2015 (Q=0.5) shuffle 4.RDS") # читаем из файла что там есть 
#res_sh <- head(mylist$data[order(-mylist$data[,2]) ,],20) # просматриваем лучшие результаты

#res_sh
