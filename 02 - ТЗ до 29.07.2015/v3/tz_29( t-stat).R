


###########################################################################################
#1) Первая часть — очистка рабочего пространства, задание раб директории 
setwd("/home/nazarov/Рабочий стол/fmlab.hse.ru/ТЗ до 29.07.2015/")
#setwd("J:/12 - ЛАФР/02 - Декомпозиция")
#clear working environment
rm(list=ls()) 

source("reality_func.R")

###########################################################################################

#4) Четвертая часть -  reality check с разбивкой на процентили (50%, 30%, 20%, 10%)
library(XLConnect)

# Загружаем ранее сохраненные цены закрытия
resultDataFull<- readWorksheet(loadWorkbook("stocks_52.xlsx"),sheet=1)

row.names(resultDataFull) <- resultDataFull[,1]
resultDataFull <-resultDataFull[,-1]


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
        temp <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
        n <- length(temp)
        realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent)    
        cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, "\n")
        
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
        realityCheckData[m, "pBoot"] <- 1-(rank(retBoot, ties.method = "first")[501]-1)/500
        #  list(V_bar,V_star)            
        ####################################################################################################################################################  
        
        m <- m+1      
      }
    }
  }
}







###########################################################################################
#5) Пятая часть -  сохранение результатов на жесткий диск



# сохранение результатов работы
stuff <- list(data=realityCheckData, num=N, V_bar=V_bar,V_star=V_star)  # список ценных объектов
saveRDS(file = "tz_29_v1(Q=0.1) sharp.RDS",stuff) # сохраняем всё ценное в файл
mylist <- readRDS("realityCheckData_percent_v2_09-07-2015 (Q=0.1).RDS") # читаем из файла что там есть 
#res_sh <- head(mylist$data[order(-mylist$data[,2]) ,],20) # просматриваем лучшие результаты

#res_sh

