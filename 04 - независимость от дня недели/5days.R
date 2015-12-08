
# Производим тестирование на влияние дня недели, выбираемого для формирования недельных цен закрытия, на перфоманс стратегий  
# 
# Всего пять файлов, в каждом соответствующие дневные цены закрытия. Из них пересчитываются недельные цены закрытия.  
# Далее для каждого файла с недельными ценами закрытия  проводится процедура формирования портфелей и выбора лучшей стратегии. 
# Собираем все результаты в один отчет
#
rm(list=ls())
library(XLConnect)
library(pander)
library(DT)
panderOptions('table.split.table', Inf) 
library(ggplot2)
library(scales)
source("R/reality_func2.R")

#############################################################################
# Параметры, которые зависят от изучаемой страны
country_name <- "Индия"
country_name_eng <- "India"
#
#temp <- ret(4, 1, 4, STEP, N, price_d1, UP1, UP2, 0.3) 
#T <- 164    
T <- 456

#############################################################################
# Загрузка 
setwd("/home/nazarov/02-fmlab.hse.ru/02 - независимость от дня недели/")
price_d1<- readWorksheet(loadWorkbook(paste(country_name,"/Monday_price.xlsx",sep="")),sheet=1)
price_d2<- readWorksheet(loadWorkbook(paste(country_name,"/Tuesday_price.xlsx",sep="")),sheet=1)
price_d3<- readWorksheet(loadWorkbook(paste(country_name,"/Wednesday_price.xlsx",sep="")),sheet=1)
price_d4<- readWorksheet(loadWorkbook(paste(country_name,"/Thursday_price.xlsx",sep="")),sheet=1)
price_d5<- readWorksheet(loadWorkbook(paste(country_name,"/Friday_price.xlsx",sep="")),sheet=1)

row.names(price_d1) <- price_d1[,1]
price_d1 <-price_d1[,-1]

row.names(price_d2) <- price_d2[,1]
price_d2 <-price_d2[,-1]

row.names(price_d3) <- price_d3[,1]
price_d3 <-price_d3[,-1]

row.names(price_d4) <- price_d4[,1]
price_d4 <-price_d4[,-1]

row.names(price_d5) <- price_d5[,1]
price_d5 <-price_d5[,-1]

price_data <- list (price_d1,price_d2,price_d3,price_d4,price_d5)

#############################################################################
# Константы
# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 12 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=12
UP2=8
UP3=12
# N - с учетом отступа
N <- (nrow(price_d1)-(2+UP3*4))%/%STEP 
#n <- T-R+1
#############################################################################
# Процедура P&R stationary bootstrap
# Фиксация рандома
set.seed(42)
# Константы для reality check
R <- 1
N_rc <- 500
Q <- 0.1  
P_R_ind <- data.frame(P_R(R,T,Q))
for (i in 2:N_rc){
  P_R_ind[,i]  <-  P_R(R,T,Q)
}
#############################################################################
# Инициализация
V_star <- c()
f_bar  <- c() 
V_star_sharp <- c()
f_sharp  <- c() 
list_of_restable <- list() # для сохранения результатов по каждому дню
#############################################################################
# Процедура формирования портфелей, подсчета статистик и bootstrap
start_time <- Sys.time()
for(day_of_week in 1:5 )
{
  resultDataFull <- price_data[[day_of_week]]
  m <- 1
  realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
  colnames(realityCheckData) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")
  for (percent in c(0.5,0.3,0.2,0.1) ){ 
    for (p1 in 1:UP1 ){
      for (p2 in 0:UP2 ){   
        for (p3 in 1:UP3 ){  
          #вектор дельт    
          temp <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
          return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
          return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
          n <- length(temp)
          #realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
          #                              mean(return.winner), mean(return.loser))    
          #cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, mean(return.winner), mean(return.loser), "\n")
          
          realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                                        mean(return.winner), mean(return.loser),length(temp[temp<0]))    
          cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, mean(return.winner), mean(return.loser),length(temp[temp<0]), "\n")
          
          
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
          #############################################################REALITY_CHECK - SHARP##################################################################      
          f_sharp <- mean(temp)/sd(temp)
          
          if(m==1){
            V_bar_sharp <- sqrt(n)*f_sharp
            for (k in 1:N_rc){
              #     V_star[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
              V_star_sharp[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]])/sd(temp[P_R_ind[,k]]) - f_sharp)
            }         
          }
          else {
            V_bar_sharp <- max(sqrt(n)* f_sharp ,V_bar_sharp)         
            for (k in 1:N_rc){
              #      t <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
              t <- sqrt(n)*(mean(temp[P_R_ind[,k]])/sd(temp[P_R_ind[,k]]) - f_sharp)
              V_star_sharp[k] <-max(t,V_star_sharp[k]) 
            }
          }
          
          retBoot.sharp <- c(V_star_sharp,V_bar_sharp)
          realityCheckData[m, "pBoot-Sharp"] <- 1-(rank(retBoot.sharp, ties.method = "first")[501]-1)/500
          #  list(V_bar,V_star)            
          ####################################################################################################################################################  
          
          m <- m+1      
        }
      }
    }
  }
  
  #Сохранение результатов
  list_of_restable[day_of_week] <- list(data=realityCheckData, num=N, V_bar=V_bar,V_star=V_star, V_bar_sharp=V_bar_sharp,V_star_sharp=V_star_sharp, day=day_of_week)  # список ценных объектов
  saveRDS(file = paste("results/",country_name_eng,"_potfolio_day_",day_of_week ,".RDS",sep=""),list_of_restable[day_of_week]) # сохраняем всё ценное в файл
  #mylist <- readRDS("tz_india_v1.RDS") # читаем из файла что там есть 
  
}

#print(sprintf("%s Start", Sys.time()))
end_time <- Sys.time()

#############################################################################
# Подсчет необходимых статистик



#############################################################################
# Генерация html кода и рассылка на почту

knit(input="R/2html_5days.rmd", output="2html_5days.md", encoding='UTF-8')
markdownToHTML("2html_5days.md","results/Календарные_эффекты.html", stylesheet="view/custom.css")
file.remove("2html_5days.md")
#print(sprintf("%s End", Sys.time()))


