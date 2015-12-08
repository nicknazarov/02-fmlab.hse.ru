
# Производим тестирование на влияние дня недели, выбираемого для формирования недельных цен закрытия, на перфоманс стратегий  
# 
# Всего пять файлов, в каждом соответствующие дневные цены закрытия. Из них пересчитываются недельные цены закрытия.  
# Далее для каждого файла с недельными ценами закрытия  проводится процедура формирования портфелей и выбора лучшей стратегии. 
# Собираем все результаты в один отчет
#
setwd("/home/nazarov/02-fmlab.hse.ru/02 - независимость от дня недели/")
rm(list=ls())
library(XLConnect)
library(pander)
library(DT)
panderOptions('table.split.table', Inf) 
library(ggplot2)
library(scales)
library(knitr)
source("R/reality_func2.R")
library(markdown)


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


#############################################################################
# Генерация html кода и рассылка на почту
resultDataFull <- price_d1

list_of_restable <-list(readRDS("results/India_potfolio_day_1.RDS"),readRDS("results/India_potfolio_day_2.RDS"))  # читаем из файла что там есть
list_of_restable <- unlist(list_of_restable, recursive=FALSE) 
knit(input="2html_5days_short.rmd", output="2html_5days_short.md", encoding='UTF-8')
#markdownToHTML("2html_5days_short.md","results/Календарные_эффекты_.html", stylesheet="view/custom.css")
markdownToHTML("2html_5days_short.md","results/Календарные_эффекты(2 дня).html")
file.remove("2html_5days_short.md")
#print(sprintf("%s End", Sys.time()))

