
###########################################################################################
#1) Первая часть — очистка рабочего пространства, задание раб директории 
setwd("/home/nazarov/02-fmlab.hse.ru/ТЗ до 29.07.2015/With zero/")
#setwd("J:/12 - ЛАФР/02 - Декомпозиция")
#clear working environment
rm(list=ls()) 

###########################################################################################
#2) Вторая часть -  подготовка данных для анализа

library(XLConnect)

# Загружаем ранее сохраненные цены закрытия, раюботаем с недельными данными, поэтому второй лист
#dataStocksRaw <- readWorksheet(loadWorkbook("/home/nazarov/Рабочий стол/fmlab.hse.ru/эмитенты КОТИРОВКИ.xlsx"),sheet=2, endCol = 165)
dataStocksRaw <- readWorksheet(loadWorkbook("weekly database.xlsx"),sheet=1 )

cleanData <-c()
i<-1
k<-3
while(k<ncol(dataStocksRaw)){
  cleanData[i] <-k
  cleanData[i+1] <-k+1
  i<-i+2
  k<-k+4
}

#Удаляем лишние столбцы
dataStocks <- dataStocksRaw[,-cleanData]

#dataStocks[,98]

# переименовываем столбцы
colnames(dataStocks)[1]<-"Date"
i<-2
while(i <length(colnames(dataStocks))){
  
  temp <- dataStocks[ 1, i]
  colnames(dataStocks)[i+1]<-temp
  i<-i+2
}

# удаляем оставшиеся лишние столбцы

cleanData2 <-c()
i<-1
k<-2
while(k<ncol(dataStocks)){
  cleanData2[i] <-k
  i<-i+1
  k<-k+2
}

resultDataFull<- dataStocks[,-cleanData2]

###########################################################################################
#3) Третья часть -  сохраняем итоговую таблицу в excel документ

wb <- loadWorkbook("stocks_russia.xlsx", create = TRUE)
createSheet(wb, "stocks")
writeWorksheet(wb, resultDataFull, "stocks",)
saveWorkbook(wb)


###########################################################################################

d<- d2[,d2[i-4*p1-p2,]!=0]

p1<-2
p2<-4
i<-16
t<-resultDataFull[,resultDataFull[i-4*p1-p2,]!=0]

resultDataFull[i-4*p1-p2,]!=0

