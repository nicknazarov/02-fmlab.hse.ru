


###########################################################################################
#1) Первая часть — очистка рабочего пространства, задание раб директории 
setwd("/home/nazarov/02-fmlab.hse.ru/ТЗ до 29.07.2015/With zero/data/")
#setwd("J:/12 - ЛАФР/02 - Декомпозиция")
#clear working environment
rm(list=ls()) 

###########################################################################################
#2) Вторая часть -  подготовка данных для анализа

# Загружаем ранее сохраненные цены закрытия, работаем с недельными данными, поэтому второй лист
#dataStocksRaw <- readWorksheet(loadWorkbook("/home/nazarov/Рабочий стол/fmlab.hse.ru/эмитенты КОТИРОВКИ.xlsx"),sheet=2, endCol = 165)

dataStocksRaw <- read.csv(file="brazil_conv 66.csv", header=TRUE, sep=";")



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

cleanData <-c()
m<-1
for(k in 1: (length(colnames(dataStocks))/2) ) {
  cleanData[k] <-m
  m <- m+2
}


#cleanData [66]
#dataStocks[133]
cleanData [67]<-133
cleanData <- cleanData [-1]

#Удаляем лишние столбцы
#rownames(dataStocks) <- dataStocks[,1]
dataStocks <- dataStocks[,-cleanData]




###########################################################################################
#3) Третья часть -  сохраняем итоговую таблицу в excel документ

wb <- loadWorkbook("stocks_india.xlsx", create = TRUE)
createSheet(wb, "stocks")
writeWorksheet(wb, dataStocks , "stocks",)
saveWorkbook(wb)


###########################################################################################

# Проверка на наличие нулей по середине столбца
flag<-0
for(k in 1:ncol(dataStocks)){
  
  for(m in 2:(nrow(dataStocks)-1)){
    
    if(   (dataStocks[m,k]!=0 & (dataStocks[m+1,k]==0))  )
      {
      flag<-flag+1
      }
  }
 if(flag>=1) { print(k) }
  flag<-0
}



dataStocks[,200]
