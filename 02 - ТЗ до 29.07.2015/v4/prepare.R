


###########################################################################################
#1) Первая часть — очистка рабочего пространства, задание раб директории 
setwd("/home/nazarov/Рабочий стол/fmlab.hse.ru/ТЗ до 29.07.2015/v4/")
#setwd("J:/12 - ЛАФР/02 - Декомпозиция")
#clear working environment
rm(list=ls()) 

###########################################################################################
#2) Вторая часть -  подготовка данных для анализа

library(XLConnect)

# Загружаем ранее сохраненные цены закрытия, раюботаем с недельными данными, поэтому второй лист
#dataStocksRaw <- readWorksheet(loadWorkbook("/home/nazarov/Рабочий стол/fmlab.hse.ru/эмитенты КОТИРОВКИ.xlsx"),sheet=2, endCol = 165)
dataStocksRaw <- readWorksheet(loadWorkbook("weekly database.xlsx"),sheet=1)



as.Date(as.character(dataStocksRaw[,1]),format="%Y%m%d")

as.character(dataStocksRaw[,1])




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


i<-2
j<-2
while(i <length(colnames(dataStocks))){
  colnames(resultData)[j]  <-colnames(dataStocks)[i] 
  j<-j+1
  i<-i+2
}


a<-dataStocks[,66]




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
