


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







###########################################################################################
#5) Пятая часть -  сохранение результатов на жесткий диск



# сохранение результатов работы
stuff <- list(data=realityCheckData, num=N, V_bar=V_bar,V_star=V_star)  # список ценных объектов
saveRDS(file = "tz_29_v1(Q=0.1).RDS",stuff) # сохраняем всё ценное в файл
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



mylist <- readRDS("tz_29_v1(Q=0.1).RDS") # читаем из файла что там есть 
V_star <- mylist[[4]]
V_bar <- mylist[[3]]
result.data <- mylist[[1]]
sort.table<- result.data[order(-result.data[,1]),] 
V_star <- mylist[[4]]
V_bar <- mylist[[3]]




#############################################################################################################
setwd("/home/nazarov/Рабочий стол/fmlab.hse.ru/ТЗ до 29.07.2015/")
library(ggplot2)
source("reality_func.R")
library(XLConnect)
resultDataFull<- readWorksheet(loadWorkbook("stocks_52.xlsx"),sheet=1)

row.names(resultDataFull) <- resultDataFull[,1]
resultDataFull <-resultDataFull[,-1]
UP1=12
UP2=8
UP3=12
STEP=1
N <- (nrow(resultDataFull)-(2+UP3*4))%/%STEP 
p1 <-11
p2 <- 2
p3 <- 3
perc<- 0.1

winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, perc) 
loser <- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, perc) 
x<-as.Date(names(y1))
data.plot1 <- data.frame(x=x,y1=y1,y2=y2)



#library(knitr)
#knit2html('tz_29.Rmd', options = c('toc', markdown::markdownHTMLOptions(TRUE)))





library(gridExtra)
library(scales)
#library(scales)
g1<- ggplot(data = data.plot1 , aes(x = x, y = y1,group=1))
g2<- ggplot(data = data.plot1 , aes(x = x, y = y2))

g1+geom_line()+ theme(axis.text.x = element_text(angle = 90, hjust =1, size=15,))+ scale_x_date(labels = date_format("%d/%m/%y"),breaks = date_breaks("1 week"))
  

winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, perc) 
loser <- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, perc) 
x<-as.Date(names(y1))
data.plot1 <- data.frame(x=x,y1=y1,y2=y2)


ggplot(data.plot1, aes(x))  +
  geom_line(aes(y = winner, colour = "winner"),size=2) + 
  geom_line(aes(y = loser, colour = "loser"),size=2)+ theme(axis.text.x = element_text(angle = 90, hjust =1,size=15),
  axis.text.y = element_text(size=15,face="bold") ,axis.title=element_text(size=20,face="bold"),plot.title = element_text(color="blue", size=34, face="bold"))+ 
  scale_x_date(labels = date_format("%d/%m/%y"),breaks = date_breaks("1 week"))  + 
  labs(title ="Доходности победителей и проигравших", x = "Даты", y = "Среднемесячная доходность" ) + 
  scale_y_continuous()+   geom_vline(xintercept= as.numeric(c(as.Date("2013-01-01"),as.Date("2014-01-01")) ), linetype=6, colour="black",size=2 )





geom_vline(xintercept=as.numeric(as.Date("2000-01-10")))

+ geom_vline(as.numeric(x[4]))
  
  geom_vline(  aes( xintercept=as.numeric(x[2])    ) , linetype=4, colour="black")



+     layer(geom="line")
+ geom_vline(xintercept=15718)

as.numeric(x)
as.numeric("2013-01-13")
+ theme_bw()






mylist <- readRDS("tz_29_v1(Q=0.1).RDS") # читаем из файла что там есть 
V_star <- mylist[[4]]
V_bar <- mylist[[3]]
result.data <- mylist[[1]]
ret_p <- c(V_star,V_bar)
boot.pvalue <- 1-(rank(ret_p, ties.method = "first")[501]-1)/500
sort.table <- result.data[order(-result.data[,1]),] 
#p1 <- unlist(sort.table[1,4])
#p2 <- unlist(sort.table[1,5])
#p3 <- unlist(sort.table[1,6])
#perc<- unlist(sort.table[1,7])
p1 <-44
p2 <- 2
p3 <- 12
perc<- 0.1
best.strat <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, perc) 
