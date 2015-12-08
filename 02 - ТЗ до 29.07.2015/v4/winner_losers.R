



library(XLConnect)
setwd("~/Рабочий стол/fmlab.hse.ru/ТЗ до 29.07.2015/v4")
ret.companies <- function (p1, p2, p3, STEP, N, d, UP1, UP2, percent)
{
  
  companies <- data.frame ( Ticket=rep(0,ncol(d)), In_winners=rep(0,ncol(d)),  In_losers= rep(0,ncol(d)))
  
 
  
  #colnames(companies) <-c("Ticket","In_winners", "In_losers")
 # print(colnames(d))
  companies[,1] <- colnames(d)

  
  companies[,2] <- rep(0,ncol(d))
  companies[,3] <- rep(0,ncol(d))

  

  
  
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    comp.order <- 1:ncol(d)
    comp.winners <- rep(0,ncol(d))
    comp.losers <- rep(0,ncol(d))
    
    temp2 <- d[,order(-temp1)]
    comp.order <- comp.order[order(-temp1)]
    
    
    
    # temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      
      comp.winners[comp.order[1:floor(length(temp2)*percent)]] <- 1
      comp.losers[comp.order[(floor(length(temp2)*percent)+1):(length(temp2))]]<- 1          
      
    }
    else{
      
      comp.winners[comp.order[1:ceiling(length(temp2)*percent)]]<- 1
      comp.losers [comp.order[ceiling(length(temp2)*(1-percent)):(length(temp2))]] <- 1
      
      
      #print("########################")
      #print(length(temp3) - ceiling(length(temp3)*(1-percent))+1  )
      #print(ceiling(length(temp3)*percent))   
      #print("########################")
      
    }
    
    
    companies[,2] <- companies[,2] + comp.winners
    companies[,3] <- companies[,3] + comp.losers
    m <- m+1
    i<-STEP+i  
  }   
  return(companies)
}


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

p1 <-11
p2 <-2
p3 <-1
percent <-  0.1 

ans <-ret.companies (p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent)


# Grouped Bar Plot
counts <- table(ans$In_winners, ans$In_losers)
barplot( ans, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


colours <- c("red", "blue")
barplot(as.matrix(t.ans), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)

library(reshape2) 



melt.ans <- melt(ans)
t.ans <- t(ans[order(ans[,2]),])

matrix.ans <- as.matrix(t.ans[-1,])
colnames(matrix.ans) <- substr(t.ans[1,],1,4) 

barplot(matrix.ans , main="Winners - Losers", ylab = "Count", cex.lab = 1.5, cex.main = 1.4,   col=colours,las=2 )




t.ans[-1,]
as.matrix(t.ans)

qplot(data=melt.ans, value, fill=variable )




ggplot(ans, aes(ans[,2], fill=ans[,1]) + geom_bar(position="dodge") + opts(title="Examplary Grouped Barplot")







library(grid)
g.mid<-ggplot(ans,aes(x=1,y=ans$In_winners))+geom_text(aes(label=ans$In_winners))+
  geom_segment(aes(x=0.94,xend=0.96,yend=ans$In_winners))+
  geom_segment(aes(x=1.04,xend=1.065,yend=ans$In_winners))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))


g1 <- ggplot(data = ans, aes(x = ans$In_winners, y = ans$In_losers)) +
  geom_bar(stat = "identity") + ggtitle("Number of sales staff") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() + coord_flip()

g2 <- ggplot(data = ans, aes(x = ans$In_winners, y = ans$In_losers)) +xlab(NULL)+
  geom_bar(stat = "identity") + ggtitle("Sales (x $1000)") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()

library(ggplot2)
library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))


















