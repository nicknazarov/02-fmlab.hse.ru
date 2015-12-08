


setwd("/home/nazarov/Рабочий стол/fmlab.hse.ru")

rm(list=ls()) 



library(XLConnect)

d <- readWorksheet(loadWorkbook("test.xlsx"),sheet=1 )



ret <- function (p1, p2, p3, STEP, N, d,  percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-4
  m <- 1 
  while(i <= N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-p1-p2,]))/as.numeric(d[i-p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ans[m] <- (sum(temp3[1:2] )/ 2- 
                   sum(temp3[4:5]) /2)/p3
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
floor(length(temp3)*percent)
r<-ret(3, 0, 2, 1, 10, d, 0.5)
r
