
p1<-1
p2<-2
p3<-1
percent<-0.3


  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (d[i-p2,] - d[i-4*p1-p2,])/d[i-4*p1-p2,]
    
    temp2 <- d[,order(-temp1[1,])]
    
    temp3 <- (temp2[i+p3*4,]- temp2[i,])/temp2[i,]
    print(i)
    if(percent==0.5){
      ans[m] <- (sum(temp3[,1:floor(ncol(temp3)*percent)] )/ floor(ncol(temp3)*percent)- 
                   sum(temp3[,ceiling(ncol(temp3)*percent):(ncol(temp3))]) /(ncol(temp3)-floor(ncol(temp3)*percent)))/p3
    }
    else{
      ans[m] <- (sum(temp3[,1:floor(ncol(temp3)*percent)])/floor(ncol(temp3)*percent)- 
                   sum(temp3[,ceiling(ncol(temp3)*(1-percent)):(ncol(temp3))]) /floor(ncol(temp3)*percent))/p3
    }
    
    
    
    m <- m+1
    i<-STEP+i  
  }   

