

###########################################################################################
P_R <- function (R,T,q){
  
  t=R
  Tetta <- c()
  Tetta[t-R+1] <- sample(R:T, 1)
  
  while(1){
    t <- t+1
    if(t>T) break
    U <- runif(1, 0, 1)
    if(U<q) {
      Tetta[t-R+1] <- sample(R:T, 1)
    }
    
    if(U>=q) {
      Tetta[t-R+1] <- Tetta[t-R]+1  
      
      if(Tetta[t-R+1]>T) {
        Tetta[t-R+1] <- R
      } 
    }
  } 
  
  Tetta   
  
}



###########################################################################################

ret.winner <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{

  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0]
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ans[m] <- (sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent))/p3
    }
    else{
      ans[m] <- (sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent))/p3
      
      #print("########################")
      #print(length(temp3) - ceiling(length(temp3)*(1-percent))+1  )
      #print(ceiling(length(temp3)*percent))   
      #print("########################")
      
    }
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}


###########################################################################################

ret.loser <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ans[m] <- (sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent)))/p3
    }
    else{
      ans[m] <- (sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent))/p3
      
      #print("########################")
      #print(length(temp3) - ceiling(length(temp3)*(1-percent))+1  )
      #print(ceiling(length(temp3)*percent))   
      #print("########################")
      
    }
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}

ret.mmvb <- function (p1, p2, p3, STEP, N, d, UP1, UP2)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    ans[m]  <- (as.numeric(d[i+p3*4])- as.numeric(d[i]))/as.numeric(d[i])        
    names(ans)[m] <- row.names(d)[i]    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}



######################################################################################################

ret <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2, ]))/as.numeric(d[i-4*p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ans[m] <- (sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent)- 
                   sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent)))/p3
    }
    else{
      ans[m] <- (sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent)- 
                   sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent))/p3
      
      #print("########################")
      #print(length(temp3) - ceiling(length(temp3)*(1-percent))+1  )
      #print(ceiling(length(temp3)*percent))   
      #print("########################")
      
    }
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}



