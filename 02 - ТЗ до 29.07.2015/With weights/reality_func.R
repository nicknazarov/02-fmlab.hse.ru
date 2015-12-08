
#w<-c(1,-1)
#x<- c(1,2)
#w*x
#weighted.mean(x, w)

#vect.weights <- c(7,6,5,3,2,1,-1,-2,-3,-5,-6,-7)

ret.weight <- function (p1, p2, p3, STEP, N, d, UP1, UP2, vect.weights)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    length.vect.weights <- length(vect.weights)
    
    ans[m] <-sum(temp3[c( 1:(length.vect.weights/2) , (length(temp3)-length.vect.weights/2+1):length(temp3) ) ] *vect.weights) /p3
    
    #print(ans[m] )
    
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}


###########################################################################################
ret <- function (p1, p2, p3, STEP, N, d, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    
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

ret.winner <- function (p1, p2, p3, STEP, N, d, UP1, UP2, pos.vect.weights)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    length.vect.weights <- length(pos.vect.weights) 
    
    ans[m] <-sum(temp3[1:length.vect.weights],pos.vect.weights) /p3
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}


###########################################################################################

ret.loser <- function (p1, p2, p3, STEP, N, d, UP1, UP2, neg.vect.weights)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    
    temp2 <- d[,order(-temp1)]
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    length.vect.weights <- length(neg.vect.weights) 
    
    ans[m] <-sum( temp3[ (length(temp3)-length.vect.weights+1): length(temp3) ] ,neg.vect.weights) /p3
    
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
    ans[m]  <- (as.numeric(d[i+p3*4])- as.numeric(d[i]))/as.numeric(d[i])/p3        
    names(ans)[m] <- row.names(d)[i]    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}




ret.companies <- function (p1, p2, p3, STEP, N, d, UP1, UP2, percent)
{
  
  companies <- data.frame (1,1,1)
  colnames(companies) <-c("Ticket","In_winners", "In_losers")
  companies[,1] <- colnames(d)
  companies[,2] <- rep(0,ncol(d))
  companies[,3] <- rep(0,ncol(d))
  # набор дельт  - ans
  ans <- c() 
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
  return(ans)
}



