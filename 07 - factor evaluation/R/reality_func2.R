


returnWrapper <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent, flag){
  
  if(flag==1){
    return (ret_1(p1, p2, p3, STEP, N, d2, UP1, UP2, percent))
  }
  
  
}

rankingFactorWrapper <- function(p1, p2, p3, STEP, N, d2, i, flag){
  #Обертка для факторов ранжирования компаний
  if(flag==1){
    return (rankingFactor_1(p1, p2, p3, STEP, N, d2, i))
  }
  
  
}

id_coeff <- function (datatable, p1, p2, i) {
  # Выдает значения коэффициента ID для компаний
  # учитывает разницу положительных и отрицательных недельных доходностей
  #
  # Args:
  #   p1: исторический период
  #   p2: период ожидания
  #   datatable: исходная  таблица с ценами закрытия
  #   i: момент времени
  #
  # Returns:
  #   Вектор  - названия компаний - ранги
  sgn <- sign(as.numeric(datatable[i-p2,]) - as.numeric(datatable[i-4*p1-p2,]))
  
  cat(i, nrow(datatable[(i-4*p1-p2):(i-p2),] ),  nrow(datatable[(i-4*p1-p2-1):(i-p2-1),]))
  
  
  daily_returns <- datatable[(i-4*p1-p2):(i-p2),] - datatable[(i-4*p1-p2-1):(i-p2-1),]
  
  #print(daily_returns)
  signs_daily_returns <- apply(daily_returns,2,sign)
  sum_signs_daily_returns <- apply(signs_daily_returns,2,sum)
  return(sgn*sum_signs_daily_returns)
}


rankingFactor_1 <- function(p1, p2, p3, STEP, N, d2, i){
  # Выдает датафрейм , в котором столбцы расставлены в порядке, определяемым
  # ранжирующей функцией - сначала две группы - победители и проигравшие - по доходности, далее
  # ID - коэффициент, учитывающий разницу положительных и отрицательных недельных доходностей
  #
  # Args:
  #   p1: исторический период
  #   p2: период ожидания
  #   p3: инвестиционный период
  #   STEP:  Шаг для подсчета разниц в группах победителей и проигравших
  #   d2: исходная  таблица с ценами закрытия
  #   i: момент времени
  #
  # Returns:
  #   Исходная таблица со столбцами в определенном порядке data.frame
  
  # Удаляем нулевые столбцы
  d <- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
  temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
  #temp1 <- ( t(d)[,i-p2] - t(d)[,i-4*p1-p2 ]) / t(d)[,i-4*p1-p2]
  temp2 <- d[,order(-temp1)]
  #######################################################################################################   
  # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
  
  for(k in 1:ncol(temp2)){
    if(temp2[i+p3*4, k]==0){
      
      count<-1
      while(temp2[i+p3*4-count,k]==0){
        count<-count+1
      }
      temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
      
    }
  }
  #######################################################################################################  
  # Далее делим на две части, сортируем отдельно первую и вторую по ID
  winners_table <- temp2[,1:ceiling(ncol(temp2)/2)]
  losers_table <- temp2[,(ceiling(ncol(temp2)/2)+1):ncol(temp2)]

  winners_table <- winners_table[ ,order(-id_coeff(winners_table, p1, p2, i))]
  losers_table <-  losers_table [ ,order(id_coeff(losers_table, p1, p2, i))]
  
  temp2 <- cbind(winners_table, losers_table)
  #data.frame(..., check.names = FALSE)
  return (temp2) 
}


ret_1 <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  # набор дельт  - ans, считает через корень доходность
  #средняя месячная доходность =(1+доходность за n месяцев)^(1/n)-1
  ans <- c() 
  i <- UP1*4+2+UP2
  m <- 1 
  while(i < N){
    temp2 <- rankingFactorWrapper(p1, p2, p3, STEP, N, d2, i, 1)
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ret_inv <- sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent)- 
        sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent))
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
     }
    else{
      ret_inv <- sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent)- 
        sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent)
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
      #средняя месячная доходность =(1+доходность за n месяцев)^(1/n)-1
    }
    
    names(ans)[m] <- row.names(d2)[i]    
    
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
ret.winner <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    # temp1 <- (t (d)[,i-p2] - t (d) [,i-4*p1-p2 ])/t (d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:length(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
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
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    #temp1 <- (t (d)[,i-p2] - t (d) [,i-4*p1-p2 ])/t (d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:length(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
    
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
old_ret <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    #temp1 <- ( t(d)[,i-p2] - t(d)[,i-4*p1-p2 ]) / t(d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:ncol(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
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

ret.companies <- function (p1, p2, p3, STEP, N, d, UP1, UP2, percent)
{
  
  companies <- data.frame ( Ticket=rep(0,ncol(d)), In_winners=rep(0,ncol(d)),  In_losers= rep(0,ncol(d)))
  
  companies[,1] <- colnames(d)
  companies[,2] <- rep(0,ncol(d))
  companies[,3] <- rep(0,ncol(d)) 
  
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    comp.order <- 1:ncol(d)
    # comp.order <- comp.order[]
    comp.winners <- rep(0,ncol(d))
    comp.losers <- rep(0,ncol(d))
    
    #temp2 <- d[,order(-temp1)]
    
    #d<- d2[,d2[i-4*p1-p2,]!=0]
    temp1 <- temp1 [d[i-4*p1-p2,]!=0]
    comp.order <- comp.order [d[i-4*p1-p2,]!=0]  
    
    comp.order <- comp.order[order(-temp1)]
    
    if(percent==0.5){   
      comp.winners[comp.order[1:floor(length(temp1)*percent)]] <- 1
      comp.losers[comp.order[(floor(length(temp1)*percent)+1):(length(temp1))]]<- 1              
    }
    else{ 
      comp.winners[comp.order[1:ceiling(length(temp1)*percent)]]<- 1
      comp.losers [comp.order[ceiling(length(temp1)*(1-percent)):(length(temp1))]] <- 1 
    }
    
    companies[,2] <- companies[,2] + comp.winners
    companies[,3] <- companies[,3] + comp.losers
    m <- m+1
    i<-STEP+i  
  }   
  return(companies)
}



###########################################################################################

new_ret.winner <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    # temp1 <- (t (d)[,i-p2] - t (d) [,i-4*p1-p2 ])/t (d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:length(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ret_inv <- sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent)
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
      #ans[m] <- (sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent))/p3
    }
    else{
      ret_inv <- sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent)
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
      #ans[m] <- (sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent))/p3
      
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

new_ret.loser <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    #temp1 <- (t (d)[,i-p2] - t (d) [,i-4*p1-p2 ])/t (d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:length(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ret_inv <- sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent))
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
      # ans[m] <- (sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent)))/p3
    }
    else{
      ret_inv <- sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent)
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
      #ans[m] <- (sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent))/p3
      
      
      
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


new_ret <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    #temp1 <- ( t(d)[,i-p2] - t(d)[,i-4*p1-p2 ]) / t(d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:ncol(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ret_inv <- sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent)- 
        sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent))
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
      # ans[m] <- (sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent)- 
      #              sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent)))/p3
    }
    else{
      ret_inv <- sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent)- 
        sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent)
      ans[m] <- (1 + ret_inv)^(1/p3) - 1
      # ans[m] <- (sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent)- 
      #              sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent))/p3
      
      
      
      
      #средняя месячная доходность =(1+доходность за n месяцев)^(1/n)-1
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



