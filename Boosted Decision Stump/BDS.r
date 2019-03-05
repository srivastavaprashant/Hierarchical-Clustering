DS <-function(train_X,train_Y){
  n_row <- nrow(train_X)
  Region <- function(s_rm,s_lstat){
    # Variables for finding our RSS for lstat and rm
    sum_lessthan_rm <-0
    sum_grtrthan_rm <-0
    sum_lessthan_lstat <-0
    sum_grtrthan_lstat <-0
    
    n_lessthan_rm <-0
    n_grtrthan_rm <-0
    n_lessthan_lstat <-0
    n_grtrthan_lstat <-0
    
    mean_Y_lessthan_rm <-0
    mean_Y_grtrthan_rm <-0
    mean_Y_lessthan_lstat <-0
    mean_Y_grtrthan_lstat <-0
    
    sum_grtrthan_sqr_rm <-0
    sum_lessthan_sqr_rm <-0
    sum_grtrthan_sqr_lstat <-0
    sum_lessthan_sqr_lstat <-0
    
    
    # Getting Regions for all S
    for(i in 1:n_row){
      if(train_X$rm[i] < s_rm){
        sum_lessthan_rm = sum_lessthan_rm + train_Y[i]
        n_lessthan_rm = n_lessthan_rm + 1
      }
      else{
        sum_grtrthan_rm = sum_grtrthan_rm + train_Y[i]
        n_grtrthan_rm = n_grtrthan_rm + 1
      }
      if(train_X$lstat[i] < s_lstat){
        sum_lessthan_lstat = sum_lessthan_lstat + train_Y[i]
        n_lessthan_lstat = n_lessthan_lstat + 1
      }
      else{
        sum_grtrthan_lstat = sum_grtrthan_lstat + train_Y[i]
        n_grtrthan_lstat = n_grtrthan_lstat + 1
      }
    }
    mean_Y_lessthan_rm = sum_lessthan_rm/n_lessthan_rm
    mean_Y_grtrthan_rm = sum_grtrthan_rm/n_grtrthan_rm
    mean_Y_lessthan_lstat = sum_lessthan_rm/n_lessthan_lstat
    mean_Y_grtrthan_lstat = sum_grtrthan_rm/n_grtrthan_lstat
    
    for(i in 1:n_row){
      if(train_X$rm[i] <s_rm){
        sum_lessthan_sqr_rm <- sum_lessthan_sqr_rm + (train_Y[i] - mean_Y_lessthan_rm)^2
      }
      else{
        sum_grtrthan_sqr_rm <- sum_grtrthan_sqr_rm + (train_Y[i] - mean_Y_grtrthan_rm)^2
      }
      if(train_X$lstat[i] <s_lstat){
        sum_lessthan_sqr_lstat <- sum_lessthan_sqr_lstat + (train_Y[i] - mean_Y_lessthan_lstat)^2
      }
      else{
        sum_grtrthan_sqr_lstat <- sum_grtrthan_sqr_lstat + (train_Y[i] - mean_Y_grtrthan_lstat)^2
      }
    }
    RSS_lstat <- sum_grtrthan_sqr_lstat+sum_lessthan_sqr_lstat
    RSS_rm <- sum_grtrthan_sqr_rm+sum_lessthan_sqr_rm
    return(c(RSS_lstat,RSS_rm))
  }

  Rss_arr_rm <- matrix(nrow = n_row,ncol=1,byrow= TRUE)
  Rss_arr_lstat <- matrix(nrow = n_row,ncol=1,byrow= TRUE)
  for(i in 1:n_row){
    srm <- train_X$rm[i]
    slstat <- train_X$lstat[i]
    output <- Region(srm,slstat)
    #print(output)
    Rss_arr_rm[i] <- output[1]
    Rss_arr_lstat[i] <- output[2]
  }
  returnArray = c()
  sumlessthan <- numlessthan <-numgreaterthan<-sumgreaterthan<-meanYlessthan<-meanYgreatethan <- 0
  if(min(Rss_arr_lstat[1,]) > min(Rss_arr_rm[1,])){
    s_id = which.min(Rss_arr_rm)
    S <- train_X$rm[s_id]
    #caclulating regions R
    for(i in 1:n_row){
      #computing in each possible s
      if (train_X$rm[i] < S)
      {
        sumlessthan = sumlessthan+ train_Y[i]
        numlessthan =numlessthan+1
      }
      else{
        sumgreaterthan = sumgreaterthan+ train_Y[i]
        numgreaterthan = numgreaterthan+1
      }
      
    }
    meanYlessthan <- sumlessthan/numlessthan
    meanYgreatethan <- sumgreaterthan/numgreaterthan
    returnArray =c("rm",S,meanYlessthan,meanYgreatethan)
    return(returnArray)
  }
  else{
    sIndex =which.min(Rss_arr_lstat)
    S<- train_X$lstat[sIndex]
    #caclulating regions R
    for(i in 1:253){
      #computing in each possible s
      if (train_X$lstat[i] < S)
      {
        sumlessthan = sumlessthan+ train_Y[i]
        numlessthan =numlessthan+1
      }
      if(train_X$lstat[i] >S){
        sumgreaterthan = sumgreaterthan+ train_Y[i]
        numgreaterthan = numgreaterthan+1
      }
      
    }
    meanYlessthan <- sumlessthan/numlessthan
    meanYgreatethan <- sumgreaterthan/numgreaterthan
    returnArray= c("lstat",S,meanYlessthan,meanYgreatethan)
    return(returnArray)
    
  }
}


library(MASS)
data("Boston")
set.seed(0911)
n_row = nrow(Boston)
dt <- sample(1:n_row,n_row/2)
train <- Boston[dt,]
test <- Boston[-dt,]
train_Y <- train$medv;

dec_stmp = DS(train,train_Y)
y_hat = matrix(nrow=1, ncol=nrow(test)) 

test_sum_less_sqr <-0
test_sum_grtr_sqr <-0
if(dec_stmp[1]=="lstat"){
  for(i in 1:nrow(test)){
    if(test$lstat[i] <as.numeric(dec_stmp[2])){
      y_hat[i]=as.numeric(dec_stmp[3])
      test_sum_less_sqr = test_sum_less_sqr + (test$medv[i] - as.numeric(dec_stmp[3]))^2    
    }
    else{
      y_hat[i]=as.numeric(dec_stmp[4])
      test_sum_grtr_sqr = test_sum_grtr_sqr + (test$medv[i] - as.numeric(dec_stmp[4]))^2    
    }
  }
} 
if(dec_stmp[1]=="rm"){
  for(i in 1:nrow(test)){
    if(test$rm[i] <as.numeric(dec_stmp[2])){
      y_hat[i]=as.numeric(dec_stmp[3])
      test_sum_less_sqr = test_sum_less_sqr + (test$medv[i] - as.numeric(dec_stmp[3]))^2    
    }
    else{
      y_hat[i]=as.numeric(dec_stmp[4])
      test_sum_grtr_sqr = test_sum_grtr_sqr + (test$medv[i] - as.numeric(dec_stmp[4]))^2    
    }
  }
} 

test_mse <- (test_sum_grtr_sqr + test_sum_less_sqr)/nrow(test)
print(test_mse)


BDS <- function(B){
  DS_B<-matrix(nrow=B,ncol=4,byrow=TRUE)
  f_hat<-matrix(nrow=B,ncol=nrow(train),byrow=TRUE)
  for(i in 1:B){
    DS_B[i,]<-DS(train,train_Y)
    for(j in 1:nrow(train)){
      if(DS_B[i,1]=="lstat"){
        lstat_S <-as.numeric(DS_B[i,2])
        lstat_avg_less <-as.numeric(DS_B[i,3])
        lstat_avg_grtr <- as.numeric(DS_B[i,4])
        if(train$lstat[j] < lstat_S){
          f_hat[i,j] <- lstat_avg_less
          
        }else{
          f_hat[i,j] <- lstat_avg_grtr
        }
        
      }
      else if(DS_B[i,1]=="rm"){
        rm_S <- as.numeric(DS_B[i,2])
        rm_avg_less <-as.numeric(DS_B[i,3])
        rm_avg_grtr <- as.numeric(DS_B[i,4])
        if(train$rm[j] < rm_S){
          f_hat[i,j] <- rm_avg_less
          
        }else{
          f_hat[i,j] <- rm_avg_grtr
        }
        
      }
      ###
      train_Y[j] <- train_Y[j]-0.01*f_hat[i,j]
      
    }
  }
  #MSE
  sqrDiff <- 0
  for(i in 1:nrow(test)){
    predictionRule = sum(0.01*f_hat[,i])
    #print(phat[,i])
    sqrDiff <- sqrDiff+(test$medv[i]- (predictionRule))^2
  }
  print(c("test MSE:" ,sqrDiff/(nrow(test))))
  return(sqrDiff/(nrow(test)))
}

bds_1000=BDS(1000)
