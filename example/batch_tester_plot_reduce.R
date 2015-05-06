#Aaron Benz
#11-07-2014
#batch tester script-- full end to end results of reduce points

#cpp reduce points benchmark examples

library(Rcpp)
library(microbenchmark)
library(data.table)
library(ggplot2)
sourceCpp("~/Google Drive/R/cpp_r/plot_reduce.cpp")

#equivalent reduce points function using cpp stuff
manipulate_reduce_points_cpp <- function(dt, column, tolerance = .01){
  return(dt[reducePoints(dt[[column]], tolerance)$index]) 
}
manipulate_reduce_points = function(dt, column, tolerance = .01){
  array = as.numeric(dt[[column]])
  
  #creates the realitive tolerance for the specified column
  tolerance_range = (max(array) - min(array)) * tolerance
  
  
  currentValue = array[1]
  rowKeep = c(1)
  rowKeep_last = length(array)
  i=2
  while(i < length(array)){
    #increase= ifelse(length(array)-i>segments,segments,length(array)-i)
    temp = which(abs(array[i:length(array)]-currentValue)>tolerance_range)[1]
    if(is.na(temp)){
      i = length(array) + 1
    }else{
      i = i + temp - 1
      rowKeep[length(rowKeep)+1] = i
      currentValue = array[i]
    }
  }
  return(dt[c(rowKeep,rowKeep_last)]) 
}

tester <- data.table("index" = 1:100000, "linear" = 1:100000)
lst_results <- list()

#function to simulate different linear combinations
test_linear <- function(sizes, tolerance, times = 5){
  pos <- data.table(expand.grid(sizes, tolerance, times)) #get all possibilities
  setnames(pos, c("sizes","tolerance","times"))
  
  func <- function(size, tol, times){
    print(paste0("Test:linear, Size: ", size, ", tol: ",tol, ", times: ", times, ", Time: ", Sys.time()))
    dt_test <- data.table("index" = 1:size, "linear" = 1:size)
    
    #do the benchmark test
    tmp <- microbenchmark(manipulate_reduce_points(dt_test,"linear", tol),
                          manipulate_reduce_points_cpp(dt_test, "linear",tol),
                          times = 10)
    
    #store name of test in as an attribute
    attr(tmp, "test") <- paste0("linear_",size,"_",tol)
    return(list(tmp))
  }
  lst <- mapply(func,pos$sizes, pos$tolerance, pos$times)
  return(lst)
}

test_sin <- function(sizes, tolerance, times = 5){
  pos <- data.table(expand.grid(sizes, tolerance, times)) #get all possibilites
  setnames(pos, c("sizes","tolerance","times"))
  
  func <- function(size, tol, times){
    print(paste0("Test:sin, Size: ", size, ", tol: ",tol, ", times: ", times, ", Time: ", Sys.time()))
    
    #create sin curve
    dt_test <- data.table("index" = 1:size, "sin" = sin(seq(0,20,10/size*2))[-1])
    
    #do benchmark test
    tmp <- microbenchmark(manipulate_reduce_points(dt_test,"sin", tol),
                          manipulate_reduce_points_cpp(dt_test, "sin",tol),
                          times = 10)
    attr(tmp, "test") <- paste0("sin_",size,"_",tol)
    return(list(tmp))
  }
  lst <- mapply(func,pos$sizes, pos$tolerance, pos$times)
  return(lst)
}
#10 million
#-----------------------------------------------------
sizes = c(50000,100000,500000,1000000)
tolerance = c(.01, .05, .1, .5)
times = 2
results_linear <- test_linear(sizes, tolerance, times)
save(results_linear, file = "results_linear.RDA")

results_sin <- test_sin(sizes, tolerance, times)
save(results_sin, file = "results_sin.RDA")

size = 1000000
tester <- data.table("index" = 1:size, "sin" = sin(seq(0,20,10/size*2))[-1])
microbenchmark(manipulate_reduce_points(tester,"sin", .01),
               manipulate_reduce_points_cpp(tester, "sin",.01), times = 10)
