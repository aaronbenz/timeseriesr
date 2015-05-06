#cpp exploring examples

library(Rcpp)
library(microbenchmark)
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
# add works like a regular R function
add
add(1, 2, 3)

sourceCpp("~/Google Drive/R/cpp_r/plot_reduce.cpp")
a <- c(TRUE,FALSE,TRUE)
all(a)

allC(a)

microbenchmark(allR(a),allC(a), all(a))
allR <- function(a) ifelse(sum(a)>0, FALSE, TRUE)


num <- c(100:120,120,120,120,121,123, 1:1000000)
head(reducePoints(num, .00009))

microbenchmark(cummin(num),cumminC(num))

library(Rcpp)
sourceCpp("Google Drive/R/plot_reduce.cpp")
reducePoints(num,.1)
scalar_missings()

reducePointsR <- function(vec, tol){
  temp = 1
  tolerance <- (max(vec) - min(vec)) * tol
  counter = 2
  new_vec <- vec[1]
  while(counter < length(vec)){
    if(abs(vec[counter] - new_vec[length(new_vec)]) > tolerance){
    new_vec[length(new_vec) + 1] <- vec[counter]
    }
    counter <- counter + 1
  }
  new_vec
}

microbenchmark(reducePointsR(num, .1), reducePoints(num, .1))
reducePointsR(num, .1)
micr

min_val(c(1:10,-5))
vec <- c(1:5,"NAN",6,7)
min_val(vec)

vec <- sample(1:1000,10000000, replace = T)

microbenchmark(reducePointsR(vec, .3), reducePoints(vec, .3), times = 10)

#equivalent reduce points function using cpp stuff
manipulate_reduce_points_cpp = function(dt, column, tolerance = .01){
    array = as.numeric(dt[[column]])
    return(dt[reducePoints(array, tolerance)]) 
}

#test the two functions out


