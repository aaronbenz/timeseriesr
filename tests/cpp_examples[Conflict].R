#cpp exploring examples

library(Rcpp)

cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')
# add works like a regular R function
add
add(1, 2, 3)

sourceCpp("Google Drive/R/plot_reduce.cpp")
sourceCpp("plot_reduce.cpp")
a <- c(TRUE,TRUE,TRUE)
all(a)

allC
