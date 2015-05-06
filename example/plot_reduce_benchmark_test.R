#cpp reduce points benchmark examples

library(Rcpp)
library(microbenchmark)
library(data.table)
library(ggplot2)
sourceCpp("~/Google Drive/R/cpp_r/plot_reduce.cpp")

microbenchmark(reducePointsR(vec, .3), reducePoints(vec, .3), times = 10)

#equivalent reduce points function using cpp stuff
manipulate_reduce_points_cpp <- function(dt, column, tolerance = .01, ranges = FALSE, groups = detectCores()){
#     if(ranges){
#         ranges_lst <- manipulate_split_dt(tester, groups = groups, ranges = T)
#         mclapply(dt, reducePoints2, column = column, range = )   
#     }
#     array = as.numeric(dt[[column]])
    return(dt[reducePoints2(dt[[column]], tolerance)$index]) 
}

#test the two functions out with simple growing value containing 1 million values
tester <- data.table("index" = 1:10000000, "a" = 1:10000000)
test1 <- microbenchmark(manipulate_reduce_points(tester, "a", .01), manipulate_reduce_points_cpp(tester, "a", .01))


#random data around a sin model
tester_sin <- data.table("index" = 1:2000000, "variable" = sin(seq(0,20,.00001)))
test2 <- microbenchmark(manipulate_reduce_points(tester_sin, column = "index", tolerance = .01), manipulate_reduce_points_cpp(tester_sin, column = "index", tolerance = .01))
microbenchmark( manipulate_reduce_points_cpp(tester_sin, column = "index", tolerance = .01))

# Unit: milliseconds
# expr        min         lq
# manipulate_reduce_points(tester_sin, column = "index", tolerance = 0.01) 2993.59233 3097.06565
# manipulate_reduce_points_cpp(tester_sin, column = "index", tolerance = 0.01)   71.39483   74.59132
# mean     median         uq       max neval
# 3166.29895 3143.53254 3194.33070 3546.6098   100
# 90.05598   80.88903   87.31548  161.3584   100


#complete test to save a plot
test3 <- microbenchmark(ggsave(plot = ggplot(manipulate_reduce_points(tester_sin, column = "index", tolerance = .01), aes(x = index, y = variable)) + geom_step(), filename = "test.png"))


reduce_r <- manipulate_reduce_points(tester_sin, column = "variable", tolerance = .01)
reduce_cpp <- 
    manipulate_reduce_points_cpp(tester_sin, column = "variable", tolerance = .01)
ggplot()
g1 <- ggplot(reduce_r, aes(x = index, y = variable)) + geom_step()
g2 <- manipulate_reduce_points_cpp()
g






library(parallel)
library(plyr)
#function to split a data.table into groups groups
manipulate_split_dt <- function(dt, groups=1, ranges = FALSE){
    if(groups<1) stop("groups must be 1 or greater")
    
    nrow_dt <- nrow(dt)
    size_split <- round(nrow_dt/groups)
    lst <- list()
    
    #if ranges=TRUE, just return ranges instead of actual data
    if(ranges){
        for(i in 0:(groups-1)){
            lst[[i+1]] <- c((i*size_split + 1), min((i+1) * size_split, nrow_dt))
        }
        return(lst)

    }
    
    #seperate groups into equal split sizes. only the last one may be different
    for(i in 0:(groups-1)){
        lst[[i+1]] <- dt[(i*size_split + 1): min((i+1) * size_split, nrow_dt)]
    }
    return(lst)
}




tester_sin_lst <- manipulate_split_dt(tester_sin, groups = detectCores())
tester_sin_lst <- manipulate_split_dt(tester_sin, groups = 2)



mclapply(tester_sin_lst, manipulate_reduce_points_cpp, column = "variable")
ddply(tester_sin, "")
n <- 3
split(tester_sin, factor(sort(rank(row.names(tester_sin))%%n)))

tester_sin[,variable2 := (variable * 100)]
microbenchmark(reducePoints2(tester_sin$variable2, .01))
microbenchmark(reducePoints(tester_sin$variable2, .01))
reducePoints2(tester_sin$variable2, .01)
reducePoints2(tester_sin$variable, .01)
reducePoints2(tester_sin$index, .01)


plot(x$index, x$values)


#shows bug
cppFunction('bool evalC(double x, double y) {
            return ((x-y)<0);
            }')

evalC(.002, .004)

#parallel test1
tester <- data.table("index" = 1:10000000, "a" = 1:10000000)
test1_par <- microbenchmark(manipulate_reduce_points(tester, "a", .01), manipulate_reduce_points_cpp(tester, "a", .01))
ranges <- manipulate_split_dt(tester, groups = detectCores(), ranges = T)



