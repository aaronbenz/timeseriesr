#' Replace Values in a vector/data.frame/data.table
#' 
#' @description Replace NAs or a specific value in a vector/data.frame/data.table
#' 
#' @param x A numeric vector
#' @param replace Value to be replaced; NA by default
#' @param replacement The replacement value, which is Last Observation Carried Forward by default
#' @param first If LOCF, and the first value is `replace`, subsitute this value
#' @return A vector where all values equal to \code{replace} in \code{x} are replaced with \code{replacement}.
#' 
#' @examples 
#' tmp <- c(NA,2,3,NA,5,6)
#' vreplace(tmp)
#' vreplace(tmp, replace = 2)
#' vreplace(tmp, replacement = 4)
#' vreplace(tmp, replace = 3, replacement = 3)
#' vreplace(tmp, first = 1)
vreplace <- function(x, replace = NA, replacement = "LOCF", first = NA){
  #use cases that must deal with individuall
  #replace = NA, replacement = NA
        #1. return same
  #replace = !NA, replacement = !NA
        #1. return same
  #replace = NA, replacement = LOCF
        #1. get first value that is not NA -> store
        #2. apend at very end (need to do this in general as well only for LOCF)
        #2. Do LOCF
  #replace = NA, replacement = !LOCF
        #1. x[is.na(x)] <- replacement
  #replace = !NA, replacement = LOCF
        #1. do LOCF operation
    #will replace NAs with LOCF, if like to be handled differently, do before this function
    #otherwise, will by default replace value with LOCF unless a number or other value is specified
    #replace and replacement are the same
  
    if(is.na(replace) & is.na(replacement)) return(x)
    if(!is.na(replace) & !is.na(replacement) & replace == replacement) return(x)
    
    if(is.na(replace)){
      if(replacement == "LOCF"){
        ind = which(!is.na(x))
      }
      if(replacement != "LOCF"){
        x[is.na(x)] <- replacement
        return(x)
      }
    }else{
      if(replacement == "LOCF") ind = which(x != replace)
      if(replacement != "LOCF"){
        x[x==replace] <- replacement
        return(x)
      }
    }
    #vectorize LOCF operation and return
    if(ind[1] != 1){
      ind <- c(1, ind)
      x[1] <- first 
      if(is.na(first) | (!is.na(replace) & replace == first)){
        warning(paste("The first non NA value in X is the replace value:",replace," and replacement is LOCF. So the beginning of X will have NAs"))
      }
    }
    rep(x[ind], times = diff(c(ind, length(x) + 1) ))
}
    
#     if(is.array(x)) return(replace_func())
#     if(is.data.frame(x)) return(lapply(x, replace_func,replace = NA, replacement = "LOCF", first = FALSE))

# 
#' @describeIn vreplace
dtreplace <- function(x, replace = NA, replacement = "LOCF"){
    rbind.data.frame(lapply(x, vreplace, replace = replace, replacement = replacement))
}

#' @describeIn vdeduplicate
dtdeduplicate <- function(x,key){
  indexes <- vdeduplicate(x[[key]],returnIndex = TRUE)
  x[indexes,]
}

##WANT to add a denormalize function that splits a dt in lists

#' See if value is empty
#' @description This function simply checks to see if an item contains anything. Specifically, it checks
#' if it is null, has rows, or columns.
#' @param x An R object
#' @details This is not meant for classes like lm. Only for plain old everyday boring R objects
#' @examples
#' is.empty(1:5)
#' is.empty(c(1:9,3,3)) 
#' is.empty(NULL)
is.empty <- function (x){
      if (is.data.frame(x)){
        return (is.null(x) || length(x) == 0 || nrow(x) == 0 || ncol(x) == 0)
      }
      return (is.null(x) || length(x) == 0)
}

#' Reduce the amount of points of a data.table/data.frame
#' @description Takes a data.table/data.frame, and reduces the total set by some tolerance percentage.
#' This use case applies particularily to large scale timeseries data, so that it only grabs points of
#' significance.
#' @param dt A data.table/data.frame
#' @param column A column name or index
#' @param tolerance A decimal representation of a percentage
#' @details This function essentially takes the min and max value from the `column` in the dataset. It then
#' calculates the value tolerance (vtol), which is `(max - min) * tolerance`. Then it iterates through the result
#' set, only keeping the next points that are considered outside of the value tolerance with respect to the
#' last known point. That is, given some f(x) where n is the last recorded point,
#' `n+1` must satisfy abs(f(n+1) - n) > vtol
#' @examples
#' df <- data.frame(x = 1:1000, y = sin(seq(0,10,length.out = 1000)))
#' df_reduced <- dtreduce(df, "y", tolerance = .01)
#' plot(df)
#' plot(df_reduced)
#' #compare the difference in the two plots. One is much more dense, while the other is able to depict
#' #nearly the same image with a nearly a fourth of the original data set.
dtreduce <- function(dt, column, tolerance = .01){
  if(!is.data.frame(dt)) stop("dt must be a data.frame/data.table")
  index <- vreduce(dt[[column]], tolerance = tolerance)
  dt <- dt[as.vector(index),]
  if(!is.data.table(dt)) rownames(dt) <- seq_along(dt[[column]])
  dt
}
#This is the old version that is nice to keep around only for comparisson of speed difference
# dtreduce <- function(dt, column, tolerance = .01){
#     array <- as.numeric(dt[[column]])
#     
#     #creates the realitive tolerance for the specified column
#     tolerance_range <- (max(array) - min(array)) * tolerance
#     
#     
#     currentValue <- array[1]
#     rowKeep <- c(1)
#     rowKeep_last <- length(array)
#     i<-2
#     while(i < length(array)){
#         #increase<- ifelse(length(array)-i>segments,segments,length(array)-i)
#         temp <- which(abs(array[i:length(array)]-currentValue)>tolerance_range)[1]
#         if(is.na(temp)){
#             i <- length(array) + 1
#         }else{
#             i <- i + temp - 1
#             rowKeep[length(rowKeep)+1] <- i
#             currentValue <- array[i]
#         }
#     }
#     return(dt[c(rowKeep,rowKeep_last),]) 
# }

#' Retern a vector that is offset by one
#' @description This function takes a vector x from 1 to N , and returns it from 2 to N, and N+1. This mainly used
#' for short hand purposes in other functions
#' @param x A vector
#' @param offset An integer 
#' @param end_value The last value to add on to the vector, since it will be one short of the original `x`. NULL to not add anything on
#' @examples
#' tmp <- 1:10
#' voffset(tmp)
#' voffset(tmp, end_value = 0)
voffset <- function(x, offset = 1, end_value = "Last"){
    stopifnot(is.integer(offset) | is.numeric(offset),
              is.vector(x),
              !is.na(end_value))
    if(end_value == "Last") end_value <- x[length(x)] #special case
    
    if(class(x) != class(end_value) & !is.null(end_value)) warning(paste("end_value class:",class(end_value),"is not the same as x class:",class(x)))
    
    if(length(x)>offset){
      if(is.null(end_value)) return (x[offset])
      return(c(x[(offset+1):length(x)],rep(end_value,offset)))
    } 
    return(c(0))
}

#' Expands a list of data.frames, like expand.grid
#' @param ... Takes a series of data.frames
#' @examples
#' df1 = data.frame(1:10, letters[1:10])
#' df2 = data.frame(101:110)
#' expand.grid.df(df1,df2)
#' expand.grid.df(df1,data.frame(20:22))
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


