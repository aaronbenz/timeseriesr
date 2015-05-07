#Given a time_date element and a value element, find the area using left riemann sum
#if return_as_vector = True, returns total over entire area by each row
# if False, returns array of total of each row
#if diffTime = TRUE, will treat time_date as if it is diff time_date, so it does have to take it
#' Calculate Area Under the Curve
#' 
#' @description This function takes two vectors, a time-date vector and a numeric vector and performs computations to measure to total area
#' under the curve. Its apprach is to use Reimann Left Sums is and aimed towards time-series data
#' 
#' @param time_date A numeric or date.time vector
#' @param value A numeric vector
#' @param unit_conversion The ratio of your variable to time. Be cautious of this unless inaccurate results appease you :)
#' @param as_vector Boolean describing if return value should be the totaled sum or a vector representing area at each `time` value
#' @param diff_time Boolean describing if `time` is the difference in area, or if it needs to be calculated
#' @param neg_area Boolean describing if negative area should be measured or it all area is positive
#' @param na.replace Function that replaces NA values from `value` with Last Observation Carried Forward. See \code{\link{vreplace}}
#
#' @return The total area under the curve of `value` with respect to the time measured at `time`
#' 
#' @examples
#'  time <- 1:10
#'  value <- sin(1:10)
#'  
#'  calc_area(time, value)
#'  calc_area(time, value, TRUE)
#'  calc_area(time, value, diff_time = TRUE)
#'  calc_area(time, value, neg_area = TRUE)
#'  calc_area(time, value, TRUE, TRUE)
calc_area = function(time_date, value, unit_conversion = 1, as_vector = FALSE, diff_time=FALSE, neg_area = FALSE, na.replace = vreplace, ...){
    #excepting time_date and value to be numeric vectors of the same length
    stopifnot(length(time_date) == length(value),
              is.numeric(time_date), is.numeric(value),
              is.logical(as_vector), is.logical(diff_time), is.logical(neg_area))
    if(sum(is.na(time_date))>0) warning("time_date contains NA value(s). Results may not be as expected")
    
    #replaces NA values in <value> with LOCF by default
    if(!exists("first")) first <- 0
    value <- na.replace(value, ...) 
    
    #if all area should be treated as positive, change all values to positive
    if(!neg_area) value <- abs(value) 
    #if wanting total sum, do matrix calculation
    if(as_vector==FALSE) {
        if(diff_time == FALSE) return((c(diff(time_date),0) %*% value)[1,1] * unit_conversion)
        return((time_date %*% value)[1,1] * unit_conversion)
    }
    #if want an area vector returned, do vector calculation
    else{
        if(diff_time == FALSE) return(c(diff(time_date),0) * value * unit_conversion)
        return(time_date * value * unit_conversion)
    }
}

#' Create a group of new_times endpoints for all groups to merge into existing dataset
#' @description This function takes a set of new_times and a data.frame/data.table, and returns a data.frame that creates a time
#' for every unique group specified by `group_by` from `dt`
#' @param dt A data.frame/data.table
#' @param new_times A vector of new_times to be added
#' @param time_col The name or index of the time_col column
#' @param group_by An index or string of the columns that serve as groups
#' @examples
#' dt <- data.table("times" = seq(1,5,length.out = 10), value = letters[1:10], grouper1 = c(rep("a",5), rep("b",5)))
#' prep_ts(dt,c(3.2,4.4),"times","grouper1")
prep_ts <- function (dt, new_times, time_col = "time", group_by = NULL){
  #if new_times is numeric, change to its proper string
  if(is.numeric(time_col)){
    if(time_col > length(dt) | time_col <= 0) stop(paste("Gave time_col index as",time_col,", but dt is only",length(dt),"long"))
    time_col = names(dt)[time_col]
  } 
  if(!time_col %in% names(dt)) stop(paste(time_col,"is not in dt"))
  if(is.numeric(group_by)){
    if(sum(group_by > length(dt)) != length(group_by)) stop("group_by fields are out of bounds")
  }else{
    if(!is.null(group_by)) group_by = str_i(names(dt),group_by)
  }
  #quick checks
  stopifnot(!is.na(group_by),
            is.data.frame(dt))
  
  
  if(!is.null(group_by)){ #if groups
    groups <- dt %>%
      dplyr::select(group_by) %>%
      dplyr::distinct()
    added_ts <- expand.grid.df(data.frame(groups), data.frame(new_times))
  }else added_ts  <- data.frame(new_times)
  setnames(added_ts, length(added_ts), time_col) #set name of new_times column to match original in dt
  return(added_ts)
}




#' Add times to an existing data.frame/data.table and have the values filled in by LOCF
#' @description This function takes a set of times and a data.frame/data.table, and returns a data.table that contains
#' the original `dt` plus the additional times with actual values. This is extremely helpful for calculating over different
#' intervals of time.
#' @inheritParams prep_ts
#' @details
#' This function utilizes the power of the data.table package, specifically is "roll" functionality. See \code{\link[data.table]{data.table}}
#' For example, if you wish to roll both ends, add rollends = TRUE as a parameter
#' @examples
#' dt <- data.table("times" = seq(1,5,length.out = 10), value = letters[1:10], grouper1 = c(rep("a",5), rep("b",5)))
#' new_times <- c(3.2,4.4)
#' time_col = "times"
#' group_by = "grouper1"
roll_on <- function(dt,new_times, time_col = "time", group_by = NULL,...){
  if(!is.data.table(dt)) setDT(dt)
  tmp_ts <- prep_ts(dt = dt,new_times = new_times,time_col = time_col,group_by = group_by) #get times with groups
  if(!is.data.table(tmp_ts)) setDT(tmp_ts)
  
  #for using a rolling join in data.table, it is important that the roll index column (aka time in this case) is last
  #the key should be only columns that are in both
  setkeyv(dt, c(group_by, time_col))
  setkeyv(tmp_ts, c(group_by, time_col))

  tmp_dt <- dt[tmp_ts,roll=TRUE,...]
  merge(dt,tmp_dt,all=T,by=names(dt))
}

#' Shorthand for getting indexes of a longer string
#' @description Shorthand function for getting indexes of a shorter string on a longer string. Can use to get indexes
#' of the names of a data.frame/data.table
#' @param str_lng A Character Vector
#' @param str_shrt A Character Vector
#' @examples
#' str_i(letters, c('f','g','y'))
str_i <- function(str_lng, str_shrt){
  stopifnot(is.character(str_lng),
            is.character(str_shrt))
  
  idx <- str_lng %in% str_shrt
  if(sum(idx) != length(str_shrt)) stop("Missing names in str_lng")
  
  which(idx)
}

#return area by time_date stamps
#' Calculates the area under a curve between sets of intervals
#' @description This function takes a data.frame/data.table, and finds the area under the curve of each interval given. 
#' @inheritParams prep_ts
#' @param beg_times A vector of times in unix time
#' @param end_times A vector of times in unix time
#' @param diff_time A boolean determining if the givin time column is the actual timestamp, or the difference in time
#' @details
#' This function enables the area of a curve to be found at multiple intervals. By default, it is using roll_ends = true. So, if there
#' is a start or end time that is before or after the actual endpoints, the area will be greater/less than what you might expect. Modifications
#' to allow this to be more robust will be made in the future. 
#' @examples
#' dt <- data.table(time = 101:110, value = 1:10, group = c('a','b'))
#' area_intervals(dt, c(101.5, 105, 109), c(110,110,110), group_by = "group") 
area_intervals = function(df, beg_times, end_times, time_col = "time", value_col = "value", group_by = NULL, diff_time = FALSE){  
  if(length(beg_times) != length(end_times)) stop("beg_times and end_times are not the same length")
  dt <- data.table(df)
  if(!is.numeric(dt[[value_col]])) stop(paste0("The column '",value_col,"' must be numeric"))
  
  #add beg and end times to dt to get true area
  rbindlist(list(dt, roll_on(dt = dt,new_times = unique(beg_times, end_times),time_col = time_col,group_by = group_by)))
  setkeyv(dt, c(group_by, time_col))
  dt <- unique(dt,by = c(group_by, time_col))
  
  #set time_col as 'time_date' for easy access and writing with data.table syntax
  setnames(dt, time_col, "time_date")
  #make sure the data.table is ordered correctly, and then exclude time_col for faster calcs later
  setkeyv(dt, c(group_by, "time_date"))
  setkeyv(dt, group_by)
  
  dt[,diff_time := c(diff(time_date),0),by=group_by]
  
  dt[,area_block:= diff_time * dt[[value_col]]]
  out_list <- list()
  for(i in seq_along(beg_times)){
    out_list[[i]] <- data.table(beg_times = beg_times[i], end_times = end_times[i],
                                dt[time_date >= beg_times[i] & time_date <= end_times[i], #every interval of time
                                   list("area" = sum(area_block)), by = group_by])
  }
  return(rbindlist(out_list))
}



#returns min and max time_date of a datatable with column 'time_date'
calc_time_range = function(dt, col_name = "time_date") {
    return(paste(c(as.POSIXct(min(dt[[col_name]]),origin = "1970-01-01")),' - ',as.POSIXct(max(dt[[col_name]]),origin = "1970-01-01")))
}


