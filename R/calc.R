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
#'  examples
#'  calc_area(time, value)
#'  calc_area(time, value, T)
#'  calc_area(time, value, diff_time = T)
#'  calc_area(time, value, neg_area = T)
#'  calc_area(time, value, T, T)
calc_area = function(time_date, value, as_vector = FALSE, diff_time=FALSE, neg_area = FALSE, na.replace = vreplace, ...){
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
        if(diff_time == FALSE) return((c(diff(time_date),0) %*% value)[1,1])
        return((time_date %*% value)[1,1])
    }
    #if want an area vector returned, do vector calculation
    else{
        if(diff_time == FALSE) return(c(diff(time_date),0) * value)
        return(time_date * value)
    }
}

#' Create a group of times endpoints for all groups to merge into existing dataset
#' @description This function takes a set of times and a data.frame/data.table, and returns a data.frame that creates a time
#' for every unique group specified by `group_by` from `dt`
#' @param dt A data.frame/data.table
#' @param times A vector of times to be added
#' @param time The name or index of the time column
#' @param group_by An index or string of the columns that serve as groups
prep_ts <- function (dt, times, time = "time", group_by = NULL){
  #if times is numeric, change to its proper string
  if(is.numeric(time)){
    if(time <= length(times)) stop(paste("There is no time column in dt called:",time))
    time = names(dt)[time]
  } 
  #quick checks
  stopifnot(sum(which(names(dt) == time)) > 0,
            !is.na(group_by),
            is.data.frame(dt))
  
  
  if(!is.null(group_by)){ #if groups
    groups <- dt %>%
      dplyr::select(group_by) %>%
      dplyr::distinct()
    added_ts <- expand.grid.df(data.frame(groups), data.frame(times))
  }else added_ts  <- data.frame(times)
  setnames(added_ts, length(added_ts), time) #set name of times column to match original in dt
  return(added_ts)
}

#' Shorthand for getting indexes of a longer string
#' @description Shorthand function for getting indexes of a shorter string on a longer string. Can use to get indexes
#' of the names of a data.frame/data.table
#' @param str_lng A Character Vector
#' @param str_shrt A Character Vector
#' @examples
#' index_from_string(letters, c('f','g','y'))
index_from_string <- function(str_lng, str_shrt){
  stopifnot(is.character(str_lng),
            is.character(str_shrt))
  
  idx <- str_lng %in% str_shrt
  if(sum(idx) != length(str_shrt)) stop("Missing names in str_lng")
  
  which(idx)
}

#return area by time_date stamps
calc_area_intervals = function(dt, time_index, value_index, beg_times, end_times, group_by = NULL, hours = FALSE){
    stopifnot(is.data.table(dt), length(beg_times) == length(end_times))
    setkeyv(dt,c(group_by,time_index))
    areaArray = list()
    if(class(time_index) == "numeric") if(names(dt)[time_index] != "time_date") setnames(dt, time_index, "time_date")
    if(class(time_index) == "character") if(time_index != "time_date"){
      setnames(dt, time_index, "time_date")
      time_index = "time_date"
    }
    #this adds all event values (the new guys that might have not existed before) to every group_by
    #this allows easier subsetting
    
    #this adds a timestamp for every group for [every begging time and every ending time]
    all_temp_groupbys <- dt[,list(tmp = rep(head(.SD[[time_index]],1),length(beg_times)*2)),by = c(group_by)]
    setnames(all_temp_groupbys, "tmp", time_index)
    newTimes <- numeric(0)
    for(i in seq_len(beg_times)) newTimes <- c(newTimes, beg_times[i],end_times[i]) ##################left off here... gross
    all_temp_groupbys$time_date <- rep(newTimes, times = nrow(all_temp_groupbys)/length(newTimes))
    setkeyv(all_temp_groupbys, c(group_by,time_index))
    all_temp_groupbys <- all_temp_groupbys[!duplicated(all_temp_groupbys)]
    setkeyv(all_temp_groupbys, c(group_by,time_index))
    setkeyv(dt, c(group_by,time_index))
    tmp_dt <- dt[all_temp_groupbys,roll=TRUE, rollends = c(F,T)]
    setcolorder(tmp_dt, names(dt))
    dt <- rbindlist(list(tmp_dt, dt))
    
    indexes <- which(names(dt) %in% value_index)
    dt[is.na(dt[[indexes[1]]]), indexes] <-  0
    
       setkeyv(dt, c(group_by,time_index))
       dt[,diff_time := c(diff(time_date),0),by=c(group_by)]
    
    
    setkeyv(dt,c(group_by))
    #loops through each set of beg_times and end_times and calculates area of variable
    #if hours is true, will also return idle and run hours
    if(hours){
      #loops through each set of beg_times and end_times and calculates area of variable
      for(i in 1:length(beg_times)){
        temp = dt[time_date >= beg_times[i] & time_date <= end_times[i]]
        #takes diff time_date of larger set to minimize re-calculation in each subset
        if(empty(temp)){ #dealing with times when there is no data between times
          new_dt = data.table("start" = beg_times[i], "end" = end_times[i], "idle_time" = 0, "run_time" = 0)
          for(j in 1:length(value_index)){ #calculateArea by each value_index listed
            new_dt <- cbind(new_dt, 0)
          }
          setnames(new_dt, c("start","end","idle_time","run_time", value_index))
          
          areaArray[[i]] <- new_dt
        }
        else{
          setkeyv(temp, c(group_by, time_index))
          #after getting subset, calculate area and store
          new_dt <- data.table("start" = beg_times[i], "end" = end_times[i], "idle_time" = ((temp[engine_idle_state ==1, sum(diff_time)])),
                               "run_time" = sum(temp[,list("run_time_hours" = (max(time_date) - min(time_date))),by=c(group_by)]$run_time_hours))
          for(j in 1:length(value_index)){ #calc_area by each value_index listed
            new_dt <- cbind(new_dt, calc_area( temp$diff_time,temp[[value_index[j]]],diffTime=TRUE))
          }
          setnames(new_dt, c("start","end","idle_time","run_time", value_index))
          areaArray[[i]] = new_dt
        }}
      end_dt <- rbindlist(areaArray)
      setnames(end_dt, c("start","end","idle_time","run_time", value_index))
      return(end_dt)
    }
    for(i in 1:length(beg_times)){
      temp = dt[time_date >= beg_times[i] & time_date <= end_times[i]]
      #takes diff time_date of larger set to minimize re-calculation in each subset
      if(empty(temp)){ #dealing with times when there is no data between times
        new_dt <- data.table("start" = beg_times[i], "end" = end_times[i])
        for(j in 1:length(value_index)){ #calculateArea by each value_index listed
         new_dt <- cbind(new_dt, 0)
        }
        setnames(new_dt, c("start","end",value_index))
        
        areaArray[[i]] <- new_dt
      }
      else{
        setkeyv(temp, c(group_by, time_index))
        #after getting subset, calculate area and store
        new_dt <- data.table("start" = beg_times[i], "end" = end_times[i])
        for(j in 1:length(value_index)){ #calc_area by each value_index listed
          new_dt <- cbind(new_dt, calc_area(temp$diff_time,temp[[value_index[j]]],diffTime=TRUE))
        }
        setnames(new_dt, c("start","end",value_index))
        areaArray[[i]] = new_dt
      }}
    end_dt <- rbindlist(areaArray)
    setnames(end_dt, c("start","end",value_index))
    return(end_dt)
}


#returns min and max time_date of a datatable with column 'time_date'
calc_time_range = function(dt, col_name = "time_date") {
    return(paste(c(as.POSIXct(min(dt[[col_name]]),origin = "1970-01-01")),' - ',as.POSIXct(max(dt[[col_name]]),origin = "1970-01-01")))
}


