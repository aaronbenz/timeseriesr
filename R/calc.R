#Given a time_date element and a value element, find the area using left riemann sum
#if return_as_vector = True, returns total over entire area by each row
# if False, returns array of total of each row
#if diffTime = TRUE, will treat time_date as if it is diff time_date, so it does have to take it
calc_area = function(time_date, value, as_vector = FALSE, diff_time=FALSE, neg_area = FALSE){
    #excepting time_date and value to be numeric vectors of the same length
    stopifnot(length(time_date) == length(value),
              is.numeric(time_date), is.numeric(value),
              is.logical(as_vector), is.logical(diff_time), is.logical(neg_area))
    
    if(length(time_date) == 0) return(0)
    if(!neg_area) value <- abs(value) #if all area should be treated as positive, change all values to positive
    if(as_vector==FALSE) {
        if(diff_time == FALSE) return((c(diff(time_date),0) %*% value)[1,1])
        return((time_date %*% value)[1,1])
    }
    else{
        if(diff_time == FALSE) return(c(diff(time_date),0) * value)
        return(time_date * value)
    }
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

