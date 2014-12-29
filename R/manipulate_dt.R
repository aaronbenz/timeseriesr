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

#given some datatable dt, return 
manipulate_subset_dt <- function(dt, beg_times, end_times, roll_beginning = F, roll_end = T, time_index = "time_date",group_by = NA){ #dt must contain a column of time_date
    #dealing with beg_time
    if(length(beg_times) >1 | length(end_times) > 1) stop("beg_times or end_times has more than one value. You can only take one subset at a time")
    original_time_index <- time_index
    dt_use <- data.table(dt)
    if(is.na(group_by)) {
        dt_use$group_by_group <- 1
        group_by <- "group_by_group"
    }
    if(class(time_index) == "numeric") if(names(dt_use)[time_index] != "time_date") setnames(dt_use, time_index, "time_date")
    if(class(time_index) == "character") if(time_index != "time_date"){
        setnames(dt_use, time_index, "time_date")
        time_index <- "time_date"
    }
    attributes_dt_use <- attributes(dt_use) #used to carry over attributes due to merge
    if(class(dt_use)[1]!="data.table") return(NULL)
    
    if(class(beg_times)!='numeric') beg_times = as.double(as.POSIXct(beg_times)) #convert beg_time/end_time to doubles if given as POSIXct
    if(class(end_times)!='numeric') end_times = as.double(as.POSIXct(end_times))
    #   if(end_time<beg_time) return(NULL)
    setkeyv(dt_use, 'time_date') #make sure dt_use is keyed by time_date
    
    setkeyv(dt_use,c(group_by,"time_date"))
    areaArray <- list()
    
    #this adds all times to dt_use
    all_groups <- dt_use[,list(tmp = rep(head(.SD[[time_index]],1),length(beg_times)*2)),by = c(group_by)]
    setnames(all_groups, "tmp", time_index)
    newTimes <- numeric(0)
    for(i in 1:length(beg_times)) newTimes <- c(newTimes, beg_times[i],end_times[i])
    all_groups$time_date <- rep(newTimes, times = nrow(all_groups)/length(newTimes))
    setkeyv(all_groups, c(group_by,time_index))
    all_groups <- all_groups[!duplicated(all_groups)]
    setkeyv(all_groups, c(group_by,time_index))
    setkeyv(dt_use, c(group_by,time_index))
    tmp_dt_use <- dt_use[all_groups,roll=TRUE, rollends = c(F,T)]
    setcolorder(tmp_dt_use, names(dt_use))
    dt_use <- rbindlist(list(tmp_dt_use, dt_use))
    
    setkeyv(dt_use, c(group_by, "time_date"))
    
    
    setkeyv(dt_use,'time_date') #rekey with new addition
    dt_use <- dt_use[time_date<=end_times & time_date >=beg_times]
    attribute_names <- names(attributes_dt_use)
    #retrieve only the ones that we care to keep as computer naturally does the rest
    attribute_names <- attribute_names[!attribute_names%in%c('row.names','class','names','.internal.selfref')]
    for (a in attribute_names)
    {
        attributes(dt_use)[[a]] <- attributes_dt_use[[a]]
    }
    setnames(dt_use, time_index, original_time_index)
    dt_use <- dt_use[!duplicated(dt_use, by=NULL)]
    if(sum(names(dt_use) %in% "group_by_group")==1) dt_use[,group_by_group := NULL]
    return(dt_use) #return subset between new time_date periods
}
#' See if value is empty
#' @description This function simply checks to see if an item contains anything. Specifically, it checks
#' if it is null, has rows, or columns.
#' @param x An R object
#' @details This is not meant for classes like lm. Only for plain old everyday boring R objects
#' @examples
#' is.empty(1:5)
#' is.empty((1:9,3,3)) 
#' is.empty(NULL)
is.empty <- function (x) 
{
    (is.null(x) || nrow(x) == 0 || ncol(x) == 0)
}
#merges the list of data.tables together that are from the hbase/local data pull
#accepts a list of data.tables, or a list structure that eventually consist of data.tables
manipulate_merge_list<- function(x, time_column, merge = TRUE, attribute_to_field = NULL, units_attribute = NULL, group_by = NULL){
    if(is.null(x) | length(x)==0) return(NULL)
    while(!is.data.frame(x[[1]])[1]) x <- unlist(x, recursive=F)
    
    is_empty <- numeric(0)
    for(i in 1:length(x)) if(is.empty(x[[i]])) {
        is_empty <- c(is_empty, i)
    }
    if(length(is_empty)!=0) x <- x[-is_empty]
    #add header info as columns to each dataset
    if(!is.null(attribute_to_field)) for(i in 1:length(x)){
        for(j in attribute_to_field){
            x[[i]][[j]] <- attr(x[[i]],j)
            x[[i]][[j]] <- attr(x[[i]],j)
        }
    }
    
    
    
    #if all the datasets only contain 1 column aka event log data, then just cbind together and return
    tmpCount <- 0
    for(i in 1:length(x)) tmpCount <- tmpCount + length(x[[i]]) #if there are only 1 column values. AKA used for cbinding event logs
    if(length(x) * (length(group_by)+length(attribute_to_field)) == tmpCount){ #*3 for new attribute names and group by
        tempDT <- x[[1]]
        if(length(x) > 1) for(i in 2:length(x)){
            tempDT <- cbind(tempDT, x[[i]][[1]])
            setnames(tempDT,length(tempDT),names(x[[i]])[1])
        }
        
        return(tempDT)
    }
    
    
    attribute_names <- names(attributes(x[[1]]))
    #retrieve only the ones that we care to keep as computer naturally does the rest
    attribute_names <- attribute_names[!attribute_names%in%c('row.names','class','names')]
    
    #rbind metrics of same type together, creating a list of data tables where each data table is a different metric type
    rbinded <- list(x[[1]]) 
    names(rbinded) <- names(rbinded[[1]])[2]
        
    #units_df <- units_insite_read_data_table("i2h_units.csv")
    if(length(x) > 1) for(i in 2:length(x)){
        if(names(x[[i]])[2]%in%names(rbinded)){
            index_to_cbind <- which(names(x[[i]])[2]==names(rbinded))
            
            #also going to automatically change the units of any of the same metrics that different in units (ex if m/s and f/s for velocity, will change so both are m/s)
            if(!is.null(units_attribute)){
                #function to change units **********************************************************************
                }
            rbinded[[index_to_cbind]] <- rbindlist(list(rbinded[[index_to_cbind]],x[[i]]))
            #get list of all attributes
            attribute_names <- names(attributes(x[[i]]))
            #retrieve only the ones that we care to keep as computer naturally does the rest
            attribute_names <- attribute_names[!attribute_names%in%c('row.names','class','names')]
            #For each attribute, for each dataframe in original list, add attribute to df2
            for(att_name in attribute_names) setattr(rbinded[[index_to_cbind]],att_name, attr(x[[i]],att_name))
        }else{
            rbinded[[length(rbinded)+1]] <- x[[i]]
            names(rbinded)[length(rbinded)]=names(x[[i]])[2]
            #get list of all attributes
            attribute_names <- names(attributes(x[[i]]))
            #retrieve only the ones that we care to keep as computer naturally does the rest
            attribute_names <- attribute_names[!attribute_names%in%c('row.names','class','names')]
            #For each attribute, for each dataframe in original list, add attribute to df2
            for(att_name in attribute_names) setattr(rbinded[[length(rbinded)]],att_name, attr(x[[i]],att_name))
        }
    }
    #do merge
    if(merge == TRUE){
        if(length(rbinded) > 0) {
            newDT <- rbinded[[1]]
            if(length(rbinded) > 1){
                for(i in 2:length(rbinded)){
                    key <- c(names(newDT),names(rbinded[[i]]))
                    keynames <- key[which(duplicated(key))]
                    setkeyv(newDT,keynames)
                    setkeyv(x[[i]],keynames)
                    #print(names(x[[i]]))
                    newDT <- merge(newDT, rbinded[[i]], all=TRUE, allow.cartesian=TRUE)
                }
            }
        }
        setkeyv(newDT,c(group_by,time_column))
        
        #replaces any nas by column by group
        newDT <- newDT[,lapply(.SD,vreplace),by = c(group_by)]
        
        setkeyv(newDT,c(time_column,group_by))
        
        #get list of all attributes
        attribute_names <- names(attributes(rbinded[[1]]))
        #retrieve only the ones that we care to keep as computer naturally does the rest
        attribute_names <- attribute_names[!attribute_names%in%c('row.names','class','names','.internal.selfref')]
        #For each attribute, for each dataframe in original list, add attribute to df2
        for(att_name in attribute_names){
            setattr(newDT,att_name, c('time_date','id_1','id_2'))
            for(i in 1:length(rbinded)) setattr(newDT,att_name, c(attr(newDT,att_name), attr(rbinded[[i]],att_name)[2]))
        }
    }else newDT <- data.table()
    lst <- list("merged_dt" = newDT, "variables" = rbinded)
    
    return(lst)
}

#reduce points for graphing of a column
#'@describeIn point_reduce
dtreduce <- function(dt, column, tolerance = .01){
  index <- point_reduce(dt[[column]], tolerance = tolerance)$index
  dt[index,]
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

#melts data table and reduces where there are duplicate id-meltedcolumn groups. For time_date, this will 
manipulate_melt_dt <- function(dt, id_var, melt_columns, time_column, melted_value_name = "value", 
                   melted_column_name = "metric_name", reduce = TRUE, ...){
    if(is.null(melt_columns)) return(dt$merged_dt)  
    dt <- unlist(list(dt),recursive=F)
    
    #replacing all column names of time to be time_date for ease of use within functions
    id_var[id_var == time_column] <- "time_date"
    setnames(dt$merged_dt,time_column,"time_date")
    for(i in dt$variables) setnames(i,time_column,"time_date")
    #determine which variables were calculated as the names differ between merged_dt and variables in dt
    rbind_names <- melt_columns[melt_columns %in% names(dt$variables)]
    new_names <- melt_columns[!melt_columns%in%rbind_names]
   lst <- list()
    #rbinding all variables from original pull (the ones that we originally pulled from hbase that are deduplicated)
    for(i in rbind_names){
#         dt$variables[[i]]$units <- attr(dt$variables[[i]], "unit_short")[[length(attr(dt$variables[[i]], "unit_short"))]]
        dt$variables[[i]]$metric_name <- i
        setnames(dt$variables[[i]], i, melted_value_name)
        setcolorder(dt$variables[[i]], c(id_var,melted_value_name,melted_column_name))
        lst[[length(lst)+1]] <- dt$variables[[i]]
    }
    #add any new variables to the new DT variable
    for(i in new_names){
#         unit <- attr(dt$merged_dt, "unit_short")[which(names(dt$merged_dt)== i)]
        temp_dt <- dt$merged_dt[,column_index(dt$merged_dt,id_var), with=F]
        temp_dt[[melted_column_name]] <- i
        temp_dt[[melted_value_name]] <- dt$merged_dt[[i]]
        lst[[length(lst)+1]] <- temp_dt
        setcolorder(lst[[length(lst)]], c(id_var,melted_value_name,melted_column_name))
    }
    newDT <- rbindlist(lst)
    setnames(newDT,"time_date",time_column)
    return(newDT)
}

#' Retern a vector that is offset by one
#' @description This function takes a vector x from 1 to N , and returns it from 2 to N, and N+1. This mainly used
#' for short hand purposes in other functions
#' @param x A vector
#' @param end_value The last value to add on to the vector, since it will be one short of the original `x`
#' @examples
#' tmp <- 1:10
#' vnext(tmp)
#' vnext(temp, end_value = 0)
vnext <- function(x, end_value = "Last"){
    if(end_value == "Last") end_value <- x[length(x)]
    if(class(x) != class(end_value)) warning("end_value is not the same class as x")
    
    if(length(x)>1) return(c(x[2:length(x)],end_value))
    return(c(0))
}

