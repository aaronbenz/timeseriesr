#test that 

#calc.R
#----------------------------------------------------------------

#calc_area
  #setup
    time <- 1:100
    value <- sin(1:100)
  #examples
    calc_area(time, value)
    calc_area(time, value, T)
    calc_area(time, value, diff_time = T)
    calc_area(time, value, neg_area = T)
    calc_area(time, value, T, T)
  #test cases
    calc_area(time[-1], value)
    calc_area(time, value[-1])
    calc_area(0,0)


