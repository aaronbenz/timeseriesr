#test calc.R
#----------------------------------------------------------------
#calc_area
#setup
  time <- 1:10
  value_sin <- sin(1:10)
  value_lin <- 1:10
test_that("Test Basic - No Parameters",{
  expect_equal(round(calc_area(time, value_sin),2), 5.95)
  expect_equal(calc_area(time, value_lin), 45)
  expect_warning(calc_area(c(1:9,NA),1:10))
  expect_true(is.na(calc_area(c(1:9,NA),1:10)))
  expect_equal(calc_area(1:10,c(1:5, NA, 7:10)),44)
  expect_equal(calc_area(1:10,c(1:5, NA, 7:10),replacement = 10),49)
  expect_equal(calc_area(1:10,c(NA, 2:10),first = 10),54)
})

test_that("Test Unit Conversion",{
  expect_equal(calc_area(time, value_lin,unit_conversion = .1),4.5)
  expect_equal(calc_area(time, value_lin,unit_conversion = .1, as_vector = T),c(.1*1:9,0))
})

test_that("Test Basic - as_vector = TRUE",{
  expect_equal(round(calc_area(time, value_sin, as_vector = T),2), c(.84,.91,.14,.76,.96,.28,.66,.99,.41,.00))
  expect_equal(calc_area(time, value_lin,as_vector = T), c(1:9, 0))
})

test_that("Test Basic - diff_time = TRUE",{
  expect_equal(round(calc_area(time, value_sin, diff_time = T),2), 34.24)
  expect_equal(calc_area(time, value_lin,diff_time = T), 385)
})

test_that("Test Basic - neg_area = TRUE",{
  expect_equal(round(calc_area(time, value_sin, neg_area = T),2), 1.96)
  expect_equal(calc_area(time, value_lin,neg_area = T), 45)
})

test_that("Test Basic - as_vector = TRUE and neg_area = TRUE",{
  expect_equal(round(calc_area(time, value_sin,as_vector = T, neg_area = T),2), c(.84,.91,.14,-.76,-.96,-.28,.66,.99,.41,.00))
  expect_equal(calc_area(time, value_lin,as_vector = T, neg_area = T), c(1:9,0))
})
  
#calc_area(time, value, T)
#calc_area(time, value, diff_time = T)
#calc_area(time, value, neg_area = T)
# calc_area(time, value, T, T)
#test cases

# calc_area(time[-1], value)
# calc_area(time, value[-1])
# calc_area(0,0)
