#test that for plot_reduce

#setup
dt <- data.frame(x = 1:10, y = seq(1,3,length.out = 10) %>% sin %>% round(2))

test_that("Test point_reduce Core Functions",{
  expect_equal(vreduce(dt$y,.1), c(1,2,7,8,9,10))
  expect_equal(vreduce(dt$x,.2), c(1,3,5,7,9,10))
  expect_equal(vreduce(dt$y, 2), c(1,10))
  
  })

test_that("Test dtreduce functions",{
  expect_equal(dtreduce(dt,"y",.1)$y, c(.84,.94,.72,.55,.36,.14))
  expect_equal(dtreduce(dt,"y",.1)$x, c(1,2,7,8,9,10))
})