#vreplace and dtreplace
#setup
dt <- data.frame("num1" = 1:10,
                 "num2" = c(1,2,3,NA,NA,6,7,8,9,10),
                 "num3" = c(NA,2:10),
                 "char1" = letters[1:10],
                 "char2" = c(NA, letters[2:10]),
                 "char3" = c('a',NA,NA,letters[4:10]))
          
tmp <- c(NA,NA, 2,3,NA,5,6)
tmp2 <- c(1,2,3,3,NA,NA,4,5)

test_that("Test vreplace",{
  expect_equal(vreplace(tmp), c(NA, NA, 2, 3, 3, 5, 6))
  expect_warning(vreplace(tmp))
  expect_equal(vreplace(tmp, replacement = 1), c(1,1,2,3,1,5,6))
  expect_equal(vreplace(tmp, replace = 2, replacement = "LOCF"), c(NA,NA,NA,3,3,5,6))
  expect_warning(vreplace(tmp, replace = 2, replacement = "LOCF"))
  expect_equal(vreplace(tmp2), c(1,2,3,3,3,3,4,5))
  expect_equal(vreplace(tmp2, replace = 1), c(NA,2,3,3,3,3,4,5))
  expect_warning(vreplace(tmp2, replace = 1))
  expect_equal(vreplace(tmp2, replace = 1, replacement = 2), c(2,2,3,3,NA,NA,4,5))  
})


# dtreplace(dt, replace = NA)
