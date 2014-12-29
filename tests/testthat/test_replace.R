#vreplace and dtreplace, and deduplicate
#setup
dt <- data.frame("num1" = 1:10,
                 "num2" = c(1,2,3,NA,NA,6,7,8,9,10),
                 "num3" = c(NA,2:10),
                 "char1" = as.vector(letters[1:10]),
                 "char2" = c(NA, letters[2:10]),
                 "char3" = c('a',NA,NA,letters[4:10]))
          
tmp <- c(NA,NA, 2,3,NA,5,6)
tmp2 <- c(1,2,3,3,NA,NA,4,5)

test_that("Test vreplace",{
  expect_warning(expect_equal(vreplace(tmp), c(NA, NA, 2, 3, 3, 5, 6)))
  expect_equal(vreplace(tmp, replacement = 1), c(1,1,2,3,1,5,6))
  expect_warning(expect_equal(vreplace(tmp, replace = 2, replacement = "LOCF"), c(NA,NA,NA,3,3,5,6)))
  expect_warning(vreplace(tmp, replace = 2, replacement = "LOCF"))
  expect_equal(vreplace(tmp2), c(1,2,3,3,3,3,4,5))
  expect_warning(expect_equal(vreplace(tmp2, replace = 1), c(NA,2,3,3,3,3,4,5)))
  expect_equal(vreplace(tmp2, replace = 1, replacement = 2), c(2,2,3,3,NA,NA,4,5))  
  expect_equal(vreplace(as.vector(dt$char3)), c("a","a","a","d","e","f","g","h","i","j"))
})

#test deduplicate
tmp <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10), 1,2,3)

test_that("Test deduplicate",{
  expect_equal(vdeduplicate(tmp), c(1,2,3,4,5,1,2,3))
  expect_equal(vdeduplicate(tmp, TRUE), c(1,11,21,31,41,51,52,53))
 })
