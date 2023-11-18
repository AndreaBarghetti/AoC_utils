test_that("circular generics work", {

  myc <- circular(c(1,3,5,7,4))
  myc2 <- circular(letters[1:5])

  expect_equal(myc[6:7], circular(c(1,3)))
  expect_equal(myc[c(4,5,6,10)], structure(c(7, 4, 1, 4), class = c("circular", "numeric")))
  expect_equal(myc[[c(4,5,6,10)]], structure(c(7, 4, 1, 4), class = c("circular", "numeric")))
  expect_equal(myc[myc>4], structure(c(5, 7), class = c("circular", "numeric")))
  expect_equal(myc2[5:6], structure(c("e", "a"), class = c("circular", "character")))
  expect_true(is.circular(myc))

})
