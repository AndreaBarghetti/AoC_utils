test_that("mat_rotate works", {

  m <- matrix(1:4, nrow=2)

  expect_error(mat_rotate(m, n="GGG"))

  expect_equal(mat_rotate(m, n=1),
               structure(c(2L, 4L, 1L, 3L), dim = c(2L, 2L)))

  expect_equal(mat_rotate(m, n=2),
               structure(4:1, dim = c(2L, 2L)))

  expect_equal(mat_rotate(m, n=1),
               mat_rotate(m, n=5))

})
