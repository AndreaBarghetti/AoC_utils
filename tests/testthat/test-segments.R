test_that("seg_include works", {

  expect_true(
    seg_include(segment(c(1,1,5,5)), 3,3, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(-5,-5,-1,-1)), -2,-2, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(-5,5,5,-5)), -1,1, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(1,1,5,1)), 3,1, esclude_extremes = T)
    )

  expect_true(
    seg_include(segment(c(1,1,1,5)), 1,3, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(1,5,1,1)), 1,3, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(5,1,1,1)), 3,1, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(-5,-1,-1,-1)), -3,-1, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(1,-1,1,-5)), 1,-3, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(1,10,10,7)), 7,8, esclude_extremes = T)
  )

  expect_true(
    seg_include(segment(c(1,9,10,6)), 7,7, esclude_extremes = T)
  )

  expect_false(
    seg_include(segment(c(1,1,3,3)), 5,5, esclude_extremes = T)
  )
  expect_false(
    seg_include(segment(c(1,1,3,3)), -5,-5, esclude_extremes = T)
  )
  expect_false(
    seg_include(segment(c(1,1,3,1)), 0,1, esclude_extremes = T)
  )
  expect_false(
    seg_include(segment(c(1,1,1,3)), 1,5, esclude_extremes = T)
  )
  expect_false(
    seg_include(segment(c(1,-1,1,-3)), 1,-5, esclude_extremes = T)
  )
  expect_false(
    seg_include(segment(c(1,1,100,100.1)), 2,2, esclude_extremes = T)
  )

})
