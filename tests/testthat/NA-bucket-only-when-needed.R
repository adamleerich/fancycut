
context("NA Buckets")


test_that("NA bucket not in levels when not needed", {
  x <- seq.int(0, 1, 0.25)
  y <- fancycut(x, low = '[0,0.5]', high = '(0.5, 1]')

  expect_identical(levels(y), c("low", "high"))
})






