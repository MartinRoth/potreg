library(potreg)
context("Input testing for declustering")

test_that("correct input handling", {
  expect_error(declusterSimpleSeparation("foo"), "time should be a numeric vector.")
  expect_error(declusterSimpleSeparation(c(1,2,3), "foo"), "x should be a numeric vector.")
  expect_error(declusterSimpleSeparation(c(1,2,3), c(2,3,1), "foo"), "sep should be a numeric.")
  expect_error(declusterSimpleSeparation(c(1,2,3), c(2,3,1), c(1,2)), "sep should be a numeric.")
  expect_error(declusterSimpleSeparation(c(1,2,3), c(2,3), 1), "time and x must have same length.")
})

context("Output testing for declustering")
test_that("simple declustering output", {
  expect_equal(declusterSimpleSeparation(c(1,2,3),   c(2,3,1),   1), 2)
  expect_equal(declusterSimpleSeparation(c(1,5,10),  c(2,3,1),   1), c(1,2,3))
  expect_equal(declusterSimpleSeparation(c(1,2,3),   c(3,1,2),   2), 1)
  expect_equal(declusterSimpleSeparation(c(1,2,3,6), c(2,3,1,1), 1), c(2,4))
  expect_equal(declusterSimpleSeparation(c(1,2,3,4), c(3,1,2,3), 2), c(1,4))
  expect_equal(declusterSimpleSeparation(c(1,2,3,4), c(3,1,2,4), 3), 4)
  expect_equal(declusterSimpleSeparation(c(1,2,3,4), c(3,1,2,1), 2), 1)
  expect_equal(declusterSimpleSeparation(c(1,2,3,6,7,8,12,13),
                                         c(1,3,1,1,1,3,2,3),     2), c(2, 6, 8))
})
