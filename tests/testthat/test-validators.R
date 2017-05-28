context("validation functions")

test_that("is_between returns TRUE if arg[2] < arg[1] < arg[3]", {
  expect_true(is_between(0.5, -2, 1))
  expect_error(is_between(0, 0.5, 1))
})

test_that("is_between raises error unless arg[2] < arg[1] < arg[3]", {
  expect_error(is_between(1, 1, 2))
})

test_that("is_between used with inclusive = TRUE", {
  expect_true(is_between(1, 1, 2, inclusive = TRUE))
})
