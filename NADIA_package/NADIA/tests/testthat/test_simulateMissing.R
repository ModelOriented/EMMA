library(NADIA)
library(testthat)

context("Testing simulate Missing ")

test_that("Generating missing in data", {
  expect_true(sum(is.na(simulate_missings(iris, 20))) > 0)


  expect_true(sum(is.na(simulate_missings(iris, 20, per_instances_missings = 20))) > 0)

  expect_true(sum(is.na(simulate_missings(iris, 20, per_variables_missings = 29))) > 0)

  expect_true(sum(is.na(simulate_missings(iris, 20, variables_with_missings = 2))) > 0)
})


test_that("Error check", {
  expect_warning(simulate_missings(iris, 20, per_instances_missings = 10, per_variables_missings = 20), "Incorrect dimensions of missings")


  expect_warning(simulate_missings(iris, 20, variables_with_missings = 2, per_variables_missings = 20), "per_variables_missings or variables_with_missings.")
})


test_that("PipeOpSimulatedMissing Tests", {
  expect_true(sum(PipeOpSimulateMissings$new()$train(list(tsk("iris")))[[1]]$missings()) > 0)
})
