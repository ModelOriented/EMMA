library(testthat)
library(NaDIA)
context("Testing simple imputation pipeline")




test_that("Creting Mode_B", {
  expect_is(PipeOpMode_B$new(), "PipeOpImpute")
})
test_that("Creting Median_B", {
  expect_is(PipeOpMedian_B$new(), "PipeOpImpute")
})
test_that("Creting Mean_B", {
  expect_is(PipeOpMean_B$new(), "PipeOpImpute")
})
test_that("Creting Hist_B", {
  expect_is(PipeOpHist_B$new(), "PipeOpImpute")
})
test_that("Creting OOR_B", {
  expect_is(PipeOpOOR_B$new(), "PipeOpImpute")
})
test_that("Creting Sample_B", {
  expect_is(PipeOpSample_B$new(), "PipeOpImpute")
})


test_that("Pipe working on example task", {
  test_set <- iris
  test_set$Sepal.Length[sample(1:150, 50)] <- NA
  test_set$Species[sample(1:150, 50)] <- NA
  test_set[, 4] <- as.integer(1:150)
  test_set$Class <- as.factor(sample(c("a", "b"), 150, replace = T))

  # Creating task
  task <- TaskClassif$new(id = "test", backend = test_set, target = "Class")



  # Mean
  expect_equal(PipeOpMean_B$new()$train(list(task))[[1]]$missings(), c("Class" = 0, "Petal.Length" = 0, "Petal.Width" = 0, "Sepal.Width" = 0, "Species" = 50, "Sepal.Length" = 0))

  # Mode
  expect_equal(PipeOpMode_B$new()$train(list(task))[[1]]$missings(), c("Class" = 0, "Petal.Length" = 0, "Petal.Width" = 0, "Sepal.Width" = 0, "Sepal.Length" = 0, "Species" = 0))

  # Median
  expect_equal(PipeOpMedian_B$new()$train(list(task))[[1]]$missings(), c("Class" = 0, "Petal.Length" = 0, "Petal.Width" = 0, "Sepal.Width" = 0, "Species" = 50, "Sepal.Length" = 0))

  # OOR
  expect_equal(PipeOpOOR_B$new()$train(list(task))[[1]]$missings(), c("Class" = 0, "Petal.Length" = 0, "Petal.Width" = 0, "Sepal.Width" = 0, "Sepal.Length" = 0, "Species" = 0))

  # HIST
  expect_equal(PipeOpHist_B$new()$train(list(task))[[1]]$missings(), c("Class" = 0, "Petal.Length" = 0, "Petal.Width" = 0, "Sepal.Width" = 0, "Species" = 50, "Sepal.Length" = 0))

  # Smaple
  expect_equal(PipeOpSample_B$new()$train(list(task))[[1]]$missings(), c("Class" = 0, "Petal.Length" = 0, "Petal.Width" = 0, "Sepal.Width" = 0, "Sepal.Length" = 0, "Species" = 0))
})
