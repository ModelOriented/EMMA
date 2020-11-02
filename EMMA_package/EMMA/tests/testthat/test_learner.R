library(EMMA)
library(testthat)

context("Testing GRF learner")

graph <- PipeOpEncodeImpact$new() %>>% LearnerClassifGrf$new()
graph_learner <- GraphLearner$new(graph)



test_that("GRF without missing", {
  expect_is(resample(tsk("german_credit"), graph_learner, rsmp("cv", folds = 2)), "ResampleResult")
})

test_that("GRF with missing numeric", {
  expect_is(resample(tsk("pima"), graph_learner, rsmp("cv", folds = 2)), "ResampleResult")
})

test_that("GRF with missing in all type of valuse", {
  raw_data <- data.frame(
    a = (sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = (sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = (sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = (sample(c("male", "female", "trans", "other"), 1000, replace = TRUE)))
  raw_data$b[sample(1:1000, 200)] <- NA
  raw_data$a <- as.factor(raw_data$a)
  raw_data$c <- as.factor(raw_data$c)
  raw_data$e <- as.factor(raw_data$e)
  raw_data$f <- as.factor(raw_data$f)

  task <- TaskClassif$new("test", raw_data, "e")


  expect_is(resample(task, graph_learner, rsmp("cv", folds = 2)), "ResampleResult")
})
