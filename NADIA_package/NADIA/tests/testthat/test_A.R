library(testthat)
library(NADIA)
context("Testing avalible methods in mice.reuse")


test_that("Testing mice A methods", {
  test_set <- iris
  test_set$Sepal.Length[sample(1:150, 50)] <- NA
  test_set$Species[sample(1:150, 50)] <- NA


  idx <- sample(1:150, 100)
  train_set <- test_set[idx, ]
  test_set <- test_set[-idx, ]

  ### 'pmm'
  model <- mice(train_set, method = "pmm", printFlag = FALSE)
    expect_equal(sum(is.na(mice.reuse(model, test_set, printFlag = FALSE)$`1`)), 0)


  ### 'rf'
  model <- mice(train_set, method = "rf", printFlag = FALSE)
  expect_equal(sum(is.na(mice.reuse(model, test_set, printFlag = FALSE)$`1`)), 0)

  ### 'sample'
  model <- mice(train_set, method = "sample", printFlag = FALSE)
  expect_equal(sum(is.na(mice.reuse(model, test_set, printFlag = FALSE)$`1`)), 0)

  ### 'cart'
  model <- mice(train_set, method = "cart", printFlag = FALSE)
  expect_equal(sum(is.na(mice.reuse(model, test_set, printFlag = FALSE)$`1`)), 0)

  ### default
  model <- mice(train_set, printFlag = FALSE)
  expect_equal(sum(is.na(mice.reuse(model, test_set, printFlag = FALSE)$`1`)), 0)
})


test_that("Testing missMDA in A approach",{

  ###  Creating Pipe
  expect_is(PipeOpMissMDA_PCA_MCA_FMAD_A$new(), "PipeOpImpute")

  ### Cheking if its work corectly

  grpah <- PipeOpMissMDA_PCA_MCA_FMAD_A$new() %>>% mlr3learners::LearnerClassifGlmnet$new()

  learner <- GraphLearner$new(grpah)

  expect_is(resample(tsk('pima'),learner,rsmp('cv',folds=5)),"ResampleResult")

})
