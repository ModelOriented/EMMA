library(testthat)
library(EMMA)
library(mlr3pipelines)
library(mlr3learners)
library(mlr3)

#Testing all packages, assuming they are all prepeared for only factors with missings
factor_imputers <-  c(PipeOpSample_B, PipeOpOOR_B, PipeOpMode_B, 
                      PipeOpAmelia, PipeOpmissForest, PipeOpMice, PipeOpSoftImpute, PipeOpmissRanger, 
                      PipeOpVIM_IRMI, PipeOpVIM_HD, PipeOpVIM_kNN, PipeOpVIM_regrImp, PipeOpMissMDA_MFA, PipeOpMissMDA_PCA_MCA_FMAD, 
                      PipeOpMice_A)

test_result <- lapply(factor_imputers, function(p){
  
  pipe_imp <- p$new()
  
  df_miss <- iris
  
  set.seed(123)
  
  df_miss$Sepal.Length <- factor(sample(10, nrow(df_miss), replace = TRUE))
  df_miss$Petal.Length <- factor(sample(10, nrow(df_miss), replace = TRUE))
  
  df_miss[sample(nrow(df_miss), size = 20), c("Sepal.Length", "Petal.Length")] <- NA
  
  task <- TaskClassif$new(id = "test_task", backend = df_miss, target = "Species")
  
  pipe_lrn <- lrn("classif.ranger")
  
  graph <- pipe_imp %>>% pipe_lrn
  graph_lrn <- GraphLearner$new(graph)
  
  rr <- resample(task, graph_lrn, rsmp("holdout"))
  
  test_that("resample result exists", {
    expect_is(rr, "ResampleResult")
  })
  
  test_that("no errors in resample", {
    expect_equal(nrow(rr$errors), 0)
  })
  
  test_that("no missings left", {
    expect_equal(sum(is.na(rr$prediction()$response)), 0)
  })
  
})

