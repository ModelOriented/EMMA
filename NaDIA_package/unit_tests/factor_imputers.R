library(testthat)
library(EMMA)

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
  
  train_task <- pipe_imp$train(list(task$filter(1:100)))
  
  test_that("impute_train", {
    expect_equal(sum(train_task$output$missings()), 0)
  })
  
  predict_task <- pipe_imp$predict(list(task$filter(101:150)))
  
  test_that("impute_test", {
    expect_equal(sum(predict_task$output$missings()), 0)
  })
  
})

