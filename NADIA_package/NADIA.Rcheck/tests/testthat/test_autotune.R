testthat::context("Cheking all functions form packages")
library(NADIA)
library(testthat)

test_set <- iris
test_set$Sepal.Length[sample(1:150, 50)] <- NA
test_set$Species[sample(1:150, 50)] <- NA
col_type <- 1:5
col_type <- c(rep("numeric", 4), "factor")
percent_of_missing <- 1:5
for (i in percent_of_missing) {
  percent_of_missing[i] <- (sum(is.na(test_set[, i])) / length(test_set)) * 100
}
col_no_miss <- colnames(test_set)[percent_of_missing == 0]
col_miss <- colnames(test_set)[percent_of_missing > 0]



test_set <- iris
test_set$Sepal.Length[sample(1:150, 50)] <- NA
test_set$Species[sample(1:150, 50)] <- NA
col_type <- 1:5
col_type <- c(rep("numeric", 4), "factor")
percent_of_missing <- 1:5
for (i in percent_of_missing) {
  percent_of_missing[i] <- (sum(is.na(test_set[, i])) / length(test_set)) * 100
}
col_no_miss <- colnames(test_set)[percent_of_missing == 0]
col_miss <- colnames(test_set)[percent_of_missing > 0]

test_that("Modyfied iris set Amleia", {
  # AMELIA
  expect_equal(sum(is.na(autotune_Amelia(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})
test_that("Modyfied iris set mice", {
  # mice
  expect_equal(sum(is.na(autotune_mice(test_set, col_miss = col_miss, col_no_miss = col_no_miss, col_type = col_type, percent_of_missing = percent_of_missing, optimize = F, iter = 2))), 0)
})
test_that("Modyfied iris set missForest", {
  # missForest
  expect_equal(sum(is.na(autotune_missForest(test_set, col_type = col_type, percent_of_missing = percent_of_missing, parallel = F, optimize = F))), 0)
})
test_that("Modyfied iris set missRanger", {
  # missRanger
  expect_equal(sum(is.na(autotune_missRanger(test_set, percent_of_missing = percent_of_missing))), 0)
})
test_that("Modyfied iris set softImpute", {
  # SoftImpute
  expect_equal(sum(is.na(autotune_softImpute(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})
test_that("Modyfied iris set VIM_HD", {
  # VIM_hotdeck
  expect_equal(sum(is.na(autotune_VIM_hotdeck(test_set, percent_of_missing = percent_of_missing))), 0)
})
test_that("Modyfied iris set VIM_irmi", {
  # VIM_irmi
  expect_equal(sum(is.na(autotune_VIM_Irmi(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})
test_that("Modyfied iris set VIM_knn", {
  # VIM_knn
  expect_equal(sum(is.na(autotune_VIM_kNN(test_set, percent_of_missing = percent_of_missing))), 0)
})
test_that("Modyfied iris set ViM_regrImp", {
  # VIM_regImp
  expect_equal(sum(is.na(autotune_VIM_regrImp(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})
test_that("Modyfied iris set missMDA FMAD", {
  # missMDA FMAD...
  expect_equal(sum(is.na(missMDA_FMAD_MCA_PCA(test_set, col_type = col_type, percent_of_missing = percent_of_missing, optimize_ncp = F))), 0)
})
test_that("Modyfied iris set missMDA MFA", {
  # missMDA MFA
  expect_equal(sum(is.na(missMDA_MFA(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})



test_set <- data.frame(
  "a" = runif(100, 0, 10),
  "b" = rnorm(100, 0, 3),
  "c" = rbeta(100, 2, 2),
  "d" = rcauchy(100),

  "f" = rweibull(n = 100, 100, 3))
for (i in 1:5) {
  test_set[, i][sample(1:100, sample(1:70, 1))] <- NA

}
col_type <- rep("numeric", 5)
percent_of_missing <- 1:5
for (i in percent_of_missing) {
  percent_of_missing[i] <- (sum(is.na(test_set[, i])) / length(test_set)) * 100
}
col_no_miss <- colnames(test_set)[percent_of_missing == 0]
col_miss <- colnames(test_set)[percent_of_missing > 0]

test_that("Small numeric data set Amleia", {
  # AMELIA
  expect_equal(sum(is.na(autotune_Amelia(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})

test_that("Small numeric data set mice", {
  # mice
  expect_error(sum(is.na(autotune_mice(test_set, col_miss = col_miss, col_no_miss = col_no_miss, col_type = col_type, percent_of_missing = percent_of_missing, optimize = F, iter = 2))), "`mice` detected constant and/or collinear variables. No predictors were left after their removal.")
})
test_that("Small numeric data set missForest", {
  # missForest
  expect_equal(sum(is.na(autotune_missForest(test_set, col_type = col_type, percent_of_missing = percent_of_missing, parallel = F, optimize = F))), 0)
})
test_that("Small numeric data set missRanger", {
  # missRanger
  expect_equal(sum(is.na(autotune_missRanger(test_set, percent_of_missing = percent_of_missing))), 0)
})
test_that("Small numeric data set softImpute", {
  # SoftImpute
  expect_equal(sum(is.na(autotune_softImpute(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})
test_that("Small numeric data set VIM_HD", {
  # VIM_hotdeck
  expect_equal(sum(is.na(autotune_VIM_hotdeck(test_set, percent_of_missing = percent_of_missing))), 0)
})
test_that("Small numeric data set VIM_irmi", {
  # VIM_irmi
  expect_equal(sum(is.na(autotune_VIM_Irmi(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})
test_that("Small numeric data set VIM_knn", {
  # VIM_knn
  expect_equal(sum(is.na(autotune_VIM_kNN(test_set, percent_of_missing = percent_of_missing))), 0)
})
test_that("Small numeric data set VIM_regrImp", {
  # VIM_regImp
  expect_error(sum(is.na(autotune_VIM_regrImp(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), "No values with no missing values")
})
test_that("Small numeric data set missMDA FMAD...", {
  # missMDA FMAD...
  expect_equal(sum(is.na(missMDA_FMAD_MCA_PCA(test_set, col_type = col_type, percent_of_missing = percent_of_missing, optimize_ncp = F))), 0)
})
test_that("Small numeric data set missMDA MFA", {
  # missMDA MFA
  expect_equal(sum(is.na(missMDA_MFA(test_set, col_type = col_type, percent_of_missing = percent_of_missing))), 0)
})




test_set <- iris
test_set$Sepal.Length[sample(1:150, 50)] <- NA
test_set$Species[sample(1:150, 50)] <- NA
col_type <- 1:5
col_type <- c(rep("numeric", 3), "integer", "factor")
test_set[, 4] <- as.integer(1:150)

percent_of_missing <- 1:5
for (i in percent_of_missing) {
  percent_of_missing[i] <- (sum(is.na(test_set[, i])) / length(test_set)) * 100
}
col_no_miss <- colnames(test_set)[percent_of_missing == 0]
col_miss <- colnames(test_set)[percent_of_missing > 0]
test_that("Corect class return Amelia", {
  # Amelia
  imp_set <- autotune_Amelia(test_set, col_type = col_type, percent_of_missing = percent_of_missing)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})

test_that("Corect class return mice", {
  # mice
  imp_set <- autotune_mice(test_set, col_type = col_type, percent_of_missing = percent_of_missing, col_miss = col_miss, col_no_miss = col_no_miss, iter = 2, optimize = F)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return missForest", {
  ## missForest
  imp_set <- autotune_missForest(test_set, col_type = col_type, percent_of_missing = percent_of_missing, optimize = F)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return missRanger", {
  ## missRanger
  imp_set <- autotune_missRanger(test_set, percent_of_missing = percent_of_missing)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return softImpute", {
  ## softImpute
  imp_set <- autotune_softImpute(test_set, percent_of_missing = percent_of_missing, col_type = col_type)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return ViM_HD", {
  # VIM_HD
  imp_set <- autotune_VIM_hotdeck(test_set, percent_of_missing = percent_of_missing)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return ViM_IRMI", {
  # VIM_IRMI
  imp_set <- autotune_VIM_Irmi(test_set, percent_of_missing = percent_of_missing, col_type = col_type)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return ViM_regrImp", {
  # VIM_regrIMP
  imp_set <- autotune_VIM_regrImp(test_set, percent_of_missing = percent_of_missing, col_type = col_type)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return ViM_knn", {
  # VIM_knn
  imp_set <- autotune_VIM_kNN(test_set, percent_of_missing = percent_of_missing)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return missMDA FMAD...", {
  # missMDA FMAD...
  imp_set <- missMDA_FMAD_MCA_PCA(test_set, percent_of_missing = percent_of_missing, col_type = col_type, optimize_ncp = F)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})
test_that("Corect class return missMDA MFA", {
  # missMDA MFA
  imp_set <- missMDA_MFA(test_set, percent_of_missing = percent_of_missing, col_type = col_type)

  imp_type <- 1:5
  for (i in 1:5) {
    imp_type[i] <- class(imp_set[, i])
  }
  expect_equal(imp_type, col_type)
})


test_that("Optimlaization test", {
  test_set <- iris
  test_set$Sepal.Length[sample(1:150, 50)] <- NA
  test_set$Species[sample(1:150, 50)] <- NA
  col_type <- 1:5
  col_type <- c(rep("numeric", 3), "integer", "factor")
  test_set[, 4] <- as.integer(1:150)

  percent_of_missing <- 1:5
  for (i in percent_of_missing) {
    percent_of_missing[i] <- (sum(is.na(test_set[, i])) / length(test_set)) * 100
  }
  col_no_miss <- colnames(test_set)[percent_of_missing == 0]
  col_miss <- colnames(test_set)[percent_of_missing > 0]

  skip_on_cran()
  # mice
  expect_equal(sum(is.na(autotune_mice(test_set, col_miss = col_miss, col_no_miss = col_no_miss, col_type = col_type, percent_of_missing = percent_of_missing, optimize = T, iter = 5))), 0)

  # missForest
  expect_equal(sum(is.na(autotune_missForest(test_set, col_type = col_type, percent_of_missing = percent_of_missing, parallel = F, optimize = T))), 0)

  # missRanger
  expect_equal(sum(is.na(autotune_missRanger(test_set, percent_of_missing = percent_of_missing, optimize = T))), 0)

  # missMDA FMAD
  expect_equal(sum(is.na(missMDA_FMAD_MCA_PCA(test_set, col_type = col_type, percent_of_missing = percent_of_missing, optimize_ncp = T))), 0)
})
