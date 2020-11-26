library(testthat)
library(NADIA)
library(mlr3learners)
context("Testing full workflow with learners")
# Prepering test set
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

imps <- c(
  PipeOpAmelia$new(parallel = FALSE), PipeOpmissForest$new(parallel = FALSE), PipeOpMissMDA_PCA_MCA_FMAD$new(optimize_ncp = F),
  PipeOpmissRanger$new(), PipeOpMode_B$new(), PipeOpSample_B$new(), PipeOpSoftImpute$new(), PipeOpVIM_HD$new(), PipeOpVIM_IRMI$new(),
  PipeOpVIM_kNN$new(), PipeOpVIM_regrImp$new())


for (i in imps) {


  test_that(paste0("Testing imputation with learner ", i$id), {
    skip_on_cran()
    graph <- i %>>% PipeOpEncodeImpact$new() %>>% mlr3learners::LearnerClassifGlmnet$new()

    graph_learner <- GraphLearner$new(graph)

    set.seed(1)
    if (i$id == "impute_softImpute_B") {
      expect_warning(expect_is(resample(task, graph_learner, rsmp("cv", folds = 2)), "ResampleResult"))

    }
    else {
      expect_is(resample(task, graph_learner, rsmp("cv", folds = 2)), "ResampleResult")
    }
  })
}




## Testing only numerci imputation
imps <- c(PipeOpHist_B$new(), PipeOpOOR_B$new(), PipeOpMean_B$new(), PipeOpMode_B$new())
for (i in imps) {


  test_that(paste0("Testing imputation with learner", i$id), {
    graph <- i %>>% PipeOpEncodeImpact$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
    graph_learner <- GraphLearner$new(graph)

    set.seed(1)
    expect_is(resample(tsk("pima"), graph_learner, rsmp("cv", folds = 2)), "ResampleResult")
  })
}






### testing mice

test <- as.data.frame(cbind(as.data.frame(tsk("pima")$data()), as.integer(sample(c(1:10, NA), 768, replace = TRUE)), as.factor(sample(c("a", "b", "c", "d", NA), 768, replace = T))))


colnames(test)[10:11] <- c("int", "fct")






imps <- c(PipeOpMice$new())
for (i in imps) {


  test_that(paste0("Testing imputation with learner", i$id), {
    graph <- i %>>% PipeOpEncodeImpact$new() %>>% mlr3learners::LearnerClassifGlmnet$new()

    graph_learner <- GraphLearner$new(graph)

    set.seed(1)
    expect_is(resample(TaskClassif$new("test", test, "diabetes"), graph_learner, rsmp("cv", folds = 2)), "ResampleResult")
  })
}
