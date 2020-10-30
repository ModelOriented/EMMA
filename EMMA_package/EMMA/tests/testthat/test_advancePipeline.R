library(testthat)
library(EMMA)
context('Testing advance imputation pipeline')

test_that('Creting Amelia',{
  #Amelia
  expect_is(PipeOpAmelia$new(),'PipeOpImpute')

  #mice
  expect_is(PipeOpMice$new(),'PipeOpImpute')

  #mice_A
  expect_is(PipeOpMice_A$new(),'PipeOpImpute')

  #missForest
  expect_is(PipeOpmissForest$new(),'PipeOpImpute')

  #missRanger
  expect_is(PipeOpmissRanger$new(),'PipeOpImpute')

  #softImpute
  expect_is(PipeOpSoftImpute$new(),'PipeOpImpute')

  #VIM_HD
  expect_is(PipeOpVIM_HD$new(),'PipeOpImpute')

  #VIM_IRMI
  expect_is(PipeOpVIM_IRMI$new(),'PipeOpImpute')

  #VIM_KNN
  expect_is(PipeOpVIM_kNN$new(),'PipeOpImpute')

  #VIM_rergrImp
  expect_is(PipeOpVIM_regrImp$new(),'PipeOpImpute')
})

raw_data <- data.frame(a = (sample(c("red","yellow","blue",NA),1000, replace = TRUE)),
                       b = 1:1000,
                       c = (sample(c("YES","NO",NA),1000,replace=TRUE)),
                       d = runif(1000,1,10),
                       e = (sample(c("YES","NO"), 1000, replace = TRUE)),
                       f = (sample(c("male","female","trans","other",NA), 1000, replace = TRUE)))
raw_data$b[sample(1:1000,200)] <- NA
raw_data$a <- as.factor(raw_data$a)
raw_data$c <- as.factor(raw_data$c)
raw_data$e <- as.factor(raw_data$e)
raw_data$f <- as.factor(raw_data$f)

task <- TaskClassif$new('test',raw_data,'e')

test_that('Adavance pipe simple imputation ',{

  #Amelia
  expect_equal(PipeOpAmelia$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))


  #mice_A
  expect_equal(as.integer(PipeOpMice_A$new()$train(list(tsk('pima')))[[1]]$missings()),rep(0,9))

  #mice
  expect_equal(as.integer(PipeOpMice$new()$train(list(tsk('pima')))[[1]]$missings()),rep(0,9))

  #missForest
  expect_equal(PipeOpmissForest$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))

  #missRanger
  expect_equal(PipeOpmissRanger$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))

  #softImpute
  expect_equal(as.integer(PipeOpSoftImpute$new()$train(list(tsk('pima')))[[1]]$missings()),rep(0,9))

  #VIM_HD
  expect_equal(PipeOpVIM_HD$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))

  #VIM_kNN
  expect_equal(PipeOpVIM_kNN$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))

  #VIM_IRMI
  expect_equal(PipeOpVIM_IRMI$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))

  #VIM_regrImp
  expect_equal(PipeOpVIM_regrImp$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))

  #missMDA FMAD ...
  expect_equal(PipeOpMissMDA_PCA_MCA_FMAD$new(optimize_ncp = F)$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))

  #missMDA MFA
  expect_equal(PipeOpMissMDA_MFA$new()$train(list(task))[[1]]$missings(),c('e'=0,'d'=0,'a'=0,'b'=0,'c'=0,'f'=0))
})



