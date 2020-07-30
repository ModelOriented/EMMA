#' Mice Imputation
#'
#' @description This class create object implements autotune_mice function for use in mlr3 pipelinies.
#' @import mlr3
#' @import mlr3pipelines
#'
#' @param ... all params are from autotune_mice function.


PipeOpMice <-  R6::R6Class("mice_imputation",lock_objects=FALSE,
                              inherit = PipeOp,  # inherit from PipeOp
                              public = list(
                                initialize = function(id = "imput_mice", m=5,maxit=5,set_cor=0.5,
                                                      set_method='pmm',low_corr=0,up_corr=1,
                                                      methods_random=c('pmm'),iter=5,random.seed=123,optimize = T,correlation=T,col_0_1=F
                                                      ) {
                                  super$initialize(id, param_vals = list( m=m,maxit=maxit,set_cor=set_cor,
                                                                          set_method=set_method,low_corr=low_corr,up_corr=up_corr,
                                                                          methods_random=methods_random,iter=iter,random.seed=random.seed,optimize = optimize,correlation=correlation,col_0_1=col_0_1),
                                                   param_set= ParamSet$new(list(
                                    'set_cor'=ParamDbl$new('set_cor', lower = 0, upper = 1, special_vals = list(), default = 0.4, tags = 'mice'),
                                    'iter'=ParamInt$new('iter',lower = 1,upper = Inf,default = 5,tags='mice'),
                                    'm'=ParamInt$new('m',lower = 1,upper = Inf,default = 2,tags='mice'),
                                    'maxit'=ParamInt$new('maxit',lower = 5,upper = 100,default = 5,tags='mice'),
                                    'set_method'=ParamFct$new('set_method',levels = c('pmm','midastouch','sample','cart'),default = 'pmm',tags='mice'),
                                    'low_corr'=ParamDbl$new('low_corr',lower = 0,upper = 1,default = 0,tags='mice'),
                                    'up_corr'=ParamDbl$new('up_corr',lower = 0,upper = 1,default = 1,tags='mice'),
                                    'methods_random'=ParamFct$new('methods_random',levels=c('pmm','midastouch','sample','cart'),default = c('pmm'),tag='mice'),
                                    'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='mice'),
                                    'optimize'=ParamLgl$new('optimize',default = T,tags='mice'),
                                    'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='mice'),
                                    'correlation'=ParamLgl$new('correlation',default = T,tags='mice')

                                )),
                                                   # declare "input" and "output" during construction here
                                                   # training and prediction take task and return task with imputed data ;
                                                   input = data.table::data.table(name = "input",
                                                                                  train = "Task", predict = 'Task'),
                                                   output = data.table::data.table(name = "output",
                                                                                   train = "Task", predict = "Task")
                                  )

                                },

                                # PipeOp deriving classes must implement train_internal and
                                # predict_internal; each taking an input list and returning
                                # a list as output.
                                train_internal = function(input) {

                                  data_to_impute <- as.data.frame(input[[1]]$data())
                                  target_col <- data_to_impute[,input[[1]]$target_names]

                                  data_to_impute <- data_to_impute[,ifelse(colnames(data_to_impute)==input[[1]]$target_names,FALSE,TRUE)]

                                  # prepering arguments for function
                                  col_type <- 1:ncol(data_to_impute)
                                  for (i in col_type){
                                    col_type[i] <- class(data_to_impute[,i])
                                  }
                                  percent_of_missing <- 1:ncol(data_to_impute)
                                  for (i in percent_of_missing){
                                    percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                  }
                                  col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                  col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                  data_imputed <- autotune_mice(data_to_impute,col_miss = col_miss,col_no_miss = col_no_miss,col_type = col_type,
                                                                  percent_of_missing = percent_of_missing,m=self$param_set$values$m,iter=self$param_set$values$iter,
                                                                maxit = self$param_set$values$maxit,
                                                                  low_corr = self$param_set$values$low_corr,up_corr = self$param_set$values$up_corr,
                                                                set_cor = self$param_set$values$set_cor,set_method = self$param_set$values$set_method,
                                                                methods_random = self$param_set$values$methods_random,random.seed = self$param_set$values$random.seed,
                                                                optimize = self$param_set$values$optimize,
                                                                correlation = self$param_set$values$correlation,col_0_1 = self$param_set$values$col_0_1
                                                                )## Zastąp funkcją

                                  data_imputed <- cbind(data_imputed,target_col)
                                  colnames(data_imputed)[ncol(data_imputed)] <- input[[1]]$target_names


                                  if (input[[1]]$task_type=='classif'){
                                  input[[1]] <- TaskClassif$new(input[[1]]$id,data_imputed,input[[1]]$target_names,positive =input[[1]]$positive )
                                  }
                                  if ( input[[1]]$task_type=='regr'){
                                  input[[1]] <- TaskRegrf$new(input[[1]]$id,data_imputed,input[[1]]$target_names,positive =input[[1]]$positive )
                                  }
                                  return(input)
                                },

                                predict_internal = function(input) {
                                  data_to_impute <- as.data.frame(input[[1]]$data())
                                  target_col <- data_to_impute[,input[[1]]$target_names]
                                  data_to_impute <- data_to_impute[,ifelse(colnames(data_to_impute)==input[[1]]$target_names,FALSE,TRUE)]
                                  # prepering arguments for function
                                  col_type <- 1:ncol(data_to_impute)
                                  for (i in col_type){
                                    col_type[i] <- class(data_to_impute[,i])
                                  }
                                  percent_of_missing <- 1:ncol(data_to_impute)
                                  for (i in percent_of_missing){
                                    percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                  }
                                  col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                  col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]


                                  data_imputed <- autotune_mice(data_to_impute,col_miss = col_miss,col_no_miss = col_no_miss,col_type = col_type,
                                                                percent_of_missing = percent_of_missing,m=self$param_set$values$m,iter=self$param_set$values$iter,
                                                                maxit = self$param_set$values$maxit,
                                                                low_corr = self$param_set$values$low_corr,up_corr = self$param_set$values$up_corr,
                                                                set_cor = self$param_set$values$set_cor,set_method = self$param_set$values$set_method,
                                                                methods_random = self$param_set$values$methods_random,random.seed = self$param_set$values$random.seed,
                                                                optimize = self$param_set$values$optimize,
                                                                correlation = self$param_set$values$correlation,col_0_1 = self$param_set$values$col_0_1
                                  )## Zastąp funkcją

                                  data_imputed <- cbind(data_imputed,target_col)
                                  colnames(data_imputed)[ncol(data_imputed)] <- input[[1]]$target_names

                                  if (input[[1]]$task_type=='classif'){
                                    input[[1]] <- TaskClassif$new(input[[1]]$id,data_imputed,input[[1]]$target_names,positive =input[[1]]$positive )
                                  }
                                  if ( input[[1]]$task_type=='regr'){
                                    input[[1]] <- TaskRegrf$new(input[[1]]$id,data_imputed,input[[1]]$target_names,positive =input[[1]]$positive )
                                  }

                                  return(input)
                                }
                              )
)
# posum = PipeOpMice$new(m = 20)
#
#
#
# graph = posum%>>% learner_po
# glrn1 = GraphLearner$new(graph)
# glrn1$train(TaskClassif$new('test',test_data,'Class'))

#glrn1$param_set$values

#### TESTING TIME  ####
# d<- ParamSet$new(list('set_corr'=ParamDbl$new('set_corr', lower = 0, upper = 1, special_vals = list(), default = 0.4, tags = '')))
#
#
#
# s<- ParamDbl$new('set_corr', lower = 0, upper = 1, special_vals = list(), default = 0.4, tags = '')
# d
# d$is_empty
#
# ps = ParamSet$new(list(
#   ParamInt$new("imput_mice.m", lower = 5, upper = 12),
#   ParamInt$new("imput_mice.iter", lower = 1, upper = 3)
# ))
#
# hout = rsmp("holdout")
# measure = msr("classif.ce")
# evals20 = term("evals", n_evals = 20)
#
# instance = TuningInstance$new(
#   task = test_taks,
#   learner = glrn1,
#   resampling = hout,
#   measure = measure,
#   param_set = ps,
#   terminator = evals20
# )
# tuner = tnr("grid_search", resolution = 5)
# tuner$tune(instance)
