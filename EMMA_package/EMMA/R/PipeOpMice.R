#' Mice Imputation
#'
#' @description This class create object implements autotune_mice function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_mice}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines



PipeOpMice <-  R6::R6Class("mice_imputation",lock_objects=FALSE,
                              inherit = PipeOpImpute,
                              public = list(
                                initialize = function(id = "imput_mice", m=5,maxit=5,set_cor=0.5,
                                                      set_method='pmm',low_corr=0,up_corr=1,
                                                      methods_random=c('pmm'),iter=5,random.seed=123,optimize = F,correlation=T,col_0_1=F
                                                      ) {
                                  super$initialize(id, whole_task_dependent=TRUE,param_vals = list( m=m,maxit=maxit,set_cor=set_cor,
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
                                    'optimize'=ParamLgl$new('optimize',default = F,tags='mice'),
                                    'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='mice'),
                                    'correlation'=ParamLgl$new('correlation',default = T,tags='mice')

                                ))
                                  )



                                  self$imputed <- FALSE
                                  self$column_counter <- NULL
                                  self$data_imputed <- NULL

                                },

                                train_imputer=function(feature, type, context){
                                  imp_function <- function(data_to_impute){




                                    data_to_impute <- as.data.frame(data_to_impute)
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
                                                                  correlation = self$param_set$values$correlation,col_0_1 = self$param_set$values$col_0_1,verbose = F
                                    )





                                    return(data_imputed)
                                  }
                                  self$imputed_predict <- TRUE
                                  self$flag <- 'train'
                                  if(!self$imputed){
                                    self$column_counter <- ncol(context)+1
                                    self$imputed <- TRUE
                                    data_to_impute <- cbind(feature,context)
                                    self$data_imputed <- imp_function(data_to_impute)
                                    colnames(self$data_imputed) <- self$state$context_cols

                                  }
                                  if(self$imputed){
                                    self$column_counter <- self$column_counter -1

                                  }
                                  if  (self$column_counter==0){
                                    self$imputed <- FALSE
                                  }
                                  return(NULL)

                                },
                                impute=function(feature, type, model, context){
                                  imp_function <- function(data_to_impute){


                                    data_to_impute <- as.data.frame(data_to_impute)
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
                                                                  correlation = self$param_set$values$correlation,col_0_1 = self$param_set$values$col_0_1,verbose = F
                                    )





                                    return(data_imputed)
                                  }
                                  if (self$imputed){
                                      feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                                  }
                                    if(nrow(self$data_imputed)!=nrow(context)){
                                      self$imputed_predict <- FALSE
                                      self$flag <- 'predict'
                                    }

                                      if(!self$imputed_predict){
                                    data_to_impute <- cbind(feature,context)
                                    self$data_imputed <- imp_function(data_to_impute)
                                    colnames(self$data_imputed) <- self$state$context_cols
                                    self$imputed_predict <- TRUE
                                  }


                                  if (self$imputed_predict & self$flag=='predict' ){
                                    feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]

                                  }

                                  if(self$column_counter == 0 & self$flag=='train'){
                                    feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]
                                    self$flag=='predict'
                                    self$imputed_predict <- FALSE
                                  }

                                  return(feature)
                                }



                              )
)
mlr_pipeops$add("mice_imputation", PipeOpMice)
#
#
# test <- PipeOpMice$new()
# graph =  test %>>% learner_po
# glrn = GraphLearner$new(graph)
# glrn$train(d)
# resample(d, glrn, rsmp("cv"))
# glrn$param_set$values = list(error_train = 1)
# d<- TaskClassif$new('w',dataset,'Utility')
# d$data()
# glrn$param_set
#
# ps = ParamSet$new(list(
#   ParamInt$new("imput_mice.m",lower = 1,upper = 10)
#
# ))
#
# library("mlr3tuning")
# instance = TuningInstance$new(
#   task = d,
#   learner = glrn,
#   resampling = rsmp("holdout"),
#   measure = msr("classif.ce"),
#   param_set = ps,
#   terminator = term("evals", n_evals = 20)
# )
#
# instance
# tuner = tnr("random_search")
# tuner$tune(instance)
