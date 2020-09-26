  #' @title PipeOpMice_A
  #'
  #' @name PipeOpMice_A
  #'
  #' @description
  #' Implements mice methods as mlr3 in A approach (training imputation model on training data and used a trained model on test data).
  #'
  #' @details
  #' Code of used function was writen by \url{https://github.com/prockenschaub} more information aboute this aproche can be found here \url{https://github.com/amices/mice/issues/32}
  #'
  #' @section Input and Output Channels:
  #' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
  #'
  #'
  #' @section Parameters:
  #' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
  #' \itemize{
  #' \item \code{id} :: \code{character(1)}\cr
  #' Identifier of resulting object, default \code{"imput_mice_A"}.
  #' \item \code{m} :: \code{integer(1)}\cr
  #' Number of datasets produced by mice, default \code{5}.
  #' \item \code{maxit} :: \code{integer(1)}\cr
  #' Maximum number of iterations for mice, default \code{5}.
  #' \item \code{set_corr} :: \code{double(1)}\cr
  #' Correlation or fraction of features used when optimize=FALSE. When correlation=FALSE, it represents a fraction of case to use in imputation for each variable, default \code{0.5}.
  #' \item \code{random.seed} :: \code{integer(1)}\cr
  #' Random seed, default \code{123}.
  #' \item \code{correlation} :: \code{logical(1)}\cr
  #' If set TRUE correlation is used, if set FALSE then fraction of case, default \code{TRUE}.
  #'}
  #'
  #' @export
  PipeOpMice_A <- R6::R6Class("mice_A_imputation",lock_objects=FALSE,
                              inherit = PipeOpImpute,  # inherit from PipeOp
                              public = list(
                                initialize = function(id = "imput_mice_A",set_cor=0.5,m=5,maxit=5,random.seed=123,correlation=F,methods=NULL
                                ) {
                                  super$initialize(id,whole_task_dependent=TRUE,packages='EMMA', param_vals = list(set_cor=set_cor,methods =methods ,m=m,maxit=maxit,random.seed=random.seed,correlation=correlation),
                                                   param_set= ParamSet$new(list(


                                                     'set_cor'=ParamDbl$new('set_cor', lower = 0, upper = 1, special_vals = list(), default = 0.5, tags = 'mice'),

                                                     'm'=ParamInt$new('m',lower = 1,upper = Inf,default = 2,tags='mice'),
                                                     'maxit'=ParamInt$new('maxit',lower = 5,upper = 100,default = 5,tags='mice'),
                                                     'methods'=ParamUty$new('methods',default = NULL,tags='mice'),



                                                     'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='mice'),

                                                     'correlation'=ParamLgl$new('correlation',default = F,tags='mice')




                                                   )),

                                  )


                                  self$imputed <- FALSE
                                  self$column_counter <- NULL
                                  self$data_imputed <- NULL

                                }),private=list(

                                  .train_imputer=function(feature, type, context){
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

                                      if(self$param_set$values$correlation){
                                        model <- mice::mice(data_to_impute,method = self$param_set$values$methods,m = self$param_set$values$m,maxit = self$param_set$values$maxit,
                                                            printFlag = F,seed = self$param_set$values$random.seed,predictorMatrix =mice::quickpred(data_to_impute, mincor=self$param_set$values$set_cor,method = 'spearman'))}
                                      else{ model <- mice::mice(data_to_impute,method = self$param_set$values$methods,m = self$param_set$values$m,maxit = self$param_set$values$maxit,
                                                                printFlag = F,seed = self$param_set$values$random.seed,predictorMatrix =mice::quickpred(data_to_impute, minpuc=self$param_set$values$set_cor,method = 'spearman'))}
                                      data_imputed <- mice::complete(model)



                                      for (i in colnames(data_to_impute)[(col_type=='factor')]){

                                        if(!setequal(levels(na.omit(data_to_impute[,i])),levels(data_imputed[,i]))){

                                          levels(data_imputed[,i]) <- c(levels(na.omit(data_to_impute[,i])))
                                        }
                                      }
                                      for (i in colnames(data_imputed)[col_type=='integer']){
                                        data_imputed[,i] <- as.integer(data_imputed[,i])
                                      }



                                      return(list('data'=data_imputed
                                                  , 'model'=model))
                                    }

                                    self$imputed_predict <- TRUE
                                    self$flag <- 'train'
                                    if(!self$imputed){
                                      self$column_counter <- ncol(context)+1
                                      self$imputed <- TRUE
                                      data_to_impute <- cbind(feature,context)
                                      colnames(data_to_impute)[1] <- setdiff(self$state$context_cols,colnames(context))
                                      data_to_impute <- as.data.frame(data_to_impute)[,self$state$context_cols]
                                      function_call <-  imp_function(data_to_impute)
                                      self$data_imputed <-function_call$data
                                      self$model <- function_call$model


                                    }
                                    if(self$imputed){
                                      self$column_counter <- self$column_counter -1

                                    }
                                    if  (self$column_counter==0){
                                      self$imputed <- FALSE
                                    }
                                    self$train_s <- TRUE
                                    return(NULL)

                                  },
                                  .impute=function(feature, type, model, context){

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

                                      if(nrow(data_to_impute)==1){
                                        data_train <- mice::complete(self$model)
                                        data_train <- rbind(data_train,data_to_impute[,self$state$context_cols])

                                        data_imputed <- EMMA::mice.reuse(newdata = self$model,mids = data_train, maxit=self$param_set$values$maxit,printFlag = F)$`1`[nrow(data_train),]

                                      }else{

                                        data_imputed <- EMMA::mice.reuse(mids = self$model,newdata = data_to_impute,maxit=self$param_set$values$maxit,printFlag = F)$`1`

                                      }
                                      for (i in colnames(data_to_impute)[(col_type=='factor')]){

                                        if(!setequal(levels(na.omit(data_to_impute[,i])),levels(data_imputed[,i]))){

                                          levels(data_imputed[,i]) <- c(levels(na.omit(data_to_impute[,i])))
                                        }
                                      }
                                      for (i in colnames(data_to_impute)[col_type=='integer']){
                                        data_imputed[,i] <- as.integer(data_imputed[,i])
                                      }

                                      print(sum(is.na(data_imputed)))
                                      return(data_imputed)
                                    }
                                    if (self$imputed){

                                      feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]



                                    }
                                    if((nrow(self$data_imputed)!=nrow(context) | !self$train_s) & self$flag=='train') {
                                      self$imputed_predict <- FALSE
                                      self$flag <- 'predict'
                                    }

                                    if(!self$imputed_predict){
                                      data_to_impute <- cbind(feature,context)
                                      colnames(data_to_impute)[1] <- setdiff(self$state$context_cols,colnames(context))
                                      # its important to keep the same columns order

                                      data_to_impute <- as.data.frame(data_to_impute)[,self$state$context_cols]
                                      self$data_imputed <- imp_function(data_to_impute)

                                      self$imputed_predict <- TRUE
                                    }

                                    if (self$imputed_predict & self$flag=='predict' ){
                                      feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]

                                    }
                                    if(self$column_counter == 0 & self$flag=='train'){
                                      feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]
                                      self$flag <- 'predict'
                                      self$imputed_predict <- FALSE
                                    }
                                    self$train_s <- FALSE

                                    return(feature)
                                  }
                                )
  )

  mlr_pipeops$add("miceA_imputation", PipeOpMice_A)



