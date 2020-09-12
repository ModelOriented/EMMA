PipeOpmice_A <-  R6::R6Class("mice_A_imputation",lock_objects=FALSE,
                                  inherit = PipeOpImpute,  # inherit from PipeOp
                                  public = list(
                                    initialize = function(id = "imput_mice_A",set_cor=0.5,m=5,maxit=5,random.seed=123,correlation=F
                                    ) {
                                      super$initialize(id,whole_task_dependent=TRUE, param_vals = list(set_cor=set_cor,m=m,maxit=maxit,random.seed=random.seed,correlation=correlation),
                                                       param_set= ParamSet$new(list(


                                                         'set_cor'=ParamDbl$new('set_cor', lower = 0, upper = 1, special_vals = list(), default = 0.5, tags = 'mice'),

                                                         'm'=ParamInt$new('m',lower = 1,upper = Inf,default = 2,tags='mice'),
                                                         'maxit'=ParamInt$new('maxit',lower = 5,upper = 100,default = 5,tags='mice'),



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
                                          model <- mice::mice(data_to_impute,m = self$param_set$values$m,maxit = self$param_set$values$maxit,
                                                              printFlag = F,seed = self$param_set$values$random.seed,predictorMatrix =mice::quickpred(data_to_impute, mincor=self$param_set$values$set_cor,method = 'spearman'))}
                                          else{ model <- mice::mice(data_to_impute,m = self$param_set$values$m,maxit = self$param_set$values$maxit,
                                                                    printFlag = F,seed = self$param_set$values$random.seed,predictorMatrix=mice::quickpred(data_to_impute, minpuc=self$param_set$values$set_cor,method = 'spearman'))}
                                          data_imputed <- mice::complete(model)





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

                                          data_imputed <- mice.reuse(self$model,data_to_impute,maxit=self$param_set$values$maxit,printFlag = F)$`1`





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

mlr_pipeops$add("miceA_imputation", PipeOpmice_A)


# w <- PipeOpmice_A$new()
# gr <- w %>>% lrn('classif.rpart')
# gr <- GraphLearner$new(gr)
#
# resample(task,gr,rsmp('cv'))
#
# library(testthat)

#w <- mice::mice(test[-sap,])


#d <- mice.reuse(w,test[sap,]

