#' softImpute Imputation
#'
#' @description This class create object implements autotune_softImpute function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_softImpute}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines



PipeOpSoftImpute <-  R6::R6Class("softImpute_imputation",lock_objects=FALSE,
                               inherit = PipeOpImpute,  # inherit from PipeOp
                               public = list(
                                 initialize = function(id = "imput_softImpute",col_0_1=F,cat_Fun=maxCat,lambda=0,rank.max=2,type='als',thresh=1e-5,maxit=100
                                 ) {
                                   super$initialize(id, whole_task_dependent=TRUE, param_vals = list( col_0_1=col_0_1,cat_Fun=cat_Fun,lambda=lambda,
                                                                                                      rank.max=rank.max,type=type,thresh,thresh,maxit=maxit),
                                                    param_set= ParamSet$new(list(

                                                      'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='softImpute'),
                                                      'cat_Fun'=ParamUty$new('cat_Fun',default = maxCat,tags = 'softImpute'),
                                                      'lambda'=ParamUty$new('lambda',default = 0,tags = 'softImpute'),
                                                      'rank.max'=ParamUty$new('rank.max',default = 2,tags = 'softImpute'),
                                                      'type'=ParamFct$new('type',levels = c('als','svd'),default = 'als',tags='softImpute'),
                                                      'thresh'=ParamDbl$new('thresh',upper = Inf,lower = 0,default = 1e-5,tags = 'softImpute'),
                                                      'maxit'=ParamDbl$new('maxit',upper = Inf,lower = 1,default = 100,tags='softImpute')
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



                                     data_imputed <- autotune_softImpute(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                         col_0_1 = self$param_set$values$col_0_1,cat_Fun = self$param_set$values$cat_Fun,
                                                                         lambda = self$param_set$values$lambda,rank.max = self$param_set$values$rank.max,
                                                                         type = self$param_set$values$type,thresh = self$param_set$values$thresh,
                                                                         maxit = self$param_set$values$maxit)



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

                                   self$train_s <- TRUE
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



                                     data_imputed <- autotune_softImpute(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                         col_0_1 = self$param_set$values$col_0_1,cat_Fun = self$param_set$values$cat_Fun,
                                                                         lambda = self$param_set$values$lambda,rank.max = self$param_set$values$rank.max,
                                                                         type = self$param_set$values$type,thresh = self$param_set$values$thresh,
                                                                         maxit = self$param_set$values$maxit)



                                     return(data_imputed)
                                   }

                                   if (self$imputed){
                                     feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                                   }
                                   if((nrow(self$data_imputed)!=nrow(context) |  !self$train_s) & self$flag=='train'){
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
                                     self$flag <- 'predict'
                                     self$imputed_predict <- FALSE
                                   }
                                   self$train_s <- FALSE

                                   return(feature)
                                 }

                               )
)
mlr_pipeops$add("softImpute_imputation",PipeOpSoftImpute)
