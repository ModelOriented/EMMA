#' @title PipeOpSoftImpute
#' @name PipeOpSoftImpute
#'
#' @description
#' Implements SoftImpute methods as mlr3 pipeline, more about SoftImpute \code{\link{autotune_softImpute}}.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_softImpute"}.
#' \item \code{lambda} :: \code{integer(1)}\cr
#' Nuclear-norm regularization parameter. If lambda=0, the algorithm reverts to "hardImpute", for which convergence is typically slower. If NULL lambda is set automatically at the highest possible value, default \code{0}.
#' \item \code{rank.max} :: \code{integer(1)}\cr
#' This param restricts the rank of the solution. If set as NULL: rank.max=min(dim(X))-1, default \code{2}.
#' \item \code{type} :: \code{character(1)}\cr
#' Two algorithms are implemented: type="svd" or the default type="als". The "svd" algorithm repeatedly computes the svd of the completed matrix, and soft thresholds its singular values. Each new soft-thresholded svd is used to re-impute the missing entries. For large matrices of class "Incomplete", the svd is achieved by an efficient form of alternating orthogonal ridge regression. The "als" algorithm uses the same alternating ridge regression, but updates the imputation at each step, leading to quite substantial speedups in some cases. The "als" approach does not currently have the same theoretical convergence guarantees as the "svd" approach, default \code{'als'}.
#' \item \code{thresh} :: \code{double(1)}\cr
#' Threshold for convergence, default \code{1e-5}
#' \item \code{maxit} :: \code{integer(1)}\cr
#' Maximum number of iterations, default \code{100}.
#' \item \code{cat_Fun} :: \code{function(){}}\cr
#' Function for aggregating the k Nearest Neighbors in case of categorical variables. It can be any function with input=not_numeric_vector and output=atomic_object, default \code{VIM::maxCat}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location. If file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export



PipeOpSoftImpute <-  R6::R6Class("softImpute_imputation",lock_objects=FALSE,
                               inherit = PipeOpImpute,  # inherit from PipeOp
                               public = list(
                                 initialize = function(id = "imput_softImpute",cat_Fun=VIM::maxCat,lambda=0,rank.max=2,type='als',thresh=1e-5,maxit=100,
                                                       out_file=NULL
                                 ) {
                                   super$initialize(id, whole_task_dependent=TRUE,packages='EMMA', param_vals = list( cat_Fun=cat_Fun,lambda=lambda,
                                                                                                      rank.max=rank.max,type=type,thresh=thresh,maxit=maxit,
                                                                                                      out_file=out_file),
                                                    param_set= ParamSet$new(list(


                                                      'cat_Fun'=ParamUty$new('cat_Fun',default = VIM::maxCat,tags = 'softImpute'),
                                                      'lambda'=ParamUty$new('lambda',default = 0,tags = 'softImpute'),
                                                      'rank.max'=ParamUty$new('rank.max',default = 2,tags = 'softImpute'),
                                                      'type'=ParamFct$new('type',levels = c('als','svd'),default = 'als',tags='softImpute'),
                                                      'thresh'=ParamDbl$new('thresh',upper = Inf,lower = 0,default = 1e-5,tags = 'softImpute'),
                                                      'maxit'=ParamDbl$new('maxit',upper = Inf,lower = 1,default = 100,tags='softImpute'),
                                                      'out_file'=ParamUty$new('out_file',default = NULL,tags = 'softImpute')
                                                    ))
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



                                     data_imputed <- EMMA::autotune_softImpute(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                        cat_Fun = self$param_set$values$cat_Fun,
                                                                         lambda = self$param_set$values$lambda,rank.max = self$param_set$values$rank.max,
                                                                         type = self$param_set$values$type,thresh = self$param_set$values$thresh,
                                                                         maxit = self$param_set$values$maxit,out_file =self$param_set$values$out_file)



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



                                     data_imputed <- EMMA::autotune_softImpute(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                     cat_Fun = self$param_set$values$cat_Fun,
                                                                         lambda = self$param_set$values$lambda,rank.max = self$param_set$values$rank.max,
                                                                         type = self$param_set$values$type,thresh = self$param_set$values$thresh,
                                                                         maxit = self$param_set$values$maxit,out_file =self$param_set$values$out_file)



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
                                     colnames(self$data_imputed)[1] <- setdiff(self$state$context_cols,colnames(context))
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

#resample(TaskClassif$new('t',as.data.frame(task$data()),'binaryClass'),graph_learner,rsmp("holdout"))
# d<- PipeOpSoftImpute$new()
# d$train(list(task))
