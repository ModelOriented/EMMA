#' @title PipeOpSoftImpute_T
#' @name PipeOpSoftImpute_T
#'
#' @description
#' Implements SoftImpute methods as mlr3 pipeline more about SoftImpute \code{\link{autotune_softImpute}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_softImpute"}.
#' \item \code{lambda} :: \code{integer(1)}\cr
#' nuclear-norm regularization parameter. If lambda=0, the algorithm reverts to "hardImpute", for which convergence is typically slower. If null lambda is set automatically at the highest possible values, default \code{0}.
#' \item \code{rank.max} :: \code{integer(1)}\cr
#' This restricts the rank of the solution. Defoult 2 if set as NULL rank.max=min(dim(X))-1, default \code{2}.
#' \item \code{type} :: \code{character(1)}\cr
#' Chose of algoritm 'als' or 'svd', defoult \code{'als'}.
#' \item \code{thresh} :: \code{double(1)}\cr
#' Threshold for convergence, default \code{1e-5}
#' \item \code{maxit} :: \code{integer(1)}\cr
#' Maximum number of iterations, default \code{100}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#' Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed, default \code{FALSE}.
#' \item \code{cat_Fun} :: \code{function(){}}\cr
#' Function for aggregating the k Nearest Neighbours in the case of a categorical variable, default \code{VIM::maxCat}
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export




PipeOpSoftImpute_T <-  R6::R6Class("softImpute_imputation",lock_objects=FALSE,
                               inherit = PipeOpTaskPreproc,  # inherit from PipeOp
                               public = list(
                                 initialize = function(id = "imput_softImpute",col_0_1=F,cat_Fun=VIM::maxCat,lambda=0,rank.max=2,type='als',thresh=1e-5,maxit=100,
                                                       out_file=NULL
                                 ) {
                                   super$initialize(id,  param_vals = list( col_0_1=col_0_1,cat_Fun=cat_Fun,lambda=lambda,
                                                                                                      rank.max=rank.max,type=type,thresh=thresh,maxit=maxit,
                                                                                                      out_file=out_file),
                                                    param_set= ParamSet$new(list(

                                                      'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='softImpute'),
                                                      'cat_Fun'=ParamUty$new('cat_Fun',default = VIM::maxCat,tags = 'softImpute'),
                                                      'lambda'=ParamUty$new('lambda',default = 0,tags = 'softImpute'),
                                                      'rank.max'=ParamUty$new('rank.max',default = 2,tags = 'softImpute'),
                                                      'type'=ParamFct$new('type',levels = c('als','svd'),default = 'als',tags='softImpute'),
                                                      'thresh'=ParamDbl$new('thresh',upper = Inf,lower = 0,default = 1e-5,tags = 'softImpute'),
                                                      'maxit'=ParamDbl$new('maxit',upper = Inf,lower = 1,default = 100,tags='softImpute'),
                                                      'out_file'=ParamUty$new('out_file',default = NULL,tags = 'softImpute')
                                                    ))
                                   )



                                  
                                 }),private=list(

                                   .train_task=function(task){
                                     
                                     data_to_impute =as.data.frame( task$data())
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
                                                                         maxit = self$param_set$values$maxit,out_file =self$param_set$values$out_file)
                                     
                                     task$cbind(as.data.table(data_imputed))
                                     
                                   },
                                   .predict_task=function(task){
                                     data_to_impute =as.data.frame( task$data())
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
                                                                         maxit = self$param_set$values$maxit,out_file =self$param_set$values$out_file)
                                     
                                     
                                     
                                     task$cbind(as.data.table(data_imputed))
                                     
                                     
                                     
                                     
                                     
                                   }
                               )
)
mlr_pipeops$add("softImpute_imputation",PipeOpSoftImpute_T)

