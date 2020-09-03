#' @title PipeOpVIM_regrImp
#' @name PipeOpVIM_regrImp
#'
#' @description
#' Implements Regression Imputation methods as mlr3 pipeline more about RI \code{\link{autotune_VIM_regrImp}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_VIM_regrImp"}.
#' \item \code{robust} :: \code{logical(1)}\cr
#' TRUE/FALSE if robust regression should be used, default \code{FALSE}.
#' \item \code{mod_cat} :: \code{logical(1)}\cr
#' TTRUE/FALSE if TRUE for categorical variables the level with the highest prediction probability is selected, otherwise it is sampled according to the probabilities, default \code{FALSE}.
#' \item \code{use_imputed} :: \code{logical(1)}\cr
#' TRUE/FALSe if TURE already imputed columns will be used to impute another, default \code{FALSE}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export



PipeOpVIM_regrImp <-  R6::R6Class("VIM_regrImp_imputation",lock_objects=FALSE,
                             inherit = PipeOpImpute,  # inherit from PipeOp
                             public = list(
                               initialize = function(id = "imput_VIM_regrImp",robust=FALSE,mod_cat=FALSE,use_imputed=FALSE,out_file=NULL
                               ) {
                                 super$initialize(id, whole_task_dependent=TRUE, param_vals = list( robust=robust,mod_cat=mod_cat,
                                                                                                    use_imputed=use_imputed,out_file=out_file),
                                                  param_set= ParamSet$new(list(


                                                    'robust'=ParamLgl$new('robust',default = F,tags = 'VIM_regrImp'),
                                                    'mod_cat'=ParamLgl$new('mod_cat',default = F,tags='VIM_regrImp'),
                                                    'use_imputed'=ParamLgl$new('use_imputed',default = F,tags = 'VIM_regrImp'),
                                                    'out_file'=ParamUty$new('out_file',default = NULL,tags = 'VIM_regrImp')


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



                                   data_imputed <- autotune_VIM_regrImp(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                       robust = self$param_set$values$robust,
                                                                        mod_cat = self$param_set$values$mod_cat , use_imputed = self$param_set$values$use_imputed,
                                                                        out_file = self$param_set$values$out_file)



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
                                   col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                   col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]



                                   data_imputed <- autotune_VIM_regrImp(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                    robust = self$param_set$values$robust,
                                                                        mod_cat = self$param_set$values$mod_cat , use_imputed = self$param_set$values$use_imputed,
                                                                        out_file = self$param_set$values$out_file)



                                   return(data_imputed)
                                 }
                                 if (self$imputed){
                                   feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                                 }
                                 if((nrow(self$data_imputed)!=nrow(context) | !self$train_s) & (self$flag=='train')){
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
mlr_pipeops$add("VIM_regrImp_imputation", PipeOpVIM_regrImp)



