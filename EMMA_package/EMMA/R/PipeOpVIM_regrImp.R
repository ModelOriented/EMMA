#' Regression Imputation
#'
#' @description This class create object implements autotune_VIM_regrImp function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_VIM_regrImp}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines



PipeOpVIM_regrImp <-  R6::R6Class("VIM_regrImp_imputation",lock_objects=FALSE,
                             inherit = PipeOpImpute,  # inherit from PipeOp
                             public = list(
                               initialize = function(id = "imput_VIM_regrImp",col_0_1= FALSE,robust=FALSE,mod_cat=FALSE,use_imputed=FALSE
                               ) {
                                 super$initialize(id, whole_task_dependent=TRUE, param_vals = list( col_0_1=col_0_1,robust=robust,mod_cat=mod_cat,
                                                                                                    use_imputed=use_imputed),
                                                  param_set= ParamSet$new(list(

                                                    'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='VIM_regrImp'),
                                                    'robust'=ParamLgl$new('robust',default = F,tags = 'VIM_regrImp'),
                                                    'mod_cat'=ParamLgl$new('mod_cat',default = F,tags='VIM_regrImp'),
                                                    'use_imputed'=ParamLgl$new('use_imputed',default = F,tags = 'VIM_regrImp')


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



                                   data_imputed <- autotune_VIM_regrImp(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                        col_0_1 = self$param_set$values$col_0_1,robust = self$param_set$values$robust,
                                                                        mod_cat = self$param_set$values$mod_cat , use_imputed = self$param_set$values$use_imputed)



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
                                   col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                   col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]



                                   data_imputed <- autotune_VIM_regrImp(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                        col_0_1 = self$param_set$values$col_0_1,robust = self$param_set$values$robust,
                                                                        mod_cat = self$param_set$values$mod_cat , use_imputed = self$param_set$values$use_imputed)



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
mlr_pipeops$add("VIM_regrImp_imputation", PipeOpVIM_regrImp)
