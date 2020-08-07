#' VIM kNN Imputation
#'
#' @description This class create object implements autotune_VIM_kNN function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_VIM_kNN}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines



PipeOpVIM_kNN <-  R6::R6Class("VIM_kNN_imputation",lock_objects=FALSE,
                           inherit = PipeOpImpute,
                           public = list(
                             initialize = function(id = "imput_VIM_kNN", k=5,numFun=median,catFun=maxCat,col_0_1=FALSE
                             ) {
                               super$initialize(id, whole_task_dependent=TRUE,param_vals = list(k=k,numFun=numFun,catFun=catFun,col_0_1=col_0_1 ),
                                                param_set= ParamSet$new(list(

                                                  'k'=ParamInt$new('k',lower = 1,upper = Inf,default = 5,tags='VIM_kNN'),
                                                  'numFun'=ParamUty$new('numFun',default = median,tags = 'VIM_kNN'),
                                                  'catFun'=ParamUty$new('catFun',default = maxCat,tags = 'VIM_kNN'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = FALSE,tags = 'VIM_kNN')

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


                                 data_imputed <- autotune_VIM_kNN(data_to_impute,percent_of_missing ,k =self$param_set$values$k,numFun = self$param_set$values$numFun,
                                                                  catFun = self$param_set$values$catFun,col_0_1 = self$param_set$values$col_0_1)




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


                                 data_imputed <- autotune_VIM_kNN(data_to_impute,percent_of_missing ,k =self$param_set$values$k,numFun = self$param_set$values$numFun,
                                                                  catFun = self$param_set$values$catFun,col_0_1 = self$param_set$values$col_0_1)




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
mlr_pipeops$add("VIM_kNN_imputation", PipeOpVIM_kNN)




