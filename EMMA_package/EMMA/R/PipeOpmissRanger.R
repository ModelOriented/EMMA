#' missRanger Imputation
#'
#' @description This class create object implements autotune_missRanger function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_missRanger}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines



PipeOpmissRanger <-  R6::R6Class("missRanger_imputation",lock_objects=FALSE,
                           inherit = PipeOpImpute,
                           public = list(
                             initialize = function(id = "imput_missRanger", maxiter = 10,random.seed=123,mtry=NULL,num.trees=500,
                                                   pmm.k=5,optimize=F,iter=10,col_0_1=F,out_file=NULL
                             ) {
                               super$initialize(id, whole_task_dependent=TRUE,param_vals = list( maxiter=maxiter,random.seed=random.seed,mtry=mtry,num.trees=num.trees,
                                                                                                 pmm.k=pmm.k,iter=iter,optimize=optimize,col_0_1=col_0_1,out_file=out_file),
                                                param_set= ParamSet$new(list(
                                                  'maxiter'= ParamInt$new('maxiter',lower = 1,upper = Inf,default = 10,tags = 'missRanger'),
                                                  'random.seed'=ParamInt$new('random.seed',default = 123,tags = 'missRanger'),
                                                  'mtry'=ParamUty$new('mtry',default = NULL,tags = 'missRanger'),
                                                  'num.trees'=ParamInt$new('num.trees',default = 500,lower = 10,upper = Inf,tags = 'missRanger'),
                                                  'pmm.k'=ParamInt$new('pmm.k',lower = 0,upper = Inf,default = 5,tags = 'missRagner'),
                                                  'optimize'=ParamLgl$new('optimize',default = F,tags = 'missRagner'),
                                                  'iter'=ParamInt$new('iter',lower = 1,upper = Inf,default = 10,tags = 'missRanger'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags = 'missRanger'),
                                                  'out_file'=ParamUty$new('out_file',default = NULL,tags = 'missRanger')

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

                                 data_imputed <- autotune_missRanger(data_to_impute,percent_of_missing,maxiter = self$param_set$values$maxiter,
                                                                     random.seed = self$param_set$values$random.seed,mtry = self$param_set$values$mtry,
                                                                     num.trees = self$param_set$values$num.trees,col_0_1 = self$param_set$values$col_0_1,
                                                                     out_file = self$param_set$values$out_file,optimize = self$param_set$values$optimize,
                                                                     iter = self$param_set$values$iter,pmm.k = self$param_set$values$pmm.k)






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

                                 data_imputed <- autotune_missRanger(data_to_impute,percent_of_missing,maxiter = self$param_set$values$maxiter,
                                                                     random.seed = self$param_set$values$random.seed,mtry = self$param_set$values$mtry,
                                                                     num.trees = self$param_set$values$num.trees,col_0_1 = self$param_set$values$col_0_1,
                                                                     out_file = self$param_set$values$out_file,optimize = self$param_set$values$optimize,
                                                                     iter = self$param_set$values$iter,pmm.k = self$param_set$values$pmm.k)




                                 return(data_imputed)
                               }
                               if (self$imputed){
                                 feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                               }
                               if((nrow(self$data_imputed)!=nrow(context) | !self$train_s ) & self$flag=='tarin'){
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
                                 self$flag=='predict'
                                 self$imputed_predict <- FALSE
                               }
                               self$train_s <- FALSE

                               return(feature)
                             }



                           )
)
mlr_pipeops$add("missRanger_imputation", PipeOpmissRanger)
