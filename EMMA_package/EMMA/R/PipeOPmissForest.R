#' missForest imputation
#'
#' @description This class create object implements autotune_missForest function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_missForest}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines

PipeOpmissForest <-  R6::R6Class("missForest_imputation",lock_objects=FALSE,
                           inherit = PipeOpImpute,  # inherit from PipeOp
                           public = list(
                             initialize = function(id = "imput_missForest", cores=NULL,ntree_set=c(100,200,500,1000),mtry_set=NULL,parallel=TRUE
                                                  ,col_0_1=FALSE,mtry=NULL,ntree=100,optimize=FALSE,maxiter=20,maxnodes=NULL
                             ) {
                               super$initialize(id,whole_task_dependent=TRUE,param_vals = list(cores =cores,ntree_set =ntree_set,mtry_set=mtry_set,parallel=parallel,
                                                                      col_0_1=col_0_1,mtry=mtry,ntree=ntree,optimize=optimize,
                                                                      maxiter=maxiter,maxnodes=maxnodes),
                                                param_set= ParamSet$new(list(
                                                  'ntree_set'=ParamUty$new('ntree_set', default = c(100,200,500,1000), tags = 'missForest'),
                                                  'cores'=ParamUty$new('cores',default = NULL,tags='missForest'),
                                                  'mtry_set'=ParamUty$new('mtry_set',default = NULL,tags='missForest'),
                                                  'parallel'=ParamLgl$new('parallel',default = TRUE,tags = 'missForest'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='missForest'),
                                                  'mtry'=ParamUty$new('mtry',default = NULL,tags='missForest'),
                                                  'ntree'=ParamInt$new('ntree',lower = 10,upper = Inf,default = 100,tags='missForest'),
                                                  'optimize'=ParamLgl$new('optimize',default = FALSE,tags='missForest'),
                                                  'maxiter'=ParamInt$new('maxiter',lower = 5,upper = Inf,default = 20,tags='missForest'),
                                                  'maxnodes'=ParamUty$new('maxnodes',default = NULL,tags='missForest')




                                                )),

                               )

                               self$imp_function <- function(data_to_impute){




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


                                 data_imputed <- autotune_missForest(data_to_impute,percent_of_missing = percent_of_missing,cores = self$param_set$values$cores,
                                                                     ntree_set = self$param_set$values$ntree_set,mtry_set = self$param_set$values$mtry_set,
                                                                     parallel = self$param_set$values$parallel,
                                                                     col_0_1 = self$param_set$values$col_0_1,optimize = self$param_set$values$optimize,
                                                                     ntree = self$param_set$values$ntree,mtry = self$param_set$values$mtry,
                                                                     maxiter=self$param_set$values$maxiter,maxnodes=self$param_set$values$maxnodes,verbose = F)




                                 return(data_imputed)
                               }

                               self$imputed <- FALSE
                               self$column_counter <- NULL
                               self$data_imputed <- NULL

                             },

                             train_imputer=function(feature, type, context){

                               self$imputed_predict <- TRUE
                               self$flag <- 'train'
                               if(!self$imputed){

                                 self$column_counter <- ncol(context)+1
                                 self$imputed <- TRUE
                                 data_to_impute <- cbind(feature,context)
                                 self$data_imputed <- self$imp_function(data_to_impute)
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

                               if (self$imputed){
                                 feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                               }
                               if(nrow(self$data_imputed)!=nrow(context)){
                                 self$imputed_predict <- FALSE
                                 self$flag <- 'predict'
                               }

                               if(!self$imputed_predict){

                                 data_to_impute <- cbind(feature,context)
                                 self$data_imputed <- self$imp_function(data_to_impute)
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

mlr_pipeops$add("missForest_imputation", PipeOpmissForest)

