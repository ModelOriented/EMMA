#' @title PipeOpmissForest
#'
#' @name PipeOpmissForest
#'
#' @description
#' Implements missForest methods as mlr3 pipeline more about missForest \code{\link{autotune_missForest}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_missForest"}.
#' \item \code{cores} :: \code{integer(1)}\cr
#' Number of threads used by parallel calculations. By default approximately half of available CPU cores, default \code{NULL}.
#' \item \code{ntree_set} :: \code{integer(1)}\cr
#' Vector contains numbers of tree for grid search, default \code{c(100,200,500,1000)}.
#' \item \code{mtry_set} :: \code{integer(1)}\cr
#' Vector contains numbers of variables randomly sampled at each split, default \code{NULL}.
#' \item \code{parallel} :: \code{logical(1)}\cr
#' If TRUE parallel calculation is using, default \code{TRUE}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#' Decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed, default \code{False}.
#' \item \code{ntree} :: \code{integer(1)}\cr
#' ntree from missForest function, default \code{100}.
#' \item \code{optimize} :: \code{logical(1)}\cr
#' If set TRUE function will optimize parametrs of imputation automaticlly. If parametrs will be tune by other methode shoude be set as FALSE, default \code{FALSE}.
#' \item \code{mtry} :: \code{integer(1)}\cr
#' mtry form missforest function, default \code{NULL}.
#' \item \code{maxiter} :: \code{integer(1)}\cr
#' maxiter form missForest function, default \code{20}.
#' \item \code{maxnodes} :: \code{character(1)}\cr
#' maxnodes from missForest function, default \code{NULL}
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export

PipeOpmissForest <-  R6::R6Class("missForest_imputation",lock_objects=FALSE,
                           inherit = PipeOpImpute,  # inherit from PipeOp
                           public = list(
                             initialize = function(id = "imput_missForest", cores=NULL,ntree_set=c(100,200,500,1000),mtry_set=NULL,parallel=TRUE
                                                  ,col_0_1=FALSE,mtry=NULL,ntree=100,optimize=FALSE,maxiter=20,maxnodes=NULL,out_file=NULL
                             ) {
                               super$initialize(id,whole_task_dependent=TRUE,param_vals = list(cores =cores,ntree_set =ntree_set,mtry_set=mtry_set,parallel=parallel,
                                                                      col_0_1=col_0_1,mtry=mtry,ntree=ntree,optimize=optimize,
                                                                      maxiter=maxiter,maxnodes=maxnodes,out_file=out_file),
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
                                                  'maxnodes'=ParamUty$new('maxnodes',default = NULL,tags='missForest'),
                                                  'out_file'=ParamUty$new('out_file',default = NULL,tags = 'missForest')




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


                                 data_imputed <- autotune_missForest(data_to_impute,percent_of_missing = percent_of_missing,cores = self$param_set$values$cores,
                                                                     ntree_set = self$param_set$values$ntree_set,mtry_set = self$param_set$values$mtry_set,
                                                                     parallel = self$param_set$values$parallel,
                                                                     col_0_1 = self$param_set$values$col_0_1,optimize = self$param_set$values$optimize,
                                                                     ntree = self$param_set$values$ntree,mtry = self$param_set$values$mtry,
                                                                     maxiter=self$param_set$values$maxiter,maxnodes=self$param_set$values$maxnodes,verbose = F,
                                                                     out_file =self$param_set$values$out_file)




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


                                 data_imputed <- autotune_missForest(data_to_impute,percent_of_missing = percent_of_missing,cores = self$param_set$values$cores,
                                                                     ntree_set = self$param_set$values$ntree_set,mtry_set = self$param_set$values$mtry_set,
                                                                     parallel = self$param_set$values$parallel,
                                                                     col_0_1 = self$param_set$values$col_0_1,optimize = self$param_set$values$optimize,
                                                                     ntree = self$param_set$values$ntree,mtry = self$param_set$values$mtry,
                                                                     maxiter=self$param_set$values$maxiter,maxnodes=self$param_set$values$maxnodes,verbose = F,
                                                                     out_file =self$param_set$values$out_file)




                                 return(data_imputed)
                               }
                               if (self$imputed){
                                 feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                               }
                               if((nrow(self$data_imputed)!=nrow(context) | !self$train_s) & self$flag=='train'){
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

mlr_pipeops$add("missForest_imputation", PipeOpmissForest)



