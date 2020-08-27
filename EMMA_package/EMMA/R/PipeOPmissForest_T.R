#' @title PipeOpmissForest_T
#'
#' @name PipeOpmissForest_T
#'
#' @description
#' Implements missForest methods as mlr3 pipeline more about missForest \code{\link{autotune_missForest}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_missForest"}.
#' \item \code{cores} :: \code{integer(1)}\cr
#' Number of threads used by parallel calculations. If NULL approximately half of available CPU cores will be used, default \code{NULL}.
#' \item \code{ntree_set} :: \code{integer(1)}\cr
#' Vector contains numbers of tree for grid search, used only when optimize == TRUE,default \code{c(100,200,500,1000)}.
#' \item \code{mtry_set} :: \code{integer(1)}\cr
#' Vector contains numbers of variables randomly sampled at each split, used only when optimize == TRUE, default \code{NULL}.
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

PipeOpmissForest_T <-  R6::R6Class("missForest_imputation",lock_objects=FALSE,
                           inherit = PipeOpTaskPreproc,  # inherit from PipeOp
                           public = list(
                             initialize = function(id = "imput_missForest", cores=NULL,ntree_set=c(100,200,500,1000),mtry_set=NULL,parallel=TRUE
                                                  ,col_0_1=FALSE,mtry=NULL,ntree=100,optimize=FALSE,maxiter=20,maxnodes=NULL,out_file=NULL
                             ) {
                               super$initialize(id,param_vals = list(cores =cores,ntree_set =ntree_set,mtry_set=mtry_set,parallel=parallel,
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





                             }),private=list(

                               .train_task=function(task){

                                 data_to_impute <- as.data.frame( task$data(cols = task$feature_names))
                                 targer <- as.data.frame(task$data(cols = task$target_names))
                                 col_type <- 1:ncol(data_to_impute)
                                 for (i in col_type){
                                   col_type[i] <- class(data_to_impute[,i])
                                 }
                                 percent_of_missing <- 1:ncol(data_to_impute)
                                 for (i in percent_of_missing){
                                   percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                 }


                                 data_imputed <- autotune_missForest(data_to_impute,percent_of_missing = percent_of_missing,cores = self$param_set$values$cores,
                                                                     ntree_set = self$param_set$values$ntree_set,mtry_set = self$param_set$values$mtry_set,
                                                                     parallel = self$param_set$values$parallel,
                                                                     col_0_1 = self$param_set$values$col_0_1,optimize = self$param_set$values$optimize,
                                                                     ntree = self$param_set$values$ntree,mtry = self$param_set$values$mtry,
                                                                     maxiter=self$param_set$values$maxiter,maxnodes=self$param_set$values$maxnodes,verbose = F,
                                                                     out_file =self$param_set$values$out_file)

                                 task$cbind(as.data.table(cbind(targer,data_imputed)))

                               },
                               .predict_task=function(task){
                                 data_to_impute <- as.data.frame( task$data(cols = task$feature_names))
                                 targer <- as.data.frame(task$data(cols = task$target_names))
                                 col_type <- 1:ncol(data_to_impute)
                                 for (i in col_type){
                                   col_type[i] <- class(data_to_impute[,i])
                                 }
                                 percent_of_missing <- 1:ncol(data_to_impute)
                                 for (i in percent_of_missing){
                                   percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                 }



                                 data_imputed <- autotune_missForest(data_to_impute,percent_of_missing = percent_of_missing,cores = self$param_set$values$cores,
                                                                     ntree_set = self$param_set$values$ntree_set,mtry_set = self$param_set$values$mtry_set,
                                                                     parallel = self$param_set$values$parallel,
                                                                     col_0_1 = self$param_set$values$col_0_1,optimize = self$param_set$values$optimize,
                                                                     ntree = self$param_set$values$ntree,mtry = self$param_set$values$mtry,
                                                                     maxiter=self$param_set$values$maxiter,maxnodes=self$param_set$values$maxnodes,verbose = F,
                                                                     out_file =self$param_set$values$out_file)




                                 task$cbind(as.data.table(cbind(targer,data_imputed)))





                               }


                           )
)

mlr_pipeops$add("missForest_imputation", PipeOpmissForest_T)



