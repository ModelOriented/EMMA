#' @title PipeOpmissRanger_T
#'
#' @name PipeOpmissRanger_T
#'
#' @description
#' Implements missRanger methods as mlr3 pipeline, more about missRanger \code{\link{autotune_missRanger}}.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_missRanger"}.
#' \item \code{mtry} :: \code{integer(1)}\cr
#' Sample fraction used by missRanger. This param isn't optimized automatically. If NULL default value from ranger package will be used, \code{NULL}.
#' \item \code{num.trees} :: \code{integer(1)}\cr
#' Number of trees. If optimize=TRUE, params set: seq(10, num.trees, iter) will be used, default \code{500}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#' Decides whether to add a bonus column informing where values were imputed. 0 - value was in dataset, 1 - value was imputed, default \code{FALSE}.
#' \item \code{pmm.k} :: \code{integer(1)}\cr
#' Number of candidate non-missing values to sample from in the predictive mean matching step. 0 to avoid this step. If optimize=TRUE params set: sample(1:pmm.k, iter) will be used. If pmm.k=0, missRanger is the same as missForest, default \code{5}.
#' \item \code{random.seed} :: \code{integer(1)}\cr
#' Random seed, default \code{123}.
#' \item \code{iter} :: \code{integer(1)}\cr
#' Number of iterations for a random search, default \code{10}.
#' \item \code{optimize} :: \code{logical(1)}\cr
#' If set TRUE, function will optimize parameters of imputation automatically. If parameters will be tuned by other method, should be set to FALSE, default \code{FALSE}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location. If file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export



PipeOpmissRanger_T <-  R6::R6Class("missRanger_imputation",lock_objects=FALSE,
                           inherit = PipeOpTaskPreproc,
                           public = list(
                             initialize = function(id = "imput_missRanger", maxiter = 10,random.seed=123,mtry=NULL,num.trees=500,
                                                   pmm.k=5,optimize=F,iter=10,col_0_1=F,out_file=NULL
                             ) {
                               super$initialize(id, param_vals = list( maxiter=maxiter,random.seed=random.seed,mtry=mtry,num.trees=num.trees,
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
                                 col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                 col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                 data_imputed <- autotune_missRanger(data_to_impute,percent_of_missing,maxiter = self$param_set$values$maxiter,
                                                                     random.seed = self$param_set$values$random.seed,mtry = self$param_set$values$mtry,
                                                                     num.trees = self$param_set$values$num.trees,col_0_1 = self$param_set$values$col_0_1,
                                                                     out_file = self$param_set$values$out_file,optimize = self$param_set$values$optimize,
                                                                     iter = self$param_set$values$iter,pmm.k = self$param_set$values$pmm.k)

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


                                 col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                 col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                 data_imputed <- autotune_missRanger(data_to_impute,percent_of_missing,maxiter = self$param_set$values$maxiter,
                                                                     random.seed = self$param_set$values$random.seed,mtry = self$param_set$values$mtry,
                                                                     num.trees = self$param_set$values$num.trees,col_0_1 = self$param_set$values$col_0_1,
                                                                     out_file = self$param_set$values$out_file,optimize = self$param_set$values$optimize,
                                                                     iter = self$param_set$values$iter,pmm.k = self$param_set$values$pmm.k)



                                 task$cbind(as.data.table(cbind(targer,data_imputed)))





                               }




                           )
)
mlr_pipeops$add("missRanger_imputation", PipeOpmissRanger_T)

