#' @title PipeOpVIM_kNN_T
#' @name PipeOpVIM_kNN_T
#'
#' @description
#' Implements KNN methods as mlr3 pipeline more about VIM_KNN \code{\link{autotune_VIM_kNN}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_VIM_kNN"}.
#' \item \code{k} :: \code{intiger(1)}\cr
#' Threshold for convergency, default \code{5}.
#' \item \code{numFUN} :: \code{function(){}}\cr
#' Function for aggregating the k Nearest Neighbours in the case of a numerical variable, default \code{media}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#' Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed, default \code{FALSE}.
#' \item \code{catFUN} :: \code{function(){}}\cr
#' Function for aggregating the k Nearest Neighbours in the case of a categorical variable, default \code{VIM::maxCat}
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export


PipeOpVIM_kNN_T <-  R6::R6Class("VIM_kNN_imputation",lock_objects=FALSE,
                           inherit = PipeOpTaskPreproc,
                           public = list(
                             initialize = function(id = "imput_VIM_kNN", k=5,numFun=median,catFun=VIM::maxCat,col_0_1=FALSE,out_file=NULL
                             ) {
                               super$initialize(id, param_vals = list(k=k,numFun=numFun,catFun=catFun,col_0_1=col_0_1,out_file=out_file ),
                                                param_set= ParamSet$new(list(

                                                  'k'=ParamInt$new('k',lower = 1,upper = Inf,default = 5,tags='VIM_kNN'),
                                                  'numFun'=ParamUty$new('numFun',default = median,tags = 'VIM_kNN'),
                                                  'catFun'=ParamUty$new('catFun',default = VIM::maxCat,tags = 'VIM_kNN'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = FALSE,tags = 'VIM_kNN'),
                                                  'out_file'=ParamUty$new('out_file',default = NULL,tags = 'VIM_kNN')

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

                                 data_imputed <- autotune_VIM_kNN(data_to_impute,percent_of_missing ,k =self$param_set$values$k,numFun = self$param_set$values$numFun,
                                                                  catFun = self$param_set$values$catFun,col_0_1 = self$param_set$values$col_0_1,out_file =self$param_set$values$out_file)

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

                                 data_imputed <- autotune_VIM_kNN(data_to_impute,percent_of_missing ,k =self$param_set$values$k,numFun = self$param_set$values$numFun,
                                                                  catFun = self$param_set$values$catFun,col_0_1 = self$param_set$values$col_0_1,out_file =self$param_set$values$out_file)





                                 task$cbind(as.data.table(cbind(targer,data_imputed)))





                               }
                           )
)
mlr_pipeops$add("VIM_kNN_imputation", PipeOpVIM_kNN_T)



