#' @title PipeOpVIM_HD_T
#'
#' @name PipeOpVIM_HD_T
#'
#' @description
#' Implements Hot Deck methods as mlr3 pipeline more about VIM_HD \code{\link{autotune_VIM_hotdeck}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_VIM_HD"}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export



PipeOpVIM_HD_T <-  R6::R6Class("VIM_HD_imputation",lock_objects=FALSE,
                           inherit = PipeOpTaskPreproc,  # inherit from PipeOp
                           public = list(
                             initialize = function(id = "imput_VIM_HD",col_0_1= FALSE,out_file=NULL
                             ) {
                               super$initialize(id,  param_vals = list( col_0_1=col_0_1,out_file=out_file),
                                                param_set= ParamSet$new(list(

                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='VIM_HD'),
                                                  'out_file'=ParamUty$new('out_file',default = NULL,tags = 'VIM_HD')


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


                                 data_imputed <- autotune_VIM_hotdeck(data_to_impute,percent_of_missing,self$param_set$values$col_0_1,
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




                                 data_imputed <- autotune_VIM_hotdeck(data_to_impute,percent_of_missing,self$param_set$values$col_0_1,
                                                                      out_file =self$param_set$values$out_file)



                                 task$cbind(as.data.table(cbind(targer,data_imputed)))





                               }
                           )
)
mlr_pipeops$add("VIM_HD_imputation", PipeOpVIM_HD_T)



