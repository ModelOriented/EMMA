#' @title PipeOpVIM_regrImp_T
#' @name PipeOpVIM_regrImp_T
#'
#' @description
#' Implements Regression Imputation methods as mlr3 pipeline more about RI \code{\link{autotune_VIM_regrImp}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_VIM_regrImp"}.
#' \item \code{robust} :: \code{logical(1)}\cr
#' TRUE/FALSE if robust regression should be used, default \code{FALSE}.
#' \item \code{mod_cat} :: \code{logical(1)}\cr
#' TTRUE/FALSE if TRUE for categorical variables the level with the highest prediction probability is selected, otherwise it is sampled according to the probabilities, default \code{FALSE}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#' Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed, default \code{FALSE}.
#' \item \code{use_imputed} :: \code{logical(1)}\cr
#' TRUE/FALSe if TURE already imputed columns will be used to impute another, default \code{FALSE}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export



PipeOpVIM_regrImp_T <-  R6::R6Class("VIM_regrImp_imputation",lock_objects=FALSE,
                             inherit = PipeOpTaskPreproc,  # inherit from PipeOp
                             public = list(
                               initialize = function(id = "imput_VIM_regrImp",col_0_1= FALSE,robust=FALSE,mod_cat=FALSE,use_imputed=FALSE,out_file=NULL
                               ) {
                                 super$initialize(id,  param_vals = list( col_0_1=col_0_1,robust=robust,mod_cat=mod_cat,
                                                                                                    use_imputed=use_imputed,out_file=out_file),
                                                  param_set= ParamSet$new(list(

                                                    'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='VIM_regrImp'),
                                                    'robust'=ParamLgl$new('robust',default = F,tags = 'VIM_regrImp'),
                                                    'mod_cat'=ParamLgl$new('mod_cat',default = F,tags='VIM_regrImp'),
                                                    'use_imputed'=ParamLgl$new('use_imputed',default = F,tags = 'VIM_regrImp'),
                                                    'out_file'=ParamUty$new('out_file',default = NULL,tags = 'VIM_regrImp')


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


                                   data_imputed <- autotune_VIM_regrImp(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                        col_0_1 = self$param_set$values$col_0_1,robust = self$param_set$values$robust,
                                                                        mod_cat = self$param_set$values$mod_cat , use_imputed = self$param_set$values$use_imputed,
                                                                        out_file = self$param_set$values$out_file)


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


                                   data_imputed <- autotune_VIM_regrImp(data_to_impute,percent_of_missing = percent_of_missing,col_type = col_type,
                                                                        col_0_1 = self$param_set$values$col_0_1,robust = self$param_set$values$robust,
                                                                        mod_cat = self$param_set$values$mod_cat , use_imputed = self$param_set$values$use_imputed,
                                                                        out_file = self$param_set$values$out_file)





                                   task$cbind(as.data.table(cbind(targer,data_imputed)))





                                 }
                             )
)
mlr_pipeops$add("VIM_regrImp_imputation", PipeOpVIM_regrImp_T)


resample(task,graph_learner,rsmp("holdout"))
