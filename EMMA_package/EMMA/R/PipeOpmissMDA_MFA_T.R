#' @title PipeOpmissMDA_MFA_T
#'
#' @name PipeOpmissMDA_MFA_T
#'
#' @description
#' Implements MFA methods as mlr3 pipeline, more about MFA \code{\link{missMDA_MFA}}.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_missMDA_MFA"}.
#' \item \code{ncp} :: \code{integer(1)}\cr
#' Number of dimensions used by algorithm, default \code{2}.
#' \item \code{random.seed} :: \code{integer(1)}\cr
#' Random seed, default \code{123}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#' Decides whether to add a bonus column informing where values were imputed. 0 - value was in dataset, 1 - value was imputed, default \code{FALSE}.
#' \item \code{maxiter} :: \code{integer(1)}\cr
#' Maximal number of iteration in algorithm, default \code{998}.
#' \item \code{coeff.ridge} :: \code{integer(1)}\cr
#' Value used in \emph{Regularized} method, default \code{1}.
#' \item \code{threshold} :: \code{double(1)}\cr
#' Threshold for convergence, default \code{1e-06}.
#' \item \code{method} :: \code{character(1)}\cr
#' Method used in imputation algorithm, default \code{'Regularized'}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location. If file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export




PipeOpMissMDA_MFA_T <-  R6::R6Class("missMDA_MFAimputation",lock_objects=FALSE,
                                           inherit = PipeOpTaskPreproc,  # inherit from PipeOp
                                           public = list(
                                             initialize = function(id = "imput_missMDA_MFA",col_0_1=F,ncp=2,random.seed=123,maxiter=998,
                                                                   coeff.ridge=1,threshold=1e-06,method='Regularized',out_file=NULL
                                             ) {
                                               super$initialize(id, param_vals = list(col_0_1=col_0_1,ncp=ncp,random.seed=random.seed,
                                                                                      maxiter=maxiter,coeff.ridge=coeff.ridge,threshold=threshold,method=method,
                                                                                      out_file=out_file),
                                                                param_set= ParamSet$new(list(


                                                                  'ncp'=ParamInt$new('ncp',lower = 1,upper = Inf,default = 2,tags='MFA'),
                                                                  'maxiter'=ParamInt$new('maxiter',lower =50,upper = Inf,default = 998,tags = 'MFA'),
                                                                  'coeff.ridge'=ParamDbl$new('coeff.ridge',lower = 0,upper = 1,default = 1,tags = 'MFA'),
                                                                  'threshold'=ParamDbl$new('threshold',lower = 0,upper = 1,default = 1e-6,tags = 'MFA'),
                                                                  'method'=ParamFct$new('method',levels = c('Regularized','EM'),default = 'Regularized',tags = 'MFA'),


                                                                  'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='MFA'),
                                                                  'out_file'=ParamUty$new('out_file',default = NULL,tags = 'MFA'),

                                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='MFA')

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
                                                 col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                                 col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                                 data_imputed <- missMDA_MFA(data_to_impute,col_type,percent_of_missing,random.seed = self$param_set$values$random.seed,
                                                                             ncp = self$param_set$values$ncp,col_0_1 = self$param_set$values$col_0_1,
                                                                             maxiter =  self$param_set$values$maxiter,coeff.ridge =  self$param_set$values$coeff.ridge,
                                                                             threshold =  self$param_set$values$threshold,method =  self$param_set$values$method,
                                                                             out_file =self$param_set$values$out_file)


                                                 data_imputed <-  cbind(data_imputed,task$row_ids)
                                                 colnames(data_imputed)[ncol(data_imputed)] <- task$backend$primary_key
                                                 task$cbind(as.data.table(data_imputed))




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



                                                 data_imputed <- missMDA_MFA(data_to_impute,col_type,percent_of_missing,random.seed = self$param_set$values$random.seed,
                                                                             ncp = self$param_set$values$ncp,col_0_1 = self$param_set$values$col_0_1,
                                                                             maxiter =  self$param_set$values$maxiter,coeff.ridge =  self$param_set$values$coeff.ridge,
                                                                             threshold =  self$param_set$values$threshold,method =  self$param_set$values$method,
                                                                             out_file =self$param_set$values$out_file)


                                                data_imputed <-  cbind(data_imputed,task$row_ids)
                                                colnames(data_imputed)[ncol(data_imputed)] <- task$backend$primary_key

                                                task$cbind(as.data.table(data_imputed))





                                               }

                                           )
)

mlr_pipeops$add("missMDA_MFAimputation", PipeOpMissMDA_MFA_T)





