#' @title PipeOpmissMDA_PCA_MCA_FMAD_T
#'
#' @name PipeOpmissMDA_PCA_MCA_FMAD_T
#'
#' @description
#' Implements PCA, MCA, FMAD methods as mlr3 pipeline more about methods \code{\link{missMDA_FMAD_MCA_PCA}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_missMDA_MCA_PCA_FMAD"}.
#' \item \code{optimize_ncp} :: \code{logical(1)}\cr
#' If true number of dimensions used to predict the missing entries will be optimized. If False by default ncp = 2 it's used ,default \code{TRUE}.
#' \item \code{set_ncp} :: \code{integer(1)}\cr
#' integer >0. Number of dimensions used by algortims. Used only if optimize_ncp = Flase, default \code{2}.
#' \item \code{col_0_1} :: \code{logical(1)}\cr
#' Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed, default \code{FALSE}.
#' \item \code{ncp.max} :: \code{integer(1)}\cr
#' Number corresponding to the maximum number of components to test, default \code{5}.
#' \item \code{random.seed} :: \code{integer(1)}\cr
#' Random seed, default \code{123}.
#' \item \code{maxiter} :: \code{integer(1)}\cr
#' Maximal number of iteration in algortihm, default \code{998}.
#' \item \code{coeff.ridge} :: \code{double(1)}\cr
#' Value use in Regularized method, default \code{1}.
#' \item \code{threshold} :: \code{double(1)}\cr
#' Threshold for convergence, default \code{1e-6}.
#' \item \code{method} :: \code{character(1)}\cr
#' Method used in imputation algoritm, default \code{'Regularized'}
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location if file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#'}
#'
#' @export

PipeOpMissMDA_PCA_MCA_FMAD_T <-  R6::R6Class("missMDA_MCA_PCA_FMAD_imputation",lock_objects=FALSE,
                           inherit = PipeOpTaskPreproc,  # inherit from PipeOp
                           public = list(
                             initialize = function(id = "imput_missMDA_MCA_PCA_FMAD", optimize_ncp = T, set_ncp=2,col_0_1=F,ncp.max=5,random.seed=123,maxiter=998,
                                                   coeff.ridge=1,threshold=1e-06,method='Regularized',out_file=NULL
                             ) {
                               super$initialize(id, param_vals = list(optimize_ncp=optimize_ncp,set_ncp=set_ncp,col_0_1=col_0_1,ncp.max=ncp.max,random.seed=random.seed,
                                                                      maxiter=maxiter,coeff.ridge=coeff.ridge,threshold=threshold,method=method,out_file=out_file),
                                                param_set= ParamSet$new(list(

                                                  'set_ncp'=ParamInt$new('set_ncp',lower = 1,upper = Inf,default = 2,tags='PCA_MCA_FMAD'),
                                                  'ncp.max'=ParamInt$new('ncp.max',lower = 1,upper = Inf,default = 2,tags='PCA_MCA_FMAD'),
                                                  'maxiter'=ParamInt$new('maxiter',lower =50,upper = Inf,default = 998,tags = 'PCA_MCA_FMAD'),
                                                  'coeff.ridge'=ParamDbl$new('coeff.ridge',lower = 0,upper = 1,default = 1,tags = 'PCA_MCA_FMAD'),
                                                  'threshold'=ParamDbl$new('threshold',lower = 0,upper = 1,default = 1e-6,tags = 'PCA_MCA_FMAD'),
                                                  'method'=ParamFct$new('method',levels = c('Regularized','EM'),default = 'Regularized',tags = 'PCA_MCA_FMAD'),
                                                  'out_file'=ParamUty$new('out_file',default = NULL,tags = 'PCA_MCA_FMAD'),



                                                  'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='PCA_MCA_FMAD'),
                                                  'optimize_ncp'=ParamLgl$new('optimize_ncp',default = T,tags='PCA_MCA_FMAD'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='PCA_MCA_FMAD')

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

                                 data_imputed <- missMDA_FMAD_MCA_PCA(data_to_impute,col_type,percent_of_missing,optimize_ncp = self$param_set$values$optimize_ncp,
                                                                      set_ncp = self$param_set$values$set_ncp,col_0_1 = self$param_set$values$col_0_1,
                                                                      ncp.max = self$param_set$values$ncp.max, random.seed = self$param_set$values$random.seed,
                                                                      maxiter =  self$param_set$values$maxiter,coeff.ridge =  self$param_set$values$coeff.ridge,
                                                                      threshold =  self$param_set$values$threshold,method =  self$param_set$values$method,
                                                                      out_file =self$param_set$values$out_file )


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
                                 data_imputed <- missMDA_FMAD_MCA_PCA(data_to_impute,col_type,percent_of_missing,optimize_ncp = self$param_set$values$optimize_ncp,
                                                                      set_ncp = self$param_set$values$set_ncp,col_0_1 = self$param_set$values$col_0_1,
                                                                      ncp.max = self$param_set$values$ncp.max, random.seed = self$param_set$values$random.seed,
                                                                      maxiter =  self$param_set$values$maxiter,coeff.ridge =  self$param_set$values$coeff.ridge,
                                                                      threshold =  self$param_set$values$threshold,method =  self$param_set$values$method,
                                                                      out_file =self$param_set$values$out_file )





                                 task$cbind(as.data.table(cbind(targer,data_imputed)))





                               }
                           )
)


mlr_pipeops$add("missMDA_MCA_PCA_FMAD_imputation", PipeOpMissMDA_PCA_MCA_FMAD_T)


# test <- PipeOpMissMDA_PCA_MCA_FMAD$new()
# graph =  test %>>% learner_po
# glrn = GraphLearner$new(graph)

# resample(test_task,glrn,rsmp('cv',folds=2L))

