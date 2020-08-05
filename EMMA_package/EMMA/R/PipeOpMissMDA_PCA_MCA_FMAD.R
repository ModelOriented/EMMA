#' missMDA_PCA_MCA_FMAD imputation
#'
#' @description This class create object implements missMDA_FMAD_MCA_PCA function for use in mlr3 pipelinies. Object can be created with \code{\link{missMDA_FMAD_MCA_PCA}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines

PipeOpMissMDA_PCA_MCA_FMAD <-  R6::R6Class("missMDA_MCA_PCA_FMAD_imputation",lock_objects=FALSE,
                           inherit = PipeOpImpute,  # inherit from PipeOp
                           public = list(
                             initialize = function(id = "imput_missMDA_MCA_PCA_FMAD", optimize_ncp = T, set_ncp=2,col_0_1=F,ncp.max=5,random.seed=123,maxiter=1000,
                                                   coeff.ridge=1,threshold=1e-06,method='Regularized'
                             ) {
                               super$initialize(id,whole_task_dependent=TRUE, param_vals = list(optimize_ncp=optimize_ncp,set_ncp=set_ncp,col_0_1=col_0_1,ncp.max=ncp.max,random.seed=random.seed,
                                                                      maxiter=maxiter,coeff.ridge=coeff.ridge,threshold=threshold,method=method),
                                                param_set= ParamSet$new(list(

                                                  'set_ncp'=ParamInt$new('set_ncp',lower = 1,upper = Inf,default = 2,tags='PCA_MCA_FMAD'),
                                                  'ncp.max'=ParamInt$new('ncp.max',lower = 1,upper = Inf,default = 2,tags='PCA_MCA_FMAD'),
                                                  'maxiter'=ParamInt$new('maxiter',lower =50,upper = Inf,default = 1000,tags = 'PCA_MCA_FMAD'),
                                                  'coeff.ridge'=ParamDbl$new('coeff.ridge',lower = 0,upper = 1,default = 1,tags = 'PCA_MCA_FMAD'),
                                                  'threshold'=ParamDbl$new('threshold',lower = 0,upper = 1,default = 1e-6,tags = 'PCA_MCA_FMAD'),
                                                  'method'=ParamFct$new('method',levels = c('Regularized','EM'),default = 'Regularized',tags = 'PCA_MCA_FMAD'),



                                                  'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='PCA_MCA_FMAD'),
                                                  'optimize_ncp'=ParamLgl$new('optimize_ncp',default = T,tags='PCA_MCA_FMAD'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='PCA_MCA_FMAD')

                                                ))
                               )
                               self$imp_function <- function(input){

                                 col_name <- input[[1]]$backend$colnames
                                 data_to_impute <- as.data.frame(input[[1]]$data())
                                 target_col <- data_to_impute[,input[[1]]$target_names]

                                 data_to_impute <- data_to_impute[,ifelse(colnames(data_to_impute)==input[[1]]$target_names,FALSE,TRUE)]

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

                                 data_imputed <- missMDA_FMAD_MCA_PCA(data_to_impute,col_type,percent_of_missing,optimize_ncp = self$param_set$values$optimize_ncp,
                                                                      set_ncp = self$param_set$values$set_ncp,col_0_1 = self$param_set$values$col_0_1,
                                                                      ncp.max = self$param_set$values$ncp.max, random.seed = self$param_set$values$random.seed,
                                                                      maxiter =  self$param_set$values$maxiter,coeff.ridge =  self$param_set$values$coeff.ridge,
                                                                      threshold =  self$param_set$values$threshold,method =  self$param_set$values$method)

                                 data_imputed <- cbind(data_imputed,target_col)
                                 colnames(data_imputed)[ncol(data_imputed)] <- input[[1]]$target_names


                                 data_backen <- as.data.frame(input[[1]]$backend$data(row=1:input[[1]]$backend$nrow,col=input[[1]]$backend$colnames))[,ifelse(input[[1]]$backend$primary_key==input[[1]]$backend$colnames,FALSE,TRUE)]
                                 data_backen[input[[1]]$row_ids,] <- data_imputed

                                 input[[1]]$backend <- as_data_backend(data_backen)


                                 return(input)
                               }


                             },

                             # PipeOp deriving classes must implement train_internal and
                             # predict_internal; each taking an input list and returning
                             # a list as output.
                             predict_internal = function(input) {

                               p <- self$imp_function(input)



                               return(p)

                             },

                             train_internal = function(input) {

                               t<- self$imp_function(input)




                               return(t)

                             }

                           )
)


mlr_pipeops$add("missMDA_MCA_PCA_FMAD_imputation", PipeOpMissMDA_PCA_MCA_FMAD)
