#' VIM kNN Imputation
#'
#' @description This class create object implements autotune_VIM_kNN function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_VIM_kNN}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines



PipeOpVIM_kNN <-  R6::R6Class("VIM_kNN_imputation",lock_objects=FALSE,
                           inherit = PipeOpImpute,
                           public = list(
                             initialize = function(id = "imput_VIM_kNN", k=5,numFun=median,catFun=maxCat,col_0_1=FALSE
                             ) {
                               super$initialize(id, whole_task_dependent=TRUE,param_vals = list(k=k,numFun=numFun,catFun=catFun,col_0_1=col_0_1 ),
                                                param_set= ParamSet$new(list(

                                                  'k'=ParamInt$new('k',lower = 1,upper = Inf,default = 5,tags='VIM_kNN'),
                                                  'numFun'=ParamUty$new('numFun',default = median,tags = 'VIM_kNN'),
                                                  'catFun'=ParamUty$new('catFun',default = maxCat,tags = 'VIM_kNN'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = FALSE,tags = 'VIM_kNN')

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

                                 data_imputed <- autotune_VIM_kNN(data_to_impute,percent_of_missing ,k =self$param_set$values$k,numFun = self$param_set$values$numFun,
                                                                  catFun = self$param_set$values$catFun,col_0_1 = self$param_set$values$col_0_1)

                                 data_imputed <- cbind(data_imputed,target_col)
                                 colnames(data_imputed)[ncol(data_imputed)] <- input[[1]]$target_names


                                 data_backen <- as.data.frame(input[[1]]$backend$data(row=1:input[[1]]$backend$nrow,col=input[[1]]$backend$colnames))[,ifelse(input[[1]]$backend$primary_key==input[[1]]$backend$colnames,FALSE,TRUE)]
                                 data_backen[input[[1]]$row_ids,] <- data_imputed

                                 input[[1]]$backend <- as_data_backend(data_backen)


                                 return(input)
                               }




                             },
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
mlr_pipeops$add("VIM_kNN_imputation", PipeOpVIM_kNN)
