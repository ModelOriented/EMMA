#' Amelia imputation
#'
#' @description This class create object implements autotune_Amelia function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_Amelia}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines

PipeOpAmelia <-  R6::R6Class("Amelia_imputation",lock_objects=FALSE,
                                 inherit = PipeOpImpute,  # inherit from PipeOp
                                 public = list(
                                   initialize = function(id = "imput_Amelia", col_0_1=FALSE,polytime=NULL,splinetime=NULL,intercs=FALSE,empir=NULL,m=3,parallel=TRUE
                                   ) {
                                     super$initialize(id, whole_task_dependent=TRUE,param_vals = list(col_0_1=col_0_1,polytime=polytime,splinetime=splinetime,intercs=intercs,empir=empir,m=m,parallel=parallel),
                                                      param_set= ParamSet$new(list(
                                                        'polytime'=ParamUty$new('polytime', default = NULL, tags = 'amelia'),
                                                        'splinetime'=ParamUty$new('splinetime',default = NULL,tags='amelia'),
                                                        'empir'=ParamUty$new('empir',default = NULL,tags='amelia'),
                                                        'parallel'=ParamLgl$new('parallel',default = TRUE,tags = 'amelia'),
                                                        'intercs'=ParamLgl$new('intercs',default = FALSE,tags='amelia'),
                                                        'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='amelia'),
                                                        'm'=ParamInt$new('m',lower = 1,upper = Inf,default = 3,tags='amelia')





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

                                       data_imputed <- autotune_Amelia(data_to_impute,col_type,percent_of_missing,col_0_1 = self$param_set$values$col_0_1,
                                                                       parallel = self$param_set$values$parallel,polytime = self$param_set$values$polytime,
                                                                       splinetime = self$param_set$values$splinetime, intercs = self$param_set$values$intercs,
                                                                       empir = self$param_set$values$empir,m=self$param_set$values$m)

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

mlr_pipeops$add("Amelia_imputation", PipeOpAmelia)
