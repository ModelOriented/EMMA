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



                                     self$imputed <- FALSE
                                     self$column_counter <- NULL
                                     self$data_imputed <- NULL

                                   },

                                   train_imputer=function(feature, type, context){
                                      imp_function <- function(data_to_impute){




                                       data_to_impute <- as.data.frame(data_to_impute)
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





                                       return(data_imputed)
                                     }
                                     self$imputed_predict <- TRUE
                                     self$flag <- 'train'
                                     if(!self$imputed){
                                       self$column_counter <- ncol(context)+1
                                       self$imputed <- TRUE
                                       data_to_impute <- cbind(feature,context)
                                       self$data_imputed <- imp_function(data_to_impute)
                                       colnames(self$data_imputed) <- self$state$context_cols

                                     }
                                     if(self$imputed){
                                       self$column_counter <- self$column_counter -1

                                     }
                                     if  (self$column_counter==0){
                                       self$imputed <- FALSE
                                     }
                                     return(NULL)

                                   },
                                   impute=function(feature, type, model, context){
                                      imp_function <- function(data_to_impute){




                                       data_to_impute <- as.data.frame(data_to_impute)
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





                                       return(data_imputed)
                                     }
                                     if (self$imputed){
                                       feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]


                                     }
                                     if(nrow(self$data_imputed)!=nrow(context)){
                                       self$imputed_predict <- FALSE
                                       self$flag <- 'predict'
                                     }

                                     if(!self$imputed_predict){
                                       data_to_impute <- cbind(feature,context)
                                       self$data_imputed <- imp_function(data_to_impute)
                                       colnames(self$data_imputed) <- self$state$context_cols
                                       self$imputed_predict <- TRUE
                                     }


                                     if (self$imputed_predict & self$flag=='predict' ){
                                       feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]

                                     }

                                     if(self$column_counter == 0 & self$flag=='train'){
                                       feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]
                                       self$flag=='predict'
                                       self$imputed_predict <- FALSE
                                     }

                                     return(feature)
                                   }

                                 )
)

mlr_pipeops$add("Amelia_imputation", PipeOpAmelia)
