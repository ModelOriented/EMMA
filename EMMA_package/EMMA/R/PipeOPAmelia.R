#' Amelia imputation
#'
#' @description This class create object implements autotune_Amelia function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_Amelia}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines

PipeOpAmelia <-  R6::R6Class("Amelia_imputation",lock_objects=FALSE,
                                 inherit = PipeOp,  # inherit from PipeOp
                                 public = list(
                                   initialize = function(id = "imput_Amelia", col_0_1=FALSE,polytime=NULL,splinetime=NULL,intercs=FALSE,empir=NULL,m=3,parallel=TRUE
                                   ) {
                                     super$initialize(id, param_vals = list(col_0_1=col_0_1,polytime=polytime,splinetime=splinetime,intercs=intercs,empir=empir,m=m,parallel=parallel),
                                                      param_set= ParamSet$new(list(
                                                        'polytime'=ParamUty$new('polytime', default = NULL, tags = 'amelia'),
                                                        'splinetime'=ParamUty$new('splinetime',default = NULL,tags='amelia'),
                                                        'empir'=ParamUty$new('empir',default = NULL,tags='amelia'),
                                                        'parallel'=ParamLgl$new('parallel',default = TRUE,tags = 'amelia'),
                                                        'intercs'=ParamLgl$new('intercs',default = FALSE,tags='amelia'),
                                                        'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='amelia'),
                                                        'm'=ParamInt$new('m',lower = 1,upper = Inf,default = 3,tags='amelia')





                                                      )),
                                                      # declare "input" and "output" during construction here
                                                      # training and prediction take task and return task with imputed data ;
                                                      input = data.table::data.table(name = "input",
                                                                                     train = "Task", predict = 'Task'),
                                                      output = data.table::data.table(name = "output",
                                                                                      train = "Task", predict = "Task")
                                     )

                                   },

                                   # PipeOp deriving classes must implement train_internal and
                                   # predict_internal; each taking an input list and returning
                                   # a list as output.
                                   train_internal = function(input) {

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


                                     data_imputed <- autotune_Amelia(data_to_impute,col_type,percent_of_missing,col_0_1 = self$param_set$values$col_0_1,
                                                                     parallel = self$param_set$values$parallel,polytime = self$param_set$values$polytime,
                                                                     splinetime = self$param_set$values$splinetime, intercs = self$param_set$values$intercs,
                                                                     empir = self$param_set$values$empir,m=self$param_set$values$m)


                                     data_imputed <- cbind(data_imputed,target_col)
                                     colnames(data_imputed)[ncol(data_imputed)] <- input[[1]]$target_names


                                     if (input[[1]]$task_type=='classif'){
                                       if (is.na(input[[1]]$positive)){
                                         input[[1]] <- TaskClassif$new(input[[1]]$id,data_imputed,input[[1]]$target_names)}
                                       else {input[[1]] <- TaskClassif$new(input[[1]]$id,data_imputed,input[[1]]$target_names,positive = input[[1]]$positive)}
                                     }
                                     if ( input[[1]]$task_type=='regr'){
                                       input[[1]] <- TaskRegrf$new(input[[1]]$id,data_imputed,input[[1]]$target_names)
                                     }
                                     return(input)
                                   },

                                   predict_internal = function(input) {
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

                                     if (input[[1]]$task_type=='classif'){
                                       if (is.na(input[[1]]$positive)){
                                         input[[1]] <- TaskClassif$new(input[[1]]$id,data_imputed,input[[1]]$target_names)}
                                       else {input[[1]] <- TaskClassif$new(input[[1]]$id,data_imputed,input[[1]]$target_names,positive = input[[1]]$positive)}
                                     }
                                     if ( input[[1]]$task_type=='regr'){
                                       input[[1]] <- TaskRegrf$new(input[[1]]$id,data_imputed,input[[1]]$target_names)
                                     }

                                     return(input)
                                   }
                                 )
)

