#' missForest imputation
#'
#' @description This class create object implements autotune_missForest function for use in mlr3 pipelinies. Object can be created with \code{\link{autotune_missForest}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines

PipeOpmissForest <-  R6::R6Class("missForest_imputation",lock_objects=FALSE,
                           inherit = PipeOp,  # inherit from PipeOp
                           public = list(
                             initialize = function(id = "imput_missForest", cores=NULL,ntree_set=c(100,200,500,1000),mtry_set=NULL,parallel=TRUE,
                                                   turn_off_parallel=FALSE,col_0_1=FALSE,mtry=NULL,ntree=100,optimize=TRUE
                             ) {
                               super$initialize(id, param_vals = list(cores =cores,ntree_set =ntree_set,mtry_set=mtry_set,parallel=parallel,
                                                                      turn_off_parallel=turn_off_parallel,col_0_1=col_0_1,mtry=mtry,ntree=ntree,optimize=optimize),
                                                param_set= ParamSet$new(list(
                                                  'ntree_set'=ParamUty$new('ntree_set', default = c(100,200,500,1000), tags = 'missForest'),
                                                  'cores'=ParamUty$new('cores',default = NULL,tags='missForest'),
                                                  'mtry_set'=ParamUty$new('mtry_set',default = NULL,tags='missForest'),
                                                  'parallel'=ParamLgl$new('parallel',default = TRUE,tags = 'missForest'),
                                                  'turn_off_parallel'=ParamLgl$new('turn_off_parallel',default = FALSE,tags='missForest'),
                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='missForest'),
                                                  'mtry'=ParamUty$new('mtry',default = NULL,tags='missForest'),
                                                  'ntree'=ParamInt$new('ntree',lower = 10,upper = Inf,default = 100,tags='missForest'),
                                                  'optimize'=ParamLgl$new('optimize',default = TRUE,tags='missForest')




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


                               data_imputed <- autotune_missForest(data_to_impute,percent_of_missing = percent_of_missing,cores = self$param_set$values$cores,
                                                                   ntree_set = self$param_set$values$ntree_set,mtry_set = self$param_set$values$mtry_set,
                                                                   parallel = self$param_set$values$parallel,turn_off_parallel = self$param_set$values$turn_off_parallel,
                                                                   col_0_1 = self$param_set$values$col_0_1,optimize = self$param_set$values$optimize,
                                                                   ntree = self$param_set$values$ntree,mtry = self$param_set$values$mtry)


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


                               data_imputed <- autotune_missForest(data_to_impute,percent_of_missing = percent_of_missing,cores = self$param_set$values$cores,
                                                                   ntree_set = self$param_set$values$ntree_set,mtry_set = self$param_set$values$mtry_set,
                                                                   parallel = self$param_set$values$parallel,turn_off_parallel = self$param_set$values$turn_off_parallel,
                                                                   col_0_1 = self$param_set$values$col_0_1,optimize = self$param_set$values$optimize,
                                                                   ntree = self$param_set$values$ntree,mtry = self$param_set$values$mtry)

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

