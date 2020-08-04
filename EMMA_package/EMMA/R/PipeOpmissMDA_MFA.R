#' missMDA_MFA imputation
#'
#' @description This class create object implements missMDA_MFA function for use in mlr3 pipelinies. Object can be created with \code{\link{missMDA_MFA}} params.
#'
#'
#'
#' @import mlr3
#' @import mlr3pipelines




PipeOpMissMDA_MFA <-  R6::R6Class("missMDA_MFAimputation",lock_objects=FALSE,
                                           inherit = PipeOp,  # inherit from PipeOp
                                           public = list(
                                             initialize = function(id = "imput_missMDA_MFA",col_0_1=F,ncp=2,random.seed=123,maxiter=1000,
                                                                   coeff.ridge=1,threshold=1e-06,method='Regularized'
                                             ) {
                                               super$initialize(id, param_vals = list(col_0_1=col_0_1,ncp=ncp,random.seed=random.seed,
                                                                                      maxiter=maxiter,coeff.ridge=coeff.ridge,threshold=threshold,method=method),
                                                                param_set= ParamSet$new(list(


                                                                  'ncp'=ParamInt$new('ncp',lower = 1,upper = Inf,default = 2,tags='MFA'),
                                                                  'maxiter'=ParamInt$new('maxiter',lower =50,upper = Inf,default = 1000,tags = 'MFA'),
                                                                  'coeff.ridge'=ParamDbl$new('coeff.ridge',lower = 0,upper = 1,default = 1,tags = 'MFA'),
                                                                  'threshold'=ParamDbl$new('threshold',lower = 0,upper = 1,default = 1e-6,tags = 'MFA'),
                                                                  'method'=ParamFct$new('method',levels = c('Regularized','EM'),default = 'Regularized',tags = 'MFA'),


                                                                  'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='MFA'),

                                                                  'col_0_1'=ParamLgl$new('col_0_1',default = F,tags='MFA')

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


                                               data_imputed <-missMDA_MFA(data_to_impute,col_type,percent_of_missing,random.seed = self$param_set$values$random.seed,
                                                                          ncp = self$param_set$values$ncp,col_0_1 = self$param_set$values$col_0_1,
                                                                          maxiter =  self$param_set$values$maxiter,coeff.ridge =  self$param_set$values$coeff.ridge,
                                                                          threshold =  self$param_set$values$threshold,method =  self$param_set$values$method)

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


                                               data_imputed <-missMDA_MFA(data_to_impute,col_type,percent_of_missing,random.seed = self$param_set$values$random.seed,
                                                                          ncp = self$param_set$values$ncp,col_0_1 = self$param_set$values$col_0_1,
                                                                          maxiter =  self$param_set$values$maxiter,coeff.ridge =  self$param_set$values$coeff.ridge,
                                                                          threshold =  self$param_set$values$threshold,method =  self$param_set$values$method)


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

mlr_pipeops$add("missMDA_MFAimputation", PipeOpMissMDA_MFA)

