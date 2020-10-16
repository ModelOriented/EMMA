#' @title PipeOpSimulateMissings
#'
#' @name PipeOpSimulateMissings
#'
#' @description
#' Generates MCAR missing values in mlr3 pipeline according to set parameters. 
#' Missings are inserted to task data once during first training.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpTaskPreproc}}.
#'
#'
#' @section Parameters:
#' \itemize{
#' \item \code{per_missings} :: \code{double(1)}\cr
#' Overall percentage of missing values generated in dataset [0, 100]. Must be set every time, default 50%.
#' \item \code{per_instances_missings} :: \code{double(1)}\cr
#' Percentage of instances which will have missing values [0, 100].
#' \item \code{per_variables_missings} :: \code{double(1)}\cr
#' Percentage of variables which will have missing values [0, 100].
#' \item \code{variables_missings} :: \code{integer}\cr
#' Only when `per_variables_missings` is `NULL`. Vector of indexes of columns in which missings will be generated.
#'}
#'
#' @export

PipeOpSimulateMissings = R6::R6Class("PipeOpSimulateMissings", 
                                     inherit = mlr3pipelines::PipeOpTaskPreproc,
                                     lock_objects = FALSE,
                                     public = list(
                                       initialize = function(id = "simulate_missings", param_vals = list(per_missings = 50)){
                                         
                                         ps = ParamSet$new(params = list(
                                           ParamDbl$new(id = "per_missings", lower = 0, upper = 100, default = 50),
                                           ParamDbl$new(id = "per_instances_missings", lower = 0, upper = 100, default = NULL, special_vals = list(NULL)),
                                           ParamDbl$new(id = "per_variables_missings", lower = 0, upper = 100, default = NULL, special_vals = list(NULL)),
                                           ParamUty$new(id = "variables_with_missings", default = NULL)))
                                         
                                         super$initialize(id = id, param_set = ps, param_vals = param_vals)
                                         
                                         self$miss_generated = FALSE
                                         
                                       }),
                                       
                                     private = list(
                                     
                                       .train_task = function(task){
                                         
                                         if(!self$miss_generated){
                                           dt <- task$backend$data(cols = task$backend$colnames, rows = 1:task$backend$nrow)
                                           
                                           df_miss <- as.data.frame(dt)
                                           df_miss[, task$feature_names] <- simulate_missings(df = df_miss[, task$feature_names], 
                                                                   per_missings = self$param_set$values$per_missings, 
                                                                   per_instances_missings = self$param_set$values$per_instances_missings, 
                                                                   per_variables_missings = self$param_set$values$per_variables_missings, 
                                                                   variables_with_missings = self$param_set$values$variables_missings)
                                           
                                           new_dt <- as.data.table(df_miss)
                                           new_backend <- as_data_backend(data = new_dt, primary_key = task$backend$primary_key)
                                           task$backend <- new_backend
                                           self$miss_generated <- TRUE
                                           
                                           return(task)
                                           
                                         }else{
                                           task
                                         }
                                       },
                                       
                                       .predict_task = function(task){
                                         
                                         #should be nothing to do
                                         if(self$miss_generated){
                                           task
                                         }else{
                                           warning("Something is wrong!\n
                                                   Missings should have been already generated.")
                                         }
                                         
                                       }
                                     ))
