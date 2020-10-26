#' @title PipeOpMIDAS
#'
#' @name PipeOpMIDAS
#'
#' @description
#' Implements Multiple Imputation using Denoising Autoencoders method as mlr3 pipeline more about MIDAS \code{\link{rMIDAS}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_MIDAS_HD"}.
#' \item \code{path} :: \code{character(1)}\cr
#' character string, path to python binary if type == "auto", path to virtualenv if type == "virtualenv", or the name of a Conda environment if type=="condaenv". Default \code{'impute'}.
#' \item \code{type} :: \code{character(1)}\cr
#' character string, one of 'auto' (for python binary),'virtualenv', or 'condaenv'. Default \code{'condaenv'} .
#' \item \code{training_epchos} :: \code{integer(1)}\cr
#' An integer, indicating the number of forward passes to conduct when running the model. Default \code{10}.
#' \item \code{layer_structure} :: \code{integer(1)}\cr
#' A vector of integers, The number of nodes in each layer of the network (default = c(256, 256, 256), denoting a three-layer network with 256 nodes per layer). Larger networks can learn more complex data structures but require longer training and are more prone to overfitting.
#' \item \code{seed} :: \code{integer(1)}\cr
#' Random seed
#' \item \code{learn_rate} :: \code{integer(1)}\cr
#' A number, the learning rate γ (default = 0.0001), which controls the size of the weight adjustment in each training epoch. In general, higher values reduce training time at the expense of less accurate results.
#' }
#' @export
PipeOpMIDAS <- R6::R6Class("MIDAS_imputation",
                            lock_objects = FALSE,
                            inherit = PipeOpImpute, # inherit from PipeOp
                            public = list(
                              initialize = function(id = "impute_MIDAS_B", path='impute',type='condaenv',training_epchos=10L,layer_structure = c(256, 256, 256),seed =123,learn_rate = 4e-04) {
                                super$initialize(id,
                                                 whole_task_dependent = TRUE, packages = "EMMA", param_vals = list(path=path,type=type,training_epchos=training_epchos,layer_structure=layer_structure,
                                                                                                                   seed=seed,learn_rate=learn_rate),
                                                 param_set = ParamSet$new(list(
                                                   "path" = ParamFct$new("path", default = 'impute', tags = "MIDAS"),
                                                   "type" = ParamFct$new("type", default = 'condaenv', tags = "MIDAS"),
                                                   'training_epchos' = ParamInt$new('training_epchos',default = 10L,tags = 'MIDAS'),
                                                   'layer_structure' = ParamInt$new('layer_structure',default =c(256, 256, 256),tags = 'MIDAS' ),
                                                   'seed' = ParamInt$new('seed',default = 123,tags = 'MIDAS'),
                                                   'learn_rate' = ParamDbl$new('learn_rate',lower = 0,upper = Inf,default = 4e-04,tags = 'MIDAS')


                                                 ))
                                )



                                self$imputed <- FALSE
                                self$column_counter <- NULL
                                self$data_imputed <- NULL

                              }), private = list(
                                .train_imputer = function(feature, type, context) {

                                  imp_function <- function(data_to_impute) {

                                    data_to_impute <- as.data.frame(data_to_impute)
                                    # prepering arguments for function
                                    col_type <- 1:ncol(data_to_impute)
                                    for (i in col_type) {
                                      col_type[i] <- class(data_to_impute[, i])
                                    }
                                    percent_of_missing <- 1:ncol(data_to_impute)
                                    for (i in percent_of_missing) {
                                      percent_of_missing[i] <- (sum(is.na(data_to_impute[, i])) / length(data_to_impute[, 1])) * 100
                                    }
                                    col_miss <- colnames(data_to_impute)[percent_of_missing > 0]
                                    col_no_miss <- colnames(data_to_impute)[percent_of_missing == 0]




                                    data_imputed <- EMMA::autotune_rMIDAS(data_to_impute,col_type = col_type,path = self$param_set$values$path,type = self$param_set$values$type,
                                                                          training_epchos = self$param_set$values$training_epchos,layer_structure = self$param_set$values$layer_structure,
                                                                          seed =self$param_set$values$seed, learn_rate = self$param_set$values$learn_rate)$data


                                    return(data_imputed)
                                  }
                                  self$imputed_predict <- TRUE
                                  self$flag <- "train"
                                  if (!self$imputed) {
                                    self$column_counter <- ncol(context) + 1
                                    self$imputed <- TRUE
                                    data_to_impute <- cbind(feature, context)
                                    self$data_imputed <- imp_function(data_to_impute)
                                    colnames(self$data_imputed) <- self$state$context_cols

                                  }
                                  if (self$imputed) {
                                    self$column_counter <- self$column_counter - 1

                                  }
                                  if (self$column_counter == 0) {
                                    self$imputed <- FALSE
                                  }
                                  self$train_s <- TRUE

                                  self$action <- 3


                                  return(list("data_imputed" = self$data_imputed, "train_s" = self$train_s, "flag" = self$flag, "imputed_predict" = self$imputed_predict, "imputed" = self$imputed, "column_counter" = self$column_counter))

                                },
                                .impute = function(feature, type, model, context) {

                                  if (is.null(self$action)) {


                                    self$train_s <- T
                                    self$flag <- 'train'
                                    self$imputed_predict <- T
                                    self$action <- 3
                                    self$data_imputed <- model$data_imputed
                                    self$imputed <- F
                                    self$column_counter <- 0

                                  }
                                  imp_function <- function(data_to_impute) {

                                    data_to_impute <- as.data.frame(data_to_impute)
                                    # prepering arguments for function
                                    col_type <- 1:ncol(data_to_impute)
                                    for (i in col_type) {
                                      col_type[i] <- class(data_to_impute[, i])
                                    }
                                    percent_of_missing <- 1:ncol(data_to_impute)
                                    for (i in percent_of_missing) {
                                      percent_of_missing[i] <- (sum(is.na(data_to_impute[, i])) / length(data_to_impute[, 1])) * 100
                                    }
                                    col_miss <- colnames(data_to_impute)[percent_of_missing > 0]
                                    col_no_miss <- colnames(data_to_impute)[percent_of_missing == 0]


                                    data_imputed <- EMMA::autotune_rMIDAS(data_to_impute,col_type = col_type,path = self$param_set$values$path,type = self$param_set$values$type,
                                                                          training_epchos = self$param_set$values$training_epchos,layer_structure = self$param_set$values$layer_structure,
                                                                          seed =self$param_set$values$seed, learn_rate = self$param_set$values$learn_rate)$data



                                    return(data_imputed)
                                  }
                                  if (self$imputed) {
                                    feature <- self$data_imputed[, setdiff(colnames(self$data_imputed), colnames(context))]


                                  }
                                  if ((nrow(self$data_imputed) != nrow(context) | !self$train_s) & (self$flag == "train")) {
                                    self$imputed_predict <- FALSE
                                    self$flag <- "predict"
                                  }

                                  if (!self$imputed_predict) {
                                    data_to_impute <- cbind(feature, context)
                                    self$data_imputed <- imp_function(data_to_impute)
                                    colnames(self$data_imputed)[1] <- setdiff(self$state$context_cols, colnames(context))
                                    self$imputed_predict <- TRUE
                                  }


                                  if (self$imputed_predict & self$flag == "predict") {
                                    feature <- self$data_imputed[, setdiff(colnames(self$data_imputed), colnames(context))]

                                  }

                                  if (self$column_counter == 0 & self$flag == "train") {
                                    feature <- self$data_imputed[, setdiff(colnames(self$data_imputed), colnames(context))]
                                    self$flag <- "predict"
                                    self$imputed_predict <- FALSE
                                  }

                                  self$train_s <- FALSE
                                  return(feature)
                                }

                              )
)
