#' @title PipeOpmissMDA_MFA_A
#'
#' @name PipeOpmissMDA_MFA_A
#'
#' @description
#' Implements MFA methods as mlr3 pipeline in A approche , more about MFA \code{\link{missMDA_MFA}} and \code{\link{missMDA.reuse}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_missMDA_MFA"}.
#' \item \code{ncp} :: \code{integer(1)}\cr
#' Number of dimensions used by algorithm, default \code{2}.
#' \item \code{maxiter} :: \code{integer(1)}\cr
#' Maximal number of iteration in algorithm, default \code{998}.
#' \item \code{coeff.ridge} :: \code{integer(1)}\cr
#' Value used in \emph{Regularized} method, default \code{1}.
#' \item \code{threshold} :: \code{double(1)}\cr
#' Threshold for convergence, default \code{1e-06}.
#' \item \code{method} :: \code{character(1)}\cr
#' Method used in imputation algorithm, default \code{'Regularized'}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location. If file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#' }
#'
#'
#' @examples
#' \donttest{
#'
#'  # Using debug learner for example purpose
#'
#'   graph <- PipeOpMissMDA_MFA_A$new() %>>% LearnerClassifDebug$new()
#'   graph_learner <- GraphLearner$new(graph)
#'
#'   # Task with NA
#'
#'   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
#' }
#' @export




PipeOpMissMDA_MFA_A <- R6::R6Class("missMDA_MFAimputation_A",
                                 lock_objects = FALSE,
                                 inherit = PipeOpImpute, # inherit from PipeOp
                                 public = list(
                                   initialize = function(id = "impute_missMDA_MFA_A", ncp = 2, maxiter = 998,
                                                         coeff.ridge = 1, threshold = 1e-06, method = "Regularized", out_file = NULL) {
                                     super$initialize(id,
                                                      whole_task_dependent = TRUE, packages = "NADIA", param_vals = list(
                                                        ncp = ncp,
                                                        maxiter = maxiter, coeff.ridge = coeff.ridge, threshold = threshold, method = method,
                                                        out_file = out_file),
                                                      param_set = ParamSet$new(list(
                                                        "ncp" = ParamInt$new("ncp", lower = 1, upper = Inf, default = 2, tags = "MFA"),
                                                        "maxiter" = ParamInt$new("maxiter", lower = 50, upper = Inf, default = 998, tags = "MFA"),
                                                        "coeff.ridge" = ParamDbl$new("coeff.ridge", lower = 0, upper = 1, default = 1, tags = "MFA"),
                                                        "threshold" = ParamDbl$new("threshold", lower = 0, upper = 1, default = 1e-6, tags = "MFA"),
                                                        "method" = ParamFct$new("method", levels = c("Regularized", "EM"), default = "Regularized", tags = "MFA"),



                                                        "out_file" = ParamUty$new("out_file", default = NULL, tags = "MFA")



                                                      )),

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

                                         data_imputed <- NADIA::missMDA_MFA(data_to_impute, col_type, percent_of_missing,
                                                                            random.seed = NULL,
                                                                            ncp = self$param_set$values$ncp,
                                                                            maxiter = self$param_set$values$maxiter, coeff.ridge = self$param_set$values$coeff.ridge,
                                                                            threshold = self$param_set$values$threshold, method = self$param_set$values$method,
                                                                            out_file = self$param_set$values$out_file,imp_data = TRUE)





                                         return(list("model"=data_imputed,"raw_data"=data_to_impute))
                                       }

                                       self$imputed_predict <- TRUE
                                       self$flag <- "train"
                                       if (!self$imputed) {
                                         self$column_counter <- ncol(context) + 1
                                         self$imputed <- TRUE
                                         data_to_impute <- cbind(feature, context)
                                         colnames(data_to_impute)[1] <- setdiff(self$state$context_cols, colnames(data_to_impute))
                                         self$model <- imp_function(data_to_impute)
                                         self$data_imputed <- self$model$model$data
                                         self$raw_data <- self$model$raw_data
                                       }
                                       if (self$imputed) {
                                         self$column_counter <- self$column_counter - 1

                                       }
                                       if (self$column_counter == 0) {
                                         self$imputed <- FALSE
                                       }
                                       self$train_s <- TRUE

                                       self$action <- 3


                                       return(list("model"=self$model,"raw_data"=self$raw_data,"data_imputed" = self$data_imputed, "train_s" = self$train_s, "flag" = self$flag, "imputed_predict" = self$imputed_predict, "imputed" = self$imputed, "column_counter" = self$column_counter))

                                     },
                                     .impute = function(feature, type, model, context) {

                                       if (is.null(self$action)) {


                                         self$train_s <- TRUE
                                         self$flag <- "train"
                                         self$imputed_predict <- TRUE
                                         self$action <- 3
                                         self$data_imputed <- model$data_imputed
                                         self$raw_data <- model$raw_data
                                         self$imputed <- FALSE
                                         self$model <- model$model
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

                                         data_imputed <-missMDA.reuse(train_data = self$raw_data,new_data = data_to_impute, random.seed = NULL,
                                                                              ncp = self$param_set$values$ncp,
                                                                              maxiter = self$param_set$values$maxiter, coeff.ridge = self$param_set$values$coeff.ridge,
                                                                              threshold = self$param_set$values$threshold, method = self$param_set$values$method,MFA = TRUE,
                                                                              MFA_Object = self$model$model)





                                         return(data_imputed)
                                       }
                                       if (self$imputed) {

                                         feature <- self$data_imputed[, setdiff(colnames(self$data_imputed), colnames(context))]



                                       }
                                       if ((nrow(self$data_imputed) != nrow(context) | !self$train_s) & self$flag == "train") {
                                         self$imputed_predict <- FALSE
                                         self$flag <- "predict"
                                       }

                                       if (!self$imputed_predict) {
                                         data_to_impute <- cbind(feature, context)
                                         colnames(data_to_impute)[1] <- setdiff(self$state$context_cols, colnames(context))
                                         # its important to keep the same columns order
                                         data_to_impute <- as.data.frame(data_to_impute)[, self$state$context_cols]
                                         self$data_imputed <- imp_function(data_to_impute)



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
