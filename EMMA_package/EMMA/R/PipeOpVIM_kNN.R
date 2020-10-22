#' @title PipeOpVIM_kNN
#' @name PipeOpVIM_kNN
#'
#' @description
#' Implements KNN methods as mlr3 pipeline, more about VIM_KNN \code{\link{autotune_VIM_kNN}}.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_VIM_kNN"}.
#' \item \code{k} :: \code{intiger(1)}\cr
#' Threshold for convergence, default \code{5}.
#' \item \code{numFUN} :: \code{function(){}}\cr
#' Function for aggregating the k Nearest Neighbours in the case of a numerical variable.  Can be ever function with input=numeric_vector and output=atomic_object, default \code{median}.
#' \item \code{catFUN} :: \code{function(){}}\cr
#' Function for aggregating the k Nearest Neighbours in case of categorical variables. It can be any function with input=not_numeric_vector and output=atomic_object, default \code{VIM::maxCat}
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location. If file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#' }
#'
#' @export


PipeOpVIM_kNN <- R6::R6Class("VIM_kNN_imputation",
  lock_objects = FALSE,
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "impute_VIM_kNN_B", k = 5, numFun = median, catFun = VIM::maxCat, out_file = NULL) {
      super$initialize(id,
        whole_task_dependent = TRUE, packages = "EMMA", param_vals = list(k = k, numFun = numFun, catFun = catFun, out_file = out_file),
        param_set = ParamSet$new(list(
          "k" = ParamInt$new("k", lower = 1, upper = Inf, default = 5, tags = "VIM_kNN"),
          "numFun" = ParamUty$new("numFun", default = median, tags = "VIM_kNN"),
          "catFun" = ParamUty$new("catFun", default = VIM::maxCat, tags = "VIM_kNN"),

          "out_file" = ParamUty$new("out_file", default = NULL, tags = "VIM_kNN")

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


        data_imputed <- EMMA::autotune_VIM_kNN(data_to_impute, percent_of_missing,
          k = self$param_set$values$k, numFun = self$param_set$values$numFun,
          catFun = self$param_set$values$catFun, out_file = self$param_set$values$out_file)




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


        data_imputed <- EMMA::autotune_VIM_kNN(data_to_impute, percent_of_missing,
          k = self$param_set$values$k, numFun = self$param_set$values$numFun,
          catFun = self$param_set$values$catFun, , out_file = self$param_set$values$out_file)




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
