#' @title PipeOpVIM_HD
#'
#' @name PipeOpVIM_HD
#'
#' @description
#' Implements Hot Deck methods as mlr3 pipeline more about VIM_HD \code{\link{autotune_VIM_hotdeck}}
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_VIM_HD"}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location. If file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#' }
#'
#'
#' @examples
#' {
#'   graph <- PipeOpVIM_HD$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
#'   graph_learner <- GraphLearner$new(graph)
#'
#'   # Task with NA
#'
#'   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
#' }
#' @export



PipeOpVIM_HD <- R6::R6Class("VIM_HD_imputation",
  lock_objects = FALSE,
  inherit = PipeOpImpute, # inherit from PipeOp
  public = list(
    initialize = function(id = "impute_VIM_HD_B", out_file = NULL) {
      super$initialize(id,
        whole_task_dependent = TRUE, packages = "EMMA", param_vals = list(out_file = out_file),
        param_set = ParamSet$new(list(
          "out_file" = ParamUty$new("out_file", default = NULL, tags = "VIM_HD")


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




        data_imputed <- EMMA::autotune_VIM_hotdeck(data_to_impute, percent_of_missing,
          out_file = self$param_set$values$out_file)



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
        self$flag <- "train"
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


        data_imputed <- EMMA::autotune_VIM_hotdeck(data_to_impute, percent_of_missing,
          out_file = self$param_set$values$out_file)



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
