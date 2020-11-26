#' @title PipeOpmissRanger
#'
#' @name PipeOpmissRanger
#'
#' @description
#' Implements missRanger methods as mlr3 pipeline, more about missRanger \code{\link{autotune_missRanger}}.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_missRanger"}.
#' \item \code{mtry} :: \code{integer(1)}\cr
#' Sample fraction used by missRanger. This param isn't optimized automatically. If NULL default value from ranger package will be used, \code{NULL}.
#' \item \code{num.trees} :: \code{integer(1)}\cr
#' Number of trees. If optimize == TRUE. Param set seq(10,num.trees,iter) will be used, default \code{500}
#' \item \code{pmm.k} :: \code{integer(1)}\cr
#' Number of candidate non-missing values to sample from in the predictive mean matching step. 0 to avoid this step. If optimize=TRUE params set: sample(1:pmm.k, iter) will be used. If pmm.k=0, missRanger is the same as missForest, default \code{5}.
#' \item \code{random.seed} :: \code{integer(1)}\cr
#' Random seed, default \code{123}.
#' \item \code{iter} :: \code{integer(1)}\cr
#' Number of iterations for a random search, default \code{10}.
#' \item \code{optimize} :: \code{logical(1)}\cr
#' If set TRUE, function will optimize parameters of imputation automatically. If parameters will be tuned by other method, should be set to FALSE, default \code{FALSE}.
#' \item \code{out_fill} :: \code{character(1)}\cr
#' Output log file location. If file already exists log message will be added. If NULL no log will be produced, default \code{NULL}.
#' }
#'
#' @examples
#' \donttest{
#'
#'  # Using debug learner for example purpose
#'
#'   graph <- PipeOpmissRanger$new() %>>% LearnerClassifDebug$new()
#'   graph_learner <- GraphLearner$new(graph)
#'
#'   # Task with NA
#'
#'   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
#' }
#' @export



PipeOpmissRanger <- R6::R6Class("missRanger_imputation",
  lock_objects = FALSE,
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "impute_missRanger_B", maxiter = 10, random.seed = 123, mtry = NULL, num.trees = 500,
      pmm.k = 5, optimize = FALSE, iter = 10, out_file = NULL) {
      super$initialize(id,
        whole_task_dependent = TRUE, packages = "NADIA", param_vals = list(
          maxiter = maxiter, random.seed = random.seed, mtry = mtry, num.trees = num.trees,
          pmm.k = pmm.k, iter = iter, optimize = optimize, out_file = out_file),
        param_set = ParamSet$new(list(
          "maxiter" = ParamInt$new("maxiter", lower = 1, upper = Inf, default = 10, tags = "missRanger"),
          "random.seed" = ParamInt$new("random.seed", default = 123, tags = "missRanger"),
          "mtry" = ParamUty$new("mtry", default = NULL, tags = "missRanger"),
          "num.trees" = ParamInt$new("num.trees", default = 500, lower = 10, upper = Inf, tags = "missRanger"),
          "pmm.k" = ParamInt$new("pmm.k", lower = 0, upper = Inf, default = 5, tags = "missRagner"),
          "optimize" = ParamLgl$new("optimize", default = FALSE, tags = "missRagner"),
          "iter" = ParamInt$new("iter", lower = 1, upper = Inf, default = 10, tags = "missRanger"),

          "out_file" = ParamUty$new("out_file", default = NULL, tags = "missRanger")

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

        data_imputed <- NADIA::autotune_missRanger(data_to_impute, percent_of_missing,
          maxiter = self$param_set$values$maxiter,
          random.seed = self$param_set$values$random.seed, mtry = self$param_set$values$mtry,
          num.trees = self$param_set$values$num.trees,
          out_file = self$param_set$values$out_file, optimize = self$param_set$values$optimize,
          iter = self$param_set$values$iter, pmm.k = self$param_set$values$pmm.k)






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


        self$train_s <- TRUE
        self$flag <- "train"
        self$imputed_predict <- TRUE
        self$action <- 3
        self$data_imputed <- model$data_imputed
        self$imputed <- FALSE
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

        data_imputed <- NADIA::autotune_missRanger(data_to_impute, percent_of_missing,
          maxiter = self$param_set$values$maxiter,
          random.seed = self$param_set$values$random.seed, mtry = self$param_set$values$mtry,
          num.trees = self$param_set$values$num.trees,
          out_file = self$param_set$values$out_file, optimize = self$param_set$values$optimize,
          iter = self$param_set$values$iter, pmm.k = self$param_set$values$pmm.k)




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
