#' @title PipeOpMedian_B
#'
#' @name PipeOpMedian_B
#'
#' @description
#' Impute features by OOR imputation in approach B (independently during the training and prediction phase).
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default `"impute_median_B"`.
#' }
#'
#' @examples
#' \donttest{
#'
#' # Using debug learner for example purpose
#'
#'   graph <- PipeOpMedian_B$new() %>>%  LearnerClassifDebug$new()
#'   graph_learner <- GraphLearner$new(graph)
#'
#'   # Task with NA
#'
#'   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
#' }
#' @export

PipeOpMedian_B = R6::R6Class("Median_B_imputation",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "impute_median_B", param_vals = list()) {
      super$initialize(id, param_vals = param_vals, packages = "stats", feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .train_imputer = function(feature, type, context) {
      NULL
    },
    .impute = function(feature, type, model, context) {
      med = stats::median(feature, na.rm = TRUE)
      if (type == "integer") {
        med = as.integer(round(med))
      }
      feature[is.na(feature)] <- med
    }
  )
)
