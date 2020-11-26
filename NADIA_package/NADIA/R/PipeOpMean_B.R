#' @title PipeOpMean_B
#'
#' @name PipeOpMean_B
#'
#' @description
#' Impute numerical features by their mean in approach B (independently during the training and prediction phase).
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_mean_B"}.
#' }
#'
#' @examples
#' \donttest{
#'
#'  # Using debug learner for example purpose
#'
#'   graph <- PipeOpMean_B$new() %>>% LearnerClassifDebug$new()
#'   graph_learner <- GraphLearner$new(graph)
#'   set.seed(1)
#'   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
#' }
#' @export


PipeOpMean_B <- R6::R6Class("Mean_B_imputation",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "impute_mean_B", param_vals = list()) {
      super$initialize(id, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(
    .train_imputer = function(feature, type, context) {
      NULL
    },

    .impute = function(feature, type, model, context) {
      men = mean(feature, na.rm = TRUE)
      if (type == "integer") {
        men = as.integer(round(men))
      }
      feature[is.na(feature)] = men
      feature
    }
  )
)
