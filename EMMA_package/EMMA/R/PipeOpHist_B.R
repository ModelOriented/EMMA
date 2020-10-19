#' @title PipeOpHist_B
#'
#' @name PipeOpHist_B
#'
#' @description
#' Impute numerical features by histogram in approach B (independently during the training and prediction phase).
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default `"impute_hist_B"`.
#' }
#'
#' @export
PipeOpHist_B = R6::R6Class("Hist_B_imputation",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(id = "impute_hist_B", param_vals = list()) {
      super$initialize(id, param_vals = param_vals, packages = "graphics", feature_types = c("integer", "numeric"))
    }
  ),
  private = list(
    .train_imputer = function(feature, type, context) {
      NULL
    },

    .impute = function(feature, type, model, context) {
      model <- graphics::hist(feature, plot = FALSE)[c("counts", "breaks")]
      if (is.atomic(model)) { # handle nullmodel
        return(super$.impute(feature, type, model, context))
      }
      which.bins = sample.int(length(model$counts), sum(is.na(feature)), replace = TRUE, prob = model$counts)
      sampled = stats::runif(length(which.bins), model$breaks[which.bins], model$breaks[which.bins + 1L])
      if (type == "integer") {
        sampled = as.integer(round(sampled))
      }
      feature[is.na(feature)] = sampled
      feature
    }
  )
)

