#' @title PipeOpOOR_B
#'
#' @name PipeOpOOR_B
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
#' Identifier of resulting object, default `"impute_OOR_B"`.
#'}
#'
#' @export

PipeOpOOR_B = R6::R6Class("OOR_B_imputation",
                          inherit = PipeOpImpute,
                          public = list(
                            initialize = function(id = "impute_oor_B", param_vals = list()) {
                              ps = ParamSet$new(params = list(
                                ParamLgl$new("min", tags = c("train", "predict")),
                                ParamDbl$new("offset", lower = 0, tags = c("train", "predict")),
                                ParamDbl$new("multiplier", lower = 0, tags = c("train", "predict")))
                              )
                              ps$values = list(min = TRUE, offset = 1, multiplier = 1)
                              # this is one of the few imputers that handles 'character' features!
                              super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("character", "factor", "integer", "numeric", "ordered"))
                            }
                          ),
                          private = list(
                            .train_imputer = function(feature, type, context) {
                             NULL
                            },
                            .impute = function(feature, type, model, context) {


                              if (type %in% c("factor", "ordered", "character")) {
                                return(".MISSING")  # early exit
                              }

                              offset = self$param_set$values$offset + self$param_set$values$multiplier * diff(range(feature, na.rm = TRUE))
                              oor = if (self$param_set$values$min) {
                                min(feature, na.rm = TRUE) - offset
                              } else {
                                max(feature, na.rm = TRUE) + offset
                              }

                              if (type == "integer") {
                                oor = as.integer(round(oor))
                              }

                              feature[is.na(feature)] <- oor

                            }
                          )
)

mlr_pipeops$add("impute_oor_B", PipeOpOOR_B)


# m <- PipeOpImputeOOR_B$new()
#
#
# gr <- m%>>% PipeOpEncodeImpact$new() %>>% lrn('classif.glmnet')
# grlr <- GraphLearner$new(gr)
# resample(task,grlr,rsmp('cv'))
