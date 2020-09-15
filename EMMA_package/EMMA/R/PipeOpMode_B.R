#' @title PipeOpMode_B
#'
#' @name PipeOpMode_B
#'
#' @description
#' Impute features by their mode in approach B (independently during the training and prediction phase).
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default `"impute_mode_B"`.
#'}
#'
#' @export


PipeOpMode_B = R6::R6Class("Mode_B_imputation",
                           inherit = PipeOpImpute,
                           public = list(
                             initialize = function(id = "impute_mode_B", param_vals = list()) {
                               super$initialize(id, param_vals = param_vals, feature_types = c("factor", "integer", "logical", "numeric", "ordered"))
                             }
                           ),
                           private = list(
                             .train_imputer = function(feature, type, context) {
                               NULL
                             },
                             .impute = function(feature, type, model, context) {

                               feature_no_na = feature[!is.na(feature)]

                               feature[is.na(feature)]  <- as.data.table(feature_no_na)[, .N, by = list(feature_no_na)][get("N") == max(get("N"))]$feature_no_na[1]
                               feature
                             }
                           )
)

mlr_pipeops$add("impute_mode_B", PipeOpMode_B)

# m <- PipeOpImputeMode_B$new()
#
#
# gr <- m%>>% PipeOpEncodeImpact$new() %>>% lrn('classif.glmnet')
# grlr <- GraphLearner$new(gr)
# resample(task,grlr,rsmp('cv'))
