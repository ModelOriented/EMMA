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
#'}
#'
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

mlr_pipeops$add("impute_median_B", PipeOpMedian_B)

# m <- PipeOpImputeMedian_B$new()
#
#
# gr <- m%>>% PipeOpEncodeImpact$new() %>>% lrn('classif.glmnet')
# grlr <- GraphLearner$new(gr)
# resample(task_numeric,grlr,rsmp('cv'))
#
# test <- iris
# test$Sepal.Width[sample(1:150,50)] <- NA
# task_numeric<- TaskClassif$new('d',test,"Species")
