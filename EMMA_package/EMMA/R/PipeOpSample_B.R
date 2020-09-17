#' @title PipeOpSample_B
#'
#' @name PipeOpSample_B
#'
#' @description
#' Impute features by sampling from non-missing data in approach B (independently during the training and prediction phase).
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#' @section Parameters:
#' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default `"impute_sample_B"`.
#'}
#'
#' @export

PipeOpSample_B = R6::R6Class("Sample_B_imputation",
                             inherit = PipeOpImpute,
                             public = list(
                               initialize = function(id = "impute_sample_B", param_vals = list()) {
                                 super$initialize(id, param_vals = param_vals, packages = "stats", feature_types = c("factor", "integer", "logical", "numeric", "ordered"))
                               }
                             ),
                             private = list(
                               .train_imputer = function(feature, type, context) {
                               NULL
                               },
                               
                               .impute = function(feature, type, model, context) {
                                 
                                 train_model <- function(feature, type, context){
                                 
                                   fvals = feature[!is.na(feature)]
                                   if (length(fvals) < 10) {  # don't bother with table if vector is short
                                     return(fvals)
                                   }
                                   tab = data.table::data.table(fvals)[, data.table::.N, by = "fvals"]
                                   if (nrow(tab) > length(fvals) / 2) {
                                     # memory usage of count table is larger than memory usage of just the values
                                     return(fvals)
                                   }
                                   model = tab$fvals
                                   attr(model, "probabilities") = tab$N / sum(tab$N)
                                   model
                                   
                                 }
                                 
                                 model <- train_model(feature, type, context)

                                 if (type %in% c("factor", "ordered")) {
                                   # in some edge cases there may be levels during training that are missing during predict.
                                   levels(feature) = c(levels(feature), as.character(model))
                                 }
                                 if (length(model) == 1) {
                                   feature[is.na(feature)] = model
                                 } else {
                                   outlen = sum(is.na(feature))
                                   feature[is.na(feature)] = sample(model, outlen, replace = TRUE, prob = attr(model, "probabilities"))
                                 }
                                 feature
                               }
                             )
)

mlr_pipeops$add("impute_sample_B", PipeOpSample_B)

