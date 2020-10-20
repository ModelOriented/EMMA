#' @title Bootstraped regression forest
#'
#' @name LearnerClassifGrf.Rd
#'
#' @description
#' The algorithm used in this learner is available here \code{\link[grf:boosted_regression_forest]{boosted_regression_forest}}
#'
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default `"classif.grf"`.
#' \item \code{num.trees} :: \code{integer(1)}\cr
#' Number of trees grown in the forest. Note: Getting accurate confidence intervals generally requires more trees than getting accurate predictions. Default  \code{2000}.
#' \item \code{sample.fractions} :: \code{double(1)}\cr
#' Fraction of the data used to build each tree. Default  \code{0.5}.
#' \item \code{mtry} :: \code{integer(1)}\cr
#' Number of variables tried for each split. Default is √ p + 20 where p is the number of variables.
#' \item \code{alpha} :: \code{integer(1)}\cr
#' A tuning parameter that controls the maximum imbalance of a split. Default is 0.05.
#'}
#'
#'
#'
#' @templateVar id classif.grf
#'
#' @export
LearnerClassifGrf = R6::R6Class("LearnerClassifGrf",
                                   inherit = LearnerClassif,

                                   public = list(

                                     initialize = function() {
                                       ps = ParamSet$new(
                                         params = list(
                                          'num.trees'= ParamInt$new('num.trees',lower = 10,upper = Inf,default = 2000,tags = 'grf'),
                                          'sample.fraction'=ParamDbl$new('sample.fraction',lower = 0.01,upper = Inf,default = 0.5,tags='grf'),
                                          'mtry'=ParamUty$new('mtry',default = NULL,tags = 'grf'),
                                          'alpha' = ParamDbl$new('alpha',default = 0.05,tags='grf')


                                         )
                                       )


                                       ps$values = list(num.trees = 2000,sample.fraction=0.5,mtry=NULL,alpha=0.05)

                                       super$initialize(

                                         id = "classif.grf",
                                         packages = "grf",
                                         predict_types = c("response","prob"),
                                         feature_types = c("numeric","logical","integer"),
                                         param_set = ps,
                                         properties = "twoclass"

                                       )
                                     }




                                   ),

                                   private = list(
                                     .train = function(task) {
                                       # data preperation
                                       data_to_train <- as.data.frame(task$data(cols=task$feature_names))
                                       target <- as.data.frame(task$data(cols = task$target_names))

                                       target <- target[,task$target_names,drop=T]

                                       # Data encoding
                                       # vars <- colnames(data_to_train)
                                       #
                                       # encoder <- recipe(data_to_train) %>% update_role(all_of(vars), new_role = "predictor") %>% step_dummy(all_predictors(),one_hot = F)
                                       #
                                       # encoder_model <- prep(encoder,training =data_to_train)
                                       #
                                       # self$state = list(
                                       #   dummy_encode = encoder_model
                                       # )
                                       #
                                       # data_to_train<- bake(encoder_model, data_to_train)
                                       #


                                       if(class(target)=='factor'){
                                         self$state$levels <- levels(target)
                                         target <- as.integer(target)-1
                                       }


                                       # training
                                       if (is.null(self$param_set$values$mtry)){mtry <- min(ceiling(sqrt(ncol(data_to_train)) + 20), ncol(data_to_train))}
                                       grf::boosted_regression_forest(data_to_train,target,num.trees = self$param_set$values$num.trees,sample.fraction = self$param_set$values$sample.fraction,
                                                                      mtry = mtry,alpha = self$param_set$values$alpha)

                                     },

                                     .predict = function(task) {
                                       data_to_predict <-  as.data.frame(task$data(cols = task$feature_names))
                                       # data_to_predict<- bake(self$state$dummy_encode,data_to_predict)



                                       pred <- predict(self$model,(data_to_predict))$predictions

                                      pred <- ifelse(pred>1,1,pred)
                                      pred <- ifelse(pred<0,0,pred)


                                       response <- self$state$levels[ifelse(round(pred)<0.5,1,2)]
                                       prob <- matrix(ncol = 2,nrow = length(pred))
                                       colnames(prob) <- self$state$levels
                                       prob[,1] <- 1-pred
                                       prob[,2] <- pred
                                       PredictionClassif$new(task = task,response = response,prob = prob)
                                     }
                                   )
)


