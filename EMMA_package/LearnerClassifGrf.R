#' @title Bootstraped regression forest with dummy encoding
#'
#' @name LearnerClassifGrf.Rd
#'
#' @description
#' Boosted regression forest using dummy encoder from \code{\link[recipes:step_dummy]{step_dummy}}. Forest is created with package \code{\link[grf:boosted_regression_forest]{boosted_regression_forest}}
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
                                         packages = c("grf","recipes"),
                                         feature_types = c("numeric","logical","integer",'factor'),
                                         predict_types = "response",
                                         param_set = ps,
                                         properties = c( "twoclass")

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
                                       vars <- colnames(data_to_train)

                                       encoder <- recipe(data_to_train) %>% update_role(all_of(vars), new_role = "predictor") %>% step_dummy(all_predictors(),one_hot = F)

                                       encoder_model <- prep(encoder,training =data_to_train)

                                       self$state = list(
                                         dummy_encode = encoder_model
                                       )

                                       data_to_train<- bake(encoder_model, data_to_train)



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
                                       data_to_predict<- bake(self$state$dummy_encode,data_to_predict)



                                       pred <- predict(self$model,(data_to_predict))




                                       pred <- self$state$levels[ifelse(round(pred)<0.5,1,2)]
                                       PredictionClassif$new(task = task,response = pred)
                                     }
                                   )
)

















#   d <- LearnerClassifGrf$new()
#  gr <-   d
#  grlr <- GraphLearner$new(gr)
# #
#  w<- resample(task,grlr,rsmp('cv',folds=5))
# #
#  w$aggregate(msr('classif.acc'))
# #
# # library(mlr3oml)
# # for (i in tasks$task.id){
#
# # d <- LearnerClassifGrf$new()
# # gr <-  d
# # grlr <- GraphLearner$new(gr)
# #
#  task <- mlr3oml::OMLTask$new(id = 3704)
#  task <- task$task
# #
# tryCatch({
# w<- resample(task,grlr,rsmp('cv',folds=5))
# },error=function(e){
#   print(as.character(e))
# })
#
#
#
#
# }
