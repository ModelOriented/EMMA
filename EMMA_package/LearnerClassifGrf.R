LearnerClassifGrf = R6::R6Class("LearnerClassifGrf",
                                   inherit = LearnerClassif,

                                   public = list(
                                     #' @description
                                     #' Creates a new instance of this [R6][R6::R6Class] class.
                                     initialize = function() {
                                       ps = ParamSet$new(
                                         params = list(


                                         )
                                       )

                                       # If you change any defaults for mlr3 compared to the underlying learner,
                                       # specify them here
                                       ps$values = list()

                                       super$initialize(
                                         # see the mlr3book for a description: https://mlr3book.mlr-org.com/extending-mlr3.html
                                         id = "classif.grf",
                                         packages = "misaem",
                                         feature_types = c("numeric","logical","integer",'factor'),
                                         predict_types = "response",
                                         param_set = ps,
                                         properties = c( "twoclass", "multiclass")

                                       )
                                     }


                                     # <Add method for importance, if learner supports that>
                                     # <See mlr3learners.randomForest for an example>

                                     # <Add method for oob_error, if learner supports that.>

                                   ),

                                   private = list(
                                     .train = function(task) {
                                       # data preperation
                                       data_to_train <- as.data.frame(task$data(cols = task$feature_names))
                                       target <- as.data.frame(task$data(cols = task$target_names))

                                       target <- target[,task$target_names,drop=T]

                                       if(class(target)=='factor'){
                                         self$state$levels <- levels(target)
                                         target <- as.integer(target)-1
                                       }
                                       data_to_train<- cbind(data_to_train,target)

                                       # training
                                       misaem::miss.glm(as.formula(paste0('target','~.')),data_to_train)

                                     },

                                     .predict = function(task) {
                                       data_to_predict <-  as.data.frame(task$data(cols = task$feature_names))
                                       pred <- predict(self$model,(data_to_predict))
                                       pred <- self$state$levels[pred+1]
                                       PredictionClassif$new(task = task,response = pred)
                                     }
                                   )
)






 d <- LearnerClassifGrf$new()
gr <-   d
grlr <- GraphLearner$new(gr)

w<- resample(task,grlr,rsmp('holdout'))


#
# library(mlr3oml)
# for (i in tasks$task.id){

# d <- LearnerClassifGrf$new()
# gr <-  d
# grlr <- GraphLearner$new(gr)
#
 task <- mlr3oml::OMLTask$new(id = 55)
 task <- task$task
#
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
