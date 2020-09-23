library(tidymodels)

PipeOpTidyBagimpute = R6::R6Class("PipeOpTidyBagimpute",
                                inherit = mlr3pipelines::PipeOpTaskPreproc,
                                public = list(
                                  initialize = function(id = "impute_tidy_bagimpute") {
                                    super$initialize(id = id)
                                  }
                                ),
                                
                                private = list(
                                  
                                  .train_dt = function(dt, levels, target) {
                                    
                                    vars <- colnames(dt)
                                    
                                    imputer <- recipe(dt) %>% 
                                      update_role(all_of(vars), new_role = "predictor") %>%
                                      step_bagimpute(all_predictors())
                                    
                                    imp_model <- prep(imputer, dt)
                                    
                                    self$state = list(
                                      bagimpute_model = imp_model
                                    )
                                    
                                    bake(imp_model, dt)
                                    
                                  },
                                  
                                  .predict_dt = function(dt, levels) {
                                    bake(self$state$bagimpute_model, dt)
                                  }
                                )
)

mlr_pipeops$add("tidy_bagimpute", PipeOpTidyBagimpute)