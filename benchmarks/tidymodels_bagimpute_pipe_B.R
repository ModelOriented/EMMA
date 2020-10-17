library(tidymodels)

PipeOpTidyBagimpute_B = R6::R6Class("PipeOpTidyBagimpute_B",
                                  inherit = mlr3pipelines::PipeOpTaskPreproc,
                                  public = list(
                                    initialize = function(id = "impute_tidy_bagimpute_B") {
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
                                      
                                      bake(imp_model, dt)
                                      
                                    },
                                    
                                    .predict_dt = function(dt, levels) {
                                      
                                      vars <- colnames(dt)
                                      
                                      imputer <- recipe(dt) %>% 
                                        update_role(all_of(vars), new_role = "predictor") %>%
                                        step_bagimpute(all_predictors())
                                      
                                      imp_model <- prep(imputer, dt)
                              
                                      bake(imp_model, dt)
                                    }
                                  )
)

mlr_pipeops$add("tidy_bagimpute_B", PipeOpTidyBagimpute_B)