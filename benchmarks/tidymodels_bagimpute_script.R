#Benchmark script for bambi server

#Output files

result_csv <- "/data/user/result_bagimpute.csv"
error_out <- "/data/user/error_log_bagimpute.txt"
task_csv <- "/data/user/benchmark/tasks.csv"

# result_csv <- "~/Pulpit/result_bagimpute.csv"
# error_out <- "~/Pulpit/error_log_bagimpute.txt"
# task_csv <- "benchmarks/tasks.csv"

#Packages
# library(OpenML)
# library(dplyr)
# library(devtools)
# library(mlr3)
# library(mlr3pipelines)
# library(paradox)
library(mlr3learners)
library(mlr3oml)
library(xgboost)

devtools::install_github("https://github.com/ModelOriented/EMMA/", subdir = "EMMA_package/EMMA", upgrade = FALSE)
library(EMMA)

# source("/data/user/benchmark/tidymodels_bagimpute_pipe_A.R")
# source("/data/user/benchmark/tidymodels_bagimpute_pipe_B.R")
source("benchmarks/tidymodels_bagimpute_pipe_A.R")
source("benchmarks/tidymodels_bagimpute_pipe_B.R")

#Benchmark

#Tasks
tasks <- read.csv(task_csv)

#Pipelines

pipes <- c(PipeOpTidyBagimpute_A, PipeOpTidyBagimpute_B)

#Learners
learners <- c(lrn("classif.glmnet"), lrn("classif.ranger"), lrn("classif.rpart"), lrn("classif.naive_bayes"), lrn("classif.xgboost"))

err_file <- file(error_out, open = "wt")

for (task_id in tasks$task.id) {
  
  for (j in 1:length(pipes)) {
    
    for(model in learners){
  
    #Take pipe
    pipe_imp <- pipes[[j]]$new()
    
    #Build model
    pipe_model <- model
    
    if("factor" %in% model$feature_types){
      
      graph <- po("removeconstants", id = "removeconstants_before") %>>% 
        po("collapsefactors", target_level_count = 20L) %>>%
        pipe_imp %>>%
        po("removeconstants", id = "removeconstants_after") %>>% 
        pipe_model
      
    }else{
      
      graph <- po("removeconstants", id = "removeconstants_before") %>>% 
        po("collapsefactors", target_level_count = 20L) %>>%
        pipe_imp %>>%
        po("removeconstants", id = "removeconstants_after") %>>% 
        po("encodeimpact") %>>%
        pipe_model
      
    }
      
    graph_learner <- GraphLearner$new(graph)
    graph_learner$predict_type <- "prob"
    
    split <- rsmp("cv", folds = 5)
    
    #Task
    oml_task <- OMLTask$new(task_id)
    task <- oml_task$task
    
    try({
      mlr3misc::encapsulate("callr", {
        sink(err_file)
        sink(err_file, type = "message")
        set.seed(1)
        rr <- resample(task, graph_learner, split)
        sink()
        sink(type="message")
      }, .pkgs = "EMMA")
    })
    sink()
    sink(type="message")
    
    if(exists("rr")){
      try({
        scores <- rr$score(measures = c(msr("classif.auc"), msr("classif.acc"), msr("classif.fbeta")))
        scores <- scores[, c("iteration", "classif.acc", "classif.auc", "classif.fbeta")]
        scores$task <- task_id
        scores$imputer <- pipe_imp$id
        scores$model <- model$id
        
        write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
        rm(list = c("rr"))
      })
    }
    
    }
  }
}


close(err_file)

