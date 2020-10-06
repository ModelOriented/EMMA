#Benchmark script for bambi server

#Output files

result_csv <- "benchmarks/trial_2/tidymodels_bagimpute.csv"
error_out <- "benchmarks/trial_2/error_tidymodels.txt"
task_csv <- "benchmarks/tasks.csv"

#Packages
library(OpenML)
library(dplyr)
library(devtools)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(mlr3learners)
library(mlr3oml)
source("benchmarks/tidymodels_bagimpute_pipe.R")

#Benchmark

#Tasks
tasks <- read.csv(task_csv)

#Pipelines

pipes <- c(PipeOpTidyBagimpute)

err_file <- file(error_out, open = "wt")

for (task_id in tasks$task.id) {
  
  for (j in 1:length(pipes)) {
    
    set.seed(1)
    
    #Take pipe
    pipe_imp <- pipes[[j]]$new()
    
    #Build model
    pipe_model <- lrn("classif.glmnet")
     
    graph <- po("removeconstants", id = "removeconstants_before") %>>% 
      po("collapsefactors", target_level_count = 20L) %>>%
      pipe_imp %>>%
      po("removeconstants", id = "removeconstants_after") %>>% 
      po("encodeimpact") %>>%
      pipe_model
      
    graph_learner <- GraphLearner$new(graph)
    
    split <- rsmp("cv", folds = 5)
    
    #Task
    oml_task <- OMLTask$new(task_id)
    task <- oml_task$task
    
    sink(err_file, type = "message")
    try({
      
      rr <- resample(task, graph_learner, split)
      
    })
    
    if(exists("rr")){
      try({
      scores <- rr$score(msr("classif.acc"))
      scores <- scores[, c("iteration", "classif.acc")]
      scores$task <- task_id
      scores$imputer <- pipe_imp$id
      
      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      })
    }
    
    sink(type = "message")
    
    rm(list = c("rr"))
  }
}


close(err_file)

