#Benchmark script for bambi server

#Output files

result_csv <- "/data/user/result.csv"
error_out <- "/data/user/error_log.txt"
task_csv <- "/data/user/benchmark/tasks.csv"

#Packages
library(OpenML)
library(dplyr)
library(devtools)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(mlr3learners)
library(mlr3oml)

#Benchmark

set.seed(123)

#Tasks
tasks <- read.csv(task_csv)


#Pipelines
devtools::install_github("https://github.com/ModelOriented/EMMA", subdir = "/EMMA_package/EMMA", upgrade = FALSE)
library(EMMA)

#Flexible below, modify to evaluate right approach (a/b/c)
pipes_simple_num <- c(PipeOpImputeMean, PipeOpImputeMedian, PipeOpImputeHist)
pipes_simple_fac <- c(PipeOpImputeSample, PipeOpImputeMode, PipeOpImputeOOR)

pipes <- expand.grid(pipes_simple_num, pipes_simple_fac)

err_file <- file(error_out, open = "wt")

for (task_id in tasks$task.id) {
  
  for (j in 1:nrow(pipes)) {
  
    #Take pipes
    pipe_num <- pipes[[j, 1]]
    pipe_num <- pipe_num$new()
    
    pipe_fac <- pipes[[j, 2]]
    pipe_fac <- pipe_fac$new(param_vals = list("affect_columns" = selector_type(c("factor", "character"))))
    
    #Build model
    pipe_model <- lrn("classif.glmnet")
     
    graph <- po("removeconstants", id = "removeconstants_before") %>>% 
      po("collapsefactors", target_level_count = 20L) %>>%
      pipe_num %>>%
      pipe_fac %>>%
      po("removeconstants", id = "removeconstants_after") %>>% 
      po("encodeimpact") %>>%
      pipe_model
      
    graph_learner <- GraphLearner$new(graph)
    
    #Error handling
    graph_learner$encapsulate = c(train = "callr", predict = "callr")
    
    split <- rsmp("cv", folds = 5)
    
    #Task
    oml_task <- OMLTask$new(task_id)
    task <- oml_task$task
    
    sink(err_file, type = "message")
    try({
      rr <- resample(task, graph_learner, split)
    })
    sink(type = "message")
    
    
    if(exists("rr")){
      try({
      scores <- rr$score(msr("classif.acc"))
      scores <- scores[, c("iteration", "classif.acc")]
      scores$task <- task_id
      scores$imputer <- paste(pipe_num$id, pipe_fac$id, sep = "_")
      
      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      })
    }
    
    rm(list = "rr")
  }
}


close(err_file)

