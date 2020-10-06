#Benchmark script for bambi server

#Output files

result_csv <- "/data/user/result_simple.csv"
error_out <- "/data/user/error_log_simple.txt"
task_csv <- "/data/user/benchmark/tasks.csv"

# result_csv <- "~/Pulpit/result_simple_xyz.csv"
# error_out <- "~/Pulpit/error_log_simple_xyz.txt"
# task_csv <- "benchmarks/tasks.csv"

#Packages
library(dplyr)
library(devtools)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(mlr3learners)
library(mlr3oml)

#Benchmark

#Tasks
tasks <- read.csv(task_csv)

#Pipelines
devtools::install_github("https://github.com/ModelOriented/EMMA", subdir = "/EMMA_package/EMMA", upgrade = FALSE)
library(EMMA)

#Flexible below, modify to evaluate right approach (a/b/c)
pipes_simple_num <- c(PipeOpImputeMean, PipeOpImputeMedian, PipeOpImputeHist, PipeOpImputeSample, PipeOpImputeOOR, PipeOpImputeMode)
pipes_simple_fac <- c(PipeOpImputeSample, PipeOpImputeOOR, PipeOpImputeMode)

simple_a <- expand.grid(pipes_simple_num, pipes_simple_fac)

pipes_simple_num <- c(PipeOpMean_B, PipeOpMedian_B, PipeOpHist_B, PipeOpSample_B, PipeOpOOR_B, PipeOpMode_B)
pipes_simple_fac <- c(PipeOpSample_B, PipeOpOOR_B, PipeOpMode_B)

simple_b <- expand.grid(pipes_simple_num, pipes_simple_fac)

pipes <- rbind(simple_a, simple_b)


#Learners
learners <- c(lrn("classif.glmnet"), lrn("classif.ranger"), lrn("classif.rpart"), lrn("classif.naive_bayes"))


err_file <- file(error_out, open = "wt")

for (task_id in tasks$task.id) {
  
  for (j in 1:nrow(pipes)) {
    
    for (model in learners) {

    #Take pipes
    pipe_num <- pipes[[j, 1]]
    pipe_num <- pipe_num$new(param_vals = list("affect_columns" = selector_type(c("integer", "numeric"))))
    pipe_num$id <- paste(pipe_num$id, "num", sep = "_")
    
    pipe_fac <- pipes[[j, 2]]
    pipe_fac <- pipe_fac$new(param_vals = list("affect_columns" = selector_type(c("factor", "logical", "ordered"))))
    pipe_fac$id <- paste(pipe_fac$id, "fac", sep = "_")
    
    #Build model
    pipe_model <- model
     
    if("factor" %in% model$feature_types){
      
      graph <- po("removeconstants", id = "removeconstants_before") %>>% 
        po("collapsefactors", target_level_count = 20L) %>>%
        pipe_num %>>%
        pipe_fac %>>%
        po("removeconstants", id = "removeconstants_after") %>>% 
        pipe_model
      
    }else{
      
      graph <- po("removeconstants", id = "removeconstants_before") %>>% 
        po("collapsefactors", target_level_count = 20L) %>>%
        pipe_num %>>%
        pipe_fac %>>%
        po("removeconstants", id = "removeconstants_after") %>>% 
        po("encodeimpact") %>>%
        pipe_model
      
    }
    
    graph_learner <- GraphLearner$new(graph)
    
    split <- rsmp("cv", folds = 5)
    
    #Task
    oml_task <- OMLTask$new(task_id)
    task <- oml_task$task
    
    sink(err_file, type = "message")
    try({
      
      mlr3misc::encapsulate("callr", {
        set.seed(1)
        rr <- resample(task, graph_learner, split)
      }, .pkgs = "EMMA")
      
    })
    
    
    if(exists("rr")){
      try({
      scores <- rr$score(msr("classif.acc"))
      scores <- scores[, c("iteration", "classif.acc")]
      scores$task <- task_id
      scores$imputer <- paste(pipe_num$id, pipe_fac$id, sep = "_")
      scores$model <- model$id
      
      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      rm(list = c("rr"))
      })
    }
    
    sink(type = "message")
    
    }
  }
}


close(err_file)

