#Benchmark script for bambi server

#Output files

# result_csv <- "/data/user/result_grf.csv"
# error_out <- "/data/user/error_log_grf.txt"
# task_csv <- "/data/user/benchmark/tasks.csv"

result_csv <- "~/Pulpit/result_grf.csv"
error_out <- "~/Pulpit/error_log_grf.txt"
task_csv <- "benchmarks/tasks.csv"

#Packages
# library(OpenML)
# library(dplyr)
# library(devtools)
# library(mlr3)
# library(mlr3pipelines)
# library(paradox)
library(mlr3learners)
library(mlr3oml)

#Benchmark

#Tasks
tasks <- read.csv(task_csv)

#Pipelines
devtools::install_github("https://github.com/ModelOriented/EMMA", subdir = "/EMMA_package/EMMA", upgrade = FALSE)
library(EMMA)

#Flexible below, modify to evaluate right approach (a/b/c)
pipe_model <- LearnerClassifGrf

err_file <- file(error_out, "w")

for (task_id in tasks$task.id[c(5,6)]) {
      
    pipe_grf <- pipe_model$new()  
    
    graph <- po("removeconstants", id = "removeconstants_before") %>>% 
      po("collapsefactors", target_level_count = 20L) %>>%
      po("removeconstants", id = "removeconstants_after") %>>% 
      po("encodeimpact") %>>%
      pipe_grf
      
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
      scores$imputer <- pipe_grf$id
      scores$model <- pipe_grf$id
      
      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      rm(list = c("rr"))
      })
      
    
  }
}


close(err_file)

