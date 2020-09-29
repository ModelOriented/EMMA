#Benchmark script for bambi server

#Output files

result_csv <- "/data/user/result_packages.csv"
error_out <- "/data/user/error_log_packages.txt"
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

#Tasks
tasks <- read.csv(task_csv)

#Pipelines
devtools::install_github("https://github.com/ModelOriented/EMMA", subdir = "/EMMA_package/EMMA", upgrade = FALSE)
library(EMMA)

#Flexible below, modify to evaluate right approach (a/b/c)
pipes <- c(PipeOpAmelia, PipeOpmissForest, PipeOpMice, PipeOpSoftImpute, PipeOpmissRanger,
           PipeOpVIM_IRMI, PipeOpVIM_HD, PipeOpVIM_kNN, PipeOpVIM_regrImp, PipeOpMissMDA_MFA, PipeOpMissMDA_PCA_MCA_FMAD)

err_file <- file(error_out, open = "wt")

for (task_id in tasks$task.id) {
  
  for (j in 1:length(pipes)) {
    
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
      scores$imputer <- pipe_imp$id
      
      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      rm(list = c("rr"))
      })
    }
    
    sink(type = "message")
    
  }
}


close(err_file)

