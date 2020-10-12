#Benchmark script for bambi server

#Output files

result_csv <- "benchmarks/trial_4/mice_glm.csv"
error_out <- "benchmarks/trial_4/error_mice_glm.txt"
task_csv <- "benchmarks/tasks.csv"

# result_csv <- "/data/user/trial_4/mice.csv"
# error_out <- "/data/user/trial_4/error_mice.txt"
# task_csv <- "/data/user/benchmark/tasks.csv"


#Packages
library(OpenML)
library(dplyr)
library(devtools)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(mlr3learners)
library(mlr3oml)
devtools::install_github("https://github.com/ModelOriented/EMMA/", subdir = "EMMA_package/EMMA", upgrade = FALSE)
library(EMMA)
library(xgboost)

#Benchmark

#Tasks
tasks <- read.csv(task_csv)

#Pipelines

pipes <- c(PipeOpMice_A, PipeOpMice)
methods <- c("pmm", "midastouch", "sample", "cart", "rf")

#Learners
#learners <- c(lrn("classif.glmnet"), lrn("classif.ranger"), lrn("classif.rpart"), lrn("classif.naive_bayes"), lrn("classif.xgboost"))
learners <- c(lrn("classif.glmnet"))

err_file <- file(error_out, open = "wt")

for (task_id in tasks$task.id) {
  
  for (j in 1:length(pipes)) {
    
    for (method in methods) {
      
      for(model in learners){
        

        #Take pipe
        pipe_imp <- pipes[[j]]$new()
        
        if(pipe_imp$id == "imput_mice_A"){
          pipe_imp$param_set$values$methods <- method
        }else{
          pipe_imp$param_set$values$set_method <- method
        }
        
        id <- paste(pipe_imp$id, method, sep = "_")
        
          
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
            scores$imputer <- id
            scores$model <- model$id
            
            write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
            rm(list = c("rr"))
          })
        }
        
        sink(type = "message")

      }
    }
  }
}


close(err_file)

