#Packages
#devtools::install_github("https://github.com/jjanborowka/EMMA/", subdir = "EMMA_package/EMMA")
library(EMMA)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(OpenML)
library(mlr3learners)
library(mlr3oml)

pipes <- PipeOpMice_A
methods <- c("pmm", "midastouch", "sample", "cart", "rf")

tasks <- read.csv("EMMA_package/tests/round_3/task_sample.csv")
result_csv <- "EMMA_package/tests/mice_A/result.csv"

for (method in methods) {
  
  positive <- 0
  id <- paste(pipes$new()$id, method, sep = "_")
  f <- file(paste("EMMA_package/tests/mice_A/logs/", id, ".txt", sep = ""), open = "a")
  
  for (i in tasks$task.id) {
    
    #Take pipe
    pipe_imp <- pipes$new()
    pipe_imp$param_set$values$methods <- method
    
    #Build simple model
    pipe_model <- lrn("classif.glmnet")
    
    graph <- po("removeconstants", id = "removeconstants_before") %>>% 
      po("collapsefactors", target_level_count = 20L) %>>%
      pipe_imp %>>%
      po("removeconstants", id = "removeconstants_after") %>>% 
      po("encodeimpact") %>>%
      pipe_model
    
    graph_learner <- GraphLearner$new(graph)
    split <- rsmp("cv", folds = 5)
    
    #Test
    task_oml <- OMLTask$new(i)
    task <- task_oml$task
    
    sink(f, type = "output")
    sink(f, type = "message")
    options(warn = 1)
    
    try({
      rr <- resample(task, graph_learner, split)
    })
    
    if(exists("rr")){
      pred <- rr$prediction()
      if(length(pred$missing)==0){
        positive <- positive+1
        scores <- rr$score(msr("classif.acc"))
        scores <- scores[, c("iteration", "classif.acc")]
        scores$task <- i
        scores$imputer <- id
        
        write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      }else{
        write(file = f, "\n")
        rr$print()
        write(file = f, "PROBABLY LEFT MISSINGS AFTER IMPUTATION!")
      }
    }
    
    write(file = f, "\n")
    sink(type = "message")
    sink(type = "output")
    rm(list = "rr")
  }
  write(file = f, paste("Successful evaluation: ", as.character(positive), "/10 tasks", sep = ""))
  write(file = f, "\n")
  close(f)
}

