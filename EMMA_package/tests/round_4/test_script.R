#Packages
#devtools::install_github("https://github.com/jjanborowka/EMMA/", subdir = "EMMA_package/EMMA")
library(EMMA)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(OpenML)
library(mlr3learners)
library(mlr3oml)

# pipes <- c(PipeOpAmelia_T, PipeOpVIM_IRMI_T, PipeOpmissForest_T, PipeOpMice_T, PipeOpSoftImpute_T,
#            PipeOpVIM_HD_T, PipeOpVIM_kNN_T, PipeOpVIM_regrImp_T, PipeOpmissRanger_T)

# pipes <- c(PipeOpMissMDA_MFA, PipeOpMissMDA_PCA_MCA_FMAD)

pipes <- c(PipeOpmissForest_T, PipeOpMice_T, PipeOpmissRanger_T)


tasks <- read.csv("EMMA_package/tests/round_3/task_sample.csv")
tasks <- tasks[sample(10, 5), ]

for (j in 1:(length(pipes))) {
  positive <- 0
  id <- pipes[[j]]$new()$id
  f <- file(paste("EMMA_package/tests/round_4/logs_pipe_preproc/", id, ".txt", sep = ""), open = "a")
  
  for (i in tasks$task.id) {
    
    #Take pipe
    pipe_imp <- pipes[[j]]$new(optimize = TRUE)
    
    #Build simple model
    pipe_model <- lrn("classif.glmnet")
    pipe_encoding <- PipeOpEncodeImpact$new()
    graph <- pipe_imp %>>% pipe_encoding %>>% pipe_model
    graph_learner <- GraphLearner$new(graph)
    split <- rsmp("cv", folds = 5)
    
    #Test
    task <- mlr3oml::OMLTask$new(id = i)
    task <- task$task
    
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
        write(file = f, "\n")
        rr$print()
        write(file = f, "\n")
        print(rr$aggregate(msr("classif.ce")))
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
  write(file = f, paste("Successful evaluation: ", as.character(positive), "/5 tasks", sep = ""))
  write(file = f, "\n")
  close(f)
}

