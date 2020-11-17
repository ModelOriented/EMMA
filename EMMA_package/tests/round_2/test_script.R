#Packages
#devtools::install_github("https://github.com/jjanborowka/EMMA/", subdir = "EMMA_package/EMMA")
library(NADIA)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(OpenML)
library(mlr3learners)
library(mlr3oml)

pipes <- c(PipeOpMIDAS)

tasks <- read.csv("../tests/round_2/task_sample.csv")

for (j in 1:(length(pipes))) {
  positive <- 0

  for (i in tasks$task.id) {

    #Take pipe
    pipe_imp <- pipes[[j]]$new()

    #Build simple model
    pipe_model <- lrn("classif.glmnet")
    pipe_encoding <- PipeOpEncodeImpact$new()
    graph <- pipe_imp %>>% pipe_encoding %>>% pipe_model
    graph_learner <- GraphLearner$new(graph)
    split <- rsmp("cv", folds = 5)

    #Test
    f <- file(paste("/home/jan/Pulpit/", pipe_imp$id, ".txt", sep = ""), open = "a")
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
        print(rr$aggregate(msr("classif.acc")))
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
}


