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

pipes <- c(PipeOpMissMDA_MFA_T, PipeOpMissMDA_PCA_MCA_FMAD_T)

for (j in 1:(length(pipes))) {
  positive <- 0
  
  for (i in c(13, 3793, 3667)) {

#Take pipe
pipe_imp <- pipes[[j]]$new()

#Build simple model
pipe_model <- lrn("classif.glmnet")
pipe_encoding <- PipeOpEncodeImpact$new()
graph <- pipe_imp %>>% pipe_encoding %>>% pipe_model
graph_learner <- GraphLearner$new(graph)
split <- rsmp("holdout")

#Test
f <- file(paste("EMMA_package/tests/round_1/logs_pipe_preproc_2/", pipe_imp$id, ".txt", sep = ""), open = "a")
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
    print(rr$score())
  }else{
    write(file = f, "\n")
    rr$print()
    write(file = f, "PROBABLY LEFT MISSINGS AFTER IMPUTATION!")
  }}

write(file = f, "\n\n")
sink(type = "message")
sink(type = "output")
rm(list = "rr")

  }
  write(file = f, paste("Successful evaluation: ", as.character(positive), "/3 tasks", sep = ""))
  write(file = f, "\n")
}

# pipes <- c(PipeOpAmelia, PipeOpVIM_IRMI, PipeOpmissForest, PipeOpMice, PipeOpSoftImpute, 
#            PipeOpVIM_HD, PipeOpVIM_kNN, PipeOpVIM_regrImp, PipeOpmissRanger)

pipes <- c(PipeOpMissMDA_MFA, PipeOpMissMDA_PCA_MCA_FMAD)

for (j in 1:(length(pipes))) {
  positive <- 0
  
  for (i in c(13, 3793, 3667)) {
    
    #Take pipe
    pipe_imp <- pipes[[j]]$new()
    
    #Build simple model
    pipe_model <- lrn("classif.glmnet")
    pipe_encoding <- PipeOpEncodeImpact$new()
    graph <- pipe_imp %>>% pipe_encoding %>>% pipe_model
    graph_learner <- GraphLearner$new(graph)
    split <- rsmp("holdout")
    
    #Test
    f <- file(paste("EMMA_package/tests/round_1/logs_2/", pipe_imp$id, ".txt", sep = ""), open = "a")
    task <- mlr3oml::OMLTask$new(id = i)
    task <- task$task
    
    sink(f, type = "output")
    sink(f, type = "message")
    options(warn = 1)
    
    try({
      rr <- resample(task, graph_learner, split)
    })
    
    if(exists("rr")){
      positive <- positive+1  
      write(file = f, "\n")
      rr$print()
      write(file = f, "\n")
      print(rr$score())
    }
    
    write(file = f, "\n\n")
    sink(type = "message")
    sink(type = "output")
    rm(list = "rr")
    
  }
  write(file = f, paste("Successful evaluation: ", as.character(positive), "/3 tasks", sep = ""))
  write(file = f, "\n")
}

source(file = "EMMA_package/tests/round_2/test_script.R")

