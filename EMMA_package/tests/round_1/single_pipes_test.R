#Packages
#devtools::install_github("https://github.com/ModelOriented/EMMA/", subdir = "EMMA_package/EMMA")
library(EMMA)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(OpenML)
library(mlr3learners)
library(mlr3oml)

### Simple tests on single pipes
# task1 <- getOMLTask(task.id = 13)
# task2 <- getOMLTask(task.id = 3793)
# task3 <- getOMLTask(task.id = 3667)

pipes <- c(PipeOpAmelia_T, PipeOpVIM_IRMI_T, PipeOpmissForest_T, PipeOpMice_T, PipeOpSoftImpute_T, PipeOpVIM_HD_T,
           PipeOpVIM_kNN_T, PipeOpVIM_regrImp_T, PipeOpmissRanger_T)

for (j in 1:(length(pipes))) {
  for (i in c(13, 3793, 3667)) {

#Take pipe
pipe_imp <- pipes[[j]]$new()
#Check params and docs
# ?PipeOpMice
#?PipeOpmissRanger
# pipe_imp$param_set

#Build simple model
pipe_model <- lrn("classif.glmnet")
pipe_encoding <- PipeOpEncodeImpact$new()
graph <- pipe_imp %>>% pipe_encoding %>>% pipe_model
graph_learner <- GraphLearner$new(graph)
split <- rsmp("holdout")

#Test
f <- file(paste("EMMA_package/tests/logs_pipe_preproc/", pipe_imp$id, ".txt", sep = ""), open = "a")
task <- mlr3oml::OMLTask$new(id = i)
task <- task$task

sink(f, type = "output")
sink(f, type = "message")
options(warn = 1)

try({
rr <- resample(task, graph_learner, split)
})

if(exists("rr")){
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
}
