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
task1 <- getOMLTask(task.id = 13)
task2 <- getOMLTask(task.id = 3793)
task3 <- getOMLTask(task.id = 3667)

for (i in c(13, 3793, 3667)) {

#Take pipe
pipe_imp <- EMMA::PipeOpMice$new()
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
f <- file("EMMA_package/tests/logs/softimpute.txt", open = "a")
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
sink()
sink()
rm(list = ls())

}

