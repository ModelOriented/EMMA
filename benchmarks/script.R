#Benchmark script for bambi server

#Output files

result_csv <- "~/home/user/result.csv"
error_out <- "~/home/user/error_log.txt"

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

set.seed(123)

#Tasks
all_tasks <- listOMLTasks(limit = NULL)
tasks <- filter(all_tasks, 
                task.type == "Supervised Classification",
                number.of.classes == 2,
                number.of.missing.values > 0) %>% 
  group_by(name) %>%
  arrange(task.id) %>%
  filter(row_number()==1) %>%
  select(task.id, data.id, name, target.feature, number.of.features, number.of.instances, number.of.missing.values) %>%
  #Only datasets with high percentage of missings 
  filter(number.of.missing.values/(number.of.features*number.of.instances) > 0.05)

#Pipelines
devtools::install_github("https://github.com/ModelOriented/EMMA", subdir = "/EMMA_package/EMMA")
library(EMMA)

#Flexible below, modify to evaluate right approach (a/b/c)
pipes_packages <- c(PipeOpAmelia, PipeOpmissForest, PipeOpMice, PipeOpSoftImpute, PipeOpmissRanger, 
                      PipeOpVIM_IRMI, PipeOpVIM_HD, PipeOpVIM_kNN, PipeOpVIM_regrImp, PipeOpMissMDA_MFA, PipeOpMissMDA_PCA_MCA_FMAD)

pipes_simple <- c()

pipes <- c(pipes_packages, pipes_simple)

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
    
    #Error handling
    graph_learner$encapsulate = c(train = "callr", predict = "callr")
    
    split <- rsmp("cv", folds = 5)
    
    #Task
    oml_task <- OMLTask$new(task_id)
    task <- oml_task$task
    
    sink(err_file, type = "message")
    try({
      rr <- resample(task, graph_learner, split)
    })
    sink(type = "message")
    
    
    if(exists("rr")){
      try({
      scores <- rr$score(msr("classif.acc"))
      scores <- scores[, c("iteration", "classif.acc")]
      scores$task <- task_id
      scores$imputer <- pipe_imp$id
      
      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      })
    }
    
    rm(list = "rr")
  }
}


close(err_file)

