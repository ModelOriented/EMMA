#Benchmark script for bambi server

#Output files

result_csv <- "~/Pulpit/test.csv"
error_out <- "~/Pulpit/error_log.txt"

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
pipes_packages <- c(PipeOpAmelia, PipeOpmissForest, PipeOpMice, PipeOpSoftImpute, PipeOpmissRanger, 
                      PipeOpVIM_IRMI, PipeOpVIM_HD, PipeOpVIM_kNN, PipeOpVIM_regrImp)

pipes_simple <- c()

pipes <- c(pipes_packages, pipes_simple)

for (task_id in 50) {
  
  #for (j in 1:length(pipes)) {
  for (j in 1) {
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
    
    tryCatch({
      rr <- resample(task, graph_learner, split)
    }, error = function(e) {
      writeLines(paste(as.character(task_id), as.character(pipe_imp$id), "\n"), error_out)
      writeLines(paste(as.character(e), "\n"), error_out)
    })
    
    
    if(exists("rr")){
      scores <- rr$score(msr("classif.acc"))
      scores <- scores[, c("iteration", "classif.acc")]
      scores$task <- task_id
      scores$imputer <- pipe_imp$id
      
      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
    }
    
    rm(list = "rr")
  }
}


close(error_out)

