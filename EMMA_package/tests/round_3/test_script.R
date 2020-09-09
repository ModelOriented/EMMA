#Packages
#devtools::install_github("https://github.com/jjanborowka/EMMA/", subdir = "EMMA_package/EMMA")
library(EMMA)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(OpenML)
library(mlr3learners)
library(mlr3oml)

source("datasets_store/preprocess.R")

# pipes <- c(PipeOpAmelia_T, PipeOpVIM_IRMI_T, PipeOpmissForest_T, PipeOpMice_T, PipeOpSoftImpute_T,
#            PipeOpVIM_HD_T, PipeOpVIM_kNN_T, PipeOpVIM_regrImp_T, PipeOpmissRanger_T)

pipes <- c(PipeOpMissMDA_MFA, PipeOpMissMDA_PCA_MCA_FMAD)

# pipes <- c(PipeOpAmelia, PipeOpVIM_IRMI, PipeOpmissForest, PipeOpMice, PipeOpSoftImpute,
#            PipeOpVIM_HD, PipeOpVIM_kNN, PipeOpVIM_regrImp, PipeOpmissRanger)


# library(dplyr)
# library(OpenML)
# all_tasks <- listOMLTasks(limit = NULL)
# datasets_miss_id <- read.csv("datasets_store/datasets_selection/all_datasets_with_missings_id.csv")
# second_sample <- read.csv("EMMA_package/tests/round_2/task_sample.csv")
# first_sample <- read.csv("datasets_store/datasets_test_sample.csv")
# 
# tasks_miss_id = all_tasks %>%
#   filter(data.id %in% datasets_miss_id$ID,
#          task.type == "Supervised Classification",
#          ! (task.id %in% second_sample$task.id),
#          ! (data.id %in% first_sample$id),
#          ! (task.id %in% c(13, 3793, 3667)),
#          number.of.instances < 1000,
#          number.of.instances > 100) %>%
#   group_by(name) %>%
#   arrange(task.id) %>%
#   filter(row_number()==1) %>%
#   select(task.id, data.id, name, target.feature)

#tasks <- tasks_miss_id[sample(nrow(tasks_miss_id), 10), ]
#write.csv(tasks, file = "EMMA_package/tests/round_3/task_sample.csv", row.names = FALSE)

tasks <- read.csv("EMMA_package/tests/round_3/task_sample.csv")

for (j in 1:(length(pipes))) {
  positive <- 0
  id <- pipes[[j]]$new()$id
  f <- file(paste("EMMA_package/tests/round_3_no_cleaning/logs/", id, ".txt", sep = ""), open = "a")
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
    
    task_oml <- getOMLTask(i)
    df_oml <- task_oml$input$data.set
    #df_clean <- preprocess(df_oml)
    task <- TaskClassif$new(id = as.character(task_oml$task.id), backend = df_oml$data, target = task_oml$input$target.features)
    
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
  write(file = f, paste("Successful evaluation: ", as.character(positive), "/10 tasks", sep = ""))
  write(file = f, "\n")
  close(f)
}

