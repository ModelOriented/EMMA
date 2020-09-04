library(OpenML)
library("mlr3tuning")
library(EMMA)
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(OpenML)
library(mlr3learners)
library(mlr3oml)

source("datasets_store/preprocess.R")
####Setings#####

out_file <- 'EMMA_package/tests/round_4/logs_amelia_empir/log_amelia_empir.txt' #out put file location
n <- 10 # Number of try for RandomSearch
# datasets ID for test
tasks <- read.csv("EMMA_package/tests/round_3/task_sample.csv")
tasks <- tasks[tasks$task.id %in% c(3802, 54, 3547, 3705, 3604), ]
datasets <- tasks$data.id

error_csv_location <- 'EMMA_package/tests/round_4/logs_amelia_empir/log_amelia_empir_errors.csv'

#### LOG CRESTING #####
write(paste0('LOG_Amelia_empir_test',Sys.Date()),file = out_file)

#### TEST LOOP ###
for (id in datasets){

  df_oml <- getOMLDataSet(id)
  df <- preprocess(df_oml, 0.9)[[1]]

  n_rows <- nrow(df)
  empir_set <- seq(0.01, 0.13, by = 0.01)
  
  for (empir in empir_set) {
    
  
  write(paste0('-------------------df___',id,'---------------------------------'),file = out_file,append = T)
  write(paste0('-------------------empir:__',empir,'---------------------------------'),file = out_file,append = T)
    
  cat('Imputation on whole dataset  :',file = out_file, append = T)

  tryCatch({
    task <- TaskClassif$new(id = as.character(id), backend = df, target = df_oml$target.features)
    
    pipe_imp <- PipeOpAmelia$new(empir = empir)
    pipe_model <- lrn("classif.glmnet")
    pipe_encoding <- PipeOpEncodeImpact$new()
    graph <- pipe_imp %>>% pipe_encoding %>>% po("removeconstants") %>>% pipe_model
    graph_learner <- GraphLearner$new(graph)
    set.seed(123)
    split <- rsmp("cv", folds = 5)
    rr <- resample(task, graph_learner, split)
    rr$aggregate(msr("classif.ce"))
    
    write('OK',file = out_file,append = T)
    
  },error=function(e){
    write(as.character(e),file = out_file,append = T)

  })
  }
}

