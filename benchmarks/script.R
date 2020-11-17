#Benchmark script for bambi server

#Output files

result_csv <- "/home/jan/Pulpit/result_MIDAS.csv"
error_out <- "/home/jan/Pulpit/error_midas.txt"


# result_csv <- "~/Pulpit/result_packages.csv"
# error_out <- "~/Pulpit/error_log_packages.txt"
# task_csv <- "benchmarks/tasks.csv"

#Packages
# library(OpenML)
# library(dplyr)
# library(devtools)
# library(mlr3)
# library(mlr3pipelines)
# library(paradox)
library(mlr3learners)
library(mlr3oml)

#Benchmark

#Tasks
tasks <- c(4,25,50,51,54,55,3021,3557,3561,3626,3675,3704,3719,3722,3761,3793,3807,3852,3856,3865,3870,3871,3881,3886,125920)

#Pipelines
#devtools::install_github("https://github.com/ModelOriented/EMMA", subdir = "/EMMA_package/EMMA", upgrade = FALSE)
library(NADIA)

#Flexible below, modify to evaluate right approach (a/b/c)
pipes <- c(PipeOpMIDAS)

err_file <- file(error_out, "w")

for (task_id in tasks) {

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
    graph_learner$predict_type <- "prob"
    graph_learner$encapsulate <- c(train='evaluate',predict='evaluate')
    split <- rsmp("cv", folds = 5)

    #Task
    next_step <- T
    while(next_step){
    tryCatch({
    next_step <- T
    oml_task <- OMLTask$new(task_id)
    task <- oml_task$task
    next_step <- F
    })
    }
    try({

        rr <- resample(task, graph_learner, split)

    })
    sink()
    sink(type="message")

    if(exists("rr")){
      try({
      scores <- rr$score(measures = c(msr("classif.auc"), msr("classif.acc"), msr("classif.fbeta")))
      scores <- scores[, c("iteration", "classif.acc", "classif.auc", "classif.fbeta")]
      scores$task <- task_id
      scores$imputer <- pipe_imp$id
      scores$model <- pipe_model$id

      write.table(scores, result_csv, sep = ",", col.names = !file.exists(result_csv), append = T)
      rm(list = c("rr"))
      })

    }
  }
}


close(err_file)

