## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,include=FALSE------------------------------------------------------
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	include = FALSE
)
library(NADIA)

## ---- include=TRUE,eval=FALSE,class.source="code"-----------------------------
#  devtools::install_github("https://github.com/ModelOriented/EMMA/", subdir = "NADIA_package/NADIA")
#  

## ----echo=TRUE,include=TRUE,eval=FALSE----------------------------------------
#  install.packages("NADIA")
#  

## ----echo=FALSE, out.width='50%', include=TRUE,fig.align='center'-------------
knitr::include_graphics('A.png')

## ----echo=FALSE, fig.align='center', include=TRUE, out.width='50%'------------
knitr::include_graphics('B.png')

## ----chunk1, class.source="code", include=TRUE, paged.print=TRUE--------------
# Task with missing data from mlr3
task_with_missing <- tsk('pima')


# Creating an operator implementing the imputation method
imputation_methods <- PipeOpMice$new()

# Imputation
task_with_no_missing <- imputation_methods$train(list(task_with_missing))[[1]]

#Check

task_with_missing$missings()

task_with_no_missing$missings()


## ----chunk2, include=TRUE, class.source="code"--------------------------------
library(mlr3learners)

# Creating graph learner

# imputation method 
imp <- PipeOpmissRanger$new()


# learner 
learner <- lrn('classif.glmnet')

graph <- imp %>>%  learner

graph_learner <- GraphLearner$new(graph, id = 'missRanger.learner')
graph_learner$id <-  'missRanger.learner'
# resampling 
set.seed(1)
resample(tsk('pima'),graph_learner,rsmp('cv',folds=5))

## ----chunk3,include=TRUE, dependson= -1 , class.source="code"-----------------
# Error handling 
graph_learner$encapsulate <- c(train='evaluate',predict='evaluate')

# Creating a problematic task
data <- iris

data[,1] <- NA

task_problematic <- TaskClassif$new('task',data,'Species')


# Resampling 

# All folds will be tested and the script run further

set.seed(1)
resample(task_problematic,graph_learner,rsmp('cv',folds=5))

  


## ----chunk4, include=TRUE,dependson=-2 , class.source="code"------------------

# Turning off encapsulation 
graph_learner$encapsulate <- c(train='none',predict='none')

# Turning on optimalization 
graph_learner$param_set$values$impute_missRanger_B.optimize <- TRUE


# Resampling 
set.seed(1)
resample(tsk('pima'),graph_learner,rsmp('cv',folds=5))


## ----include=TRUE , class.source="code"---------------------------------------

# Creating graph learner

# imputation method 
imp <- PipeOpMean_B$new()

# learner 
learner <- lrn('classif.glmnet')

graph <- imp %>>% learner

graph_learner <- GraphLearner$new(graph)
graph_learner$id <-  'mean.learner'
# resampling 
set.seed(1)
resample(tsk('pima'),graph_learner,rsmp('cv',folds=5))


## ----echo=TRUE,include=TRUE---------------------------------------------------
library(missMDA)
library(mlr3learners)
# Using task form mlr3 
task <- tsk("pima")

# I can't perform imputation on task so I have to extract data frame  

data <- as.data.frame(task$data())

# Splitting into two sets and removing the target column 

indx <- sample(1:nrow(data),nrow(data)/2)

data1 <- data[indx,-1]

data2 <- data[-indx,-1]

##  Performing imputation with optimization
 
  # Features are only numeric so I will use PCA this has to be checked 
  # Optimization  

ncp1 <- estim_ncpPCA(data1)$ncp

ncp2 <- estim_ncpPCA(data2)$ncp
 
  # Imputation 

data1 <- as.data.frame(imputePCA(data1,ncp1)$completeObs)

data2 <- as.data.frame(imputePCA(data2,ncp2)$completeObs)

# Adding back target column 

data1$diabetes <- data$diabetes[indx]

data2$diabetes <- data$diabetes[-indx]

# Creating new tasks to make a prediction 

task1 <- TaskClassif$new("t1",data1,"diabetes")

task2 <- TaskClassif$new("t2",data2,"diabetes")

# Training, prediction, and evaluation  
# Fold1 
learner <- lrn("classif.glmnet")

learner$train(task1)

p2 <- learner$predict(task2)

acc2 <- p2$score(msr("classif.acc"))

# Fold2
learner <- lrn("classif.glmnet")

learner$train(task2)

p1 <- learner$predict(task1)

acc1 <- p1$score(msr("classif.acc"))

# Mean acc
(acc1+acc2)/2

## ----echo=TRUE,include=TRUE---------------------------------------------------
library(mlr3learners)
# Using task form mlr3 
task <- tsk("pima")

# Imputation, training, prediction, and evaluation 

graph <- PipeOpMissMDA_PCA_MCA_FMAD$new() %>>% lrns("classif.glmnet")

graph_learner <- GraphLearner$new(graph)

graph_learner$id <- 'learner'

rr <- resample(task,graph_learner,rsmp("cv",folds=2))

rr$aggregate(msr("classif.acc"))


