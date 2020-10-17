# EMMA
Evaluation of Methods for dealing with Missing data in Machine Learning algorithms

# Motivation 
Package is mlr3 extensions create to help dealing with missing data. More about EMMA can be read here: [Imputing missing data in mlr3 with EMMA](https://medium.com/responsibleml/imputing-missing-data-with-emma-ed4cbc27510c). 

# Installation 
EMMA can be install using 
```
devtools::install_github("https://github.com/jjanborowka/EMMA/", subdir = "EMMA_package/EMMA")
```
Package is not yet available on CRAN. 

# Example
EMMA working example using mlr3 
``` 
liblary(EMMA)
library(mlr3oml)
task <- mlr3oml::OMLTask$new(55)
task <- task$task

graph <- PipeOpMice$new()%>>% PipeOpEncodeImpact$new() %>>% lrn('classif.glmnet')
GRLR <- GraphLearner$new(graph)
resample(task,GRLR,rsmp('cv',folds=5))
```
