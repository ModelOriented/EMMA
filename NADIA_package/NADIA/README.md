# NADIA
NA Data Imputation Algorithms

# Motivation 
Package is mlr3 extensions create to help dealing with missing data. More about NADIA can be read here: [Imputing missing data in mlr3 with NADIA](https://medium.com/responsibleml/imputing-missing-data-with-NADIA-ed4cbc27510c). 

# Installation 
NADIA can be install using 
```
devtools::install_github("https://github.com/jjanborowka/NADIA/", subdir = "NaDIA_package/NADIA")
```
Package is not yet available on CRAN. 

# Example
NADIA working example using mlr3 
``` 
liblary(NADIA)
library(mlr3oml)
task <- mlr3oml::OMLTask$new(55)
task <- task$task

graph <- PipeOpMice$new()%>>% PipeOpEncodeImpact$new() %>>% lrn('classif.glmnet')
GRLR <- GraphLearner$new(graph)
resample(task,GRLR,rsmp('cv',folds=5))
```
