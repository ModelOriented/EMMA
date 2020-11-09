# NaDIA
Na Data Imputation Algorithms

# Motivation 
Package is mlr3 extensions create to help dealing with missing data. More about NaDIA can be read here: [Imputing missing data in mlr3 with NaDIA](https://medium.com/responsibleml/imputing-missing-data-with-NaDIA-ed4cbc27510c). 

# Installation 
NaDIA can be install using 
```
devtools::install_github("https://github.com/jjanborowka/NaDIA/", subdir = "NaDIA_package/NaDIA")
```
Package is not yet available on CRAN. 

# Example
NaDIA working example using mlr3 
``` 
liblary(NaDIA)
library(mlr3oml)
task <- mlr3oml::OMLTask$new(55)
task <- task$task

graph <- PipeOpMice$new()%>>% PipeOpEncodeImpact$new() %>>% lrn('classif.glmnet')
GRLR <- GraphLearner$new(graph)
resample(task,GRLR,rsmp('cv',folds=5))
```
