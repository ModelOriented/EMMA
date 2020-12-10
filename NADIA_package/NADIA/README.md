
# NADIA
Na Data Imputation Algorithms

# Motivation 
Package is mlr3 extensions create to help dealing with missing data. More about NADIA can be read here: [Imputing missing data in mlr3 with NADIA](https://medium.com/responsibleml/imputing-missing-data-with-NADIA-ed4cbc27510c). 

# Installation 
NADIA can be install using 
``` 
install.packages("NADIA")
```


# Example
NADIA example using mlr3 
``` 
library(NADIA)
library(mlr3oml)
task <- mlr3oml::OMLTask$new(55)
task <- task$task

graph <- PipeOpMice$new()%>>% PipeOpEncodeImpact$new() %>>% lrn('classif.glmnet')
GRLR <- GraphLearner$new(graph)
resample(task,GRLR,rsmp('cv',folds=5))
```
