pkgname <- "NADIA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "NADIA-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('NADIA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("PipeOpAmelia")
### * PipeOpAmelia

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpAmelia
### Title: PipeOpAmelia
### Aliases: PipeOpAmelia

### ** Examples

{

# Using debug learner for example purpose

  graph <- PipeOpAmelia$new() %>>% LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpAmelia", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpHist_B")
### * PipeOpHist_B

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpHist_B
### Title: PipeOpHist_B
### Aliases: PipeOpHist_B

### ** Examples

{

# Using debug learner for example purpose

  graph <- PipeOpHist_B$new() %>>%  LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpHist_B", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpMean_B")
### * PipeOpMean_B

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpMean_B
### Title: PipeOpMean_B
### Aliases: PipeOpMean_B

### ** Examples

{

 # Using debug learner for example purpose

  graph <- PipeOpMean_B$new() %>>% LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpMean_B", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpMedian_B")
### * PipeOpMedian_B

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpMedian_B
### Title: PipeOpMedian_B
### Aliases: PipeOpMedian_B

### ** Examples

{

# Using debug learner for example purpose

  graph <- PipeOpMedian_B$new() %>>%  LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpMedian_B", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpMice")
### * PipeOpMice

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpMice
### Title: PipeOpMice
### Aliases: PipeOpMice

### ** Examples

{

# Using debug learner for example purpose

  graph <- PipeOpMice$new() %>>%  LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpMice", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpMice_A")
### * PipeOpMice_A

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpMice_A
### Title: PipeOpMice_A
### Aliases: PipeOpMice_A

### ** Examples

## Not run: 
##D 
##D  # Using debug learner for example purpose
##D 
##D   graph <- PipeOpMice_A$new() %>>% LearnerClassifDebug$new()
##D   graph_learner <- GraphLearner$new(graph)
##D 
##D   # Task with NA
##D 
##D   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpMice_A", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpMode_B")
### * PipeOpMode_B

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpMode_B
### Title: PipeOpMode_B
### Aliases: PipeOpMode_B

### ** Examples

{
 # Using debug learner for example purpose

  graph <- PipeOpMode_B$new() %>>% LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpMode_B", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpOOR_B")
### * PipeOpOOR_B

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpOOR_B
### Title: PipeOpOOR_B
### Aliases: PipeOpOOR_B

### ** Examples

{

 # Using debug learner for example purpose

  graph <- PipeOpOOR_B$new() %>>% LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpOOR_B", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpSample_B")
### * PipeOpSample_B

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpSample_B
### Title: PipeOpSample_B
### Aliases: PipeOpSample_B

### ** Examples

{
  graph <- PipeOpSample_B$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpSample_B", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpSimulateMissings")
### * PipeOpSimulateMissings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpSimulateMissings
### Title: PipeOpSimulateMissings
### Aliases: PipeOpSimulateMissings

### ** Examples

{
  task_NA <- PipeOpSimulateMissings$new()$train(list(tsk("iris")))[[1]]

  # check
  sum(task_NA$missings()) > 0
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpSimulateMissings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpSoftImpute")
### * PipeOpSoftImpute

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpSoftImpute
### Title: PipeOpSoftImpute
### Aliases: PipeOpSoftImpute

### ** Examples

{
  graph <- PipeOpAmelia$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpSoftImpute", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpVIM_HD")
### * PipeOpVIM_HD

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpVIM_HD
### Title: PipeOpVIM_HD
### Aliases: PipeOpVIM_HD

### ** Examples

{
  graph <- PipeOpVIM_HD$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpVIM_HD", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpVIM_IRMI")
### * PipeOpVIM_IRMI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpVIM_IRMI
### Title: PipeOpVIM_IRMI
### Aliases: PipeOpVIM_IRMI

### ** Examples

{
  graph <- PipeOpVIM_IRMI$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpVIM_IRMI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpVIM_kNN")
### * PipeOpVIM_kNN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpVIM_kNN
### Title: PipeOpVIM_kNN
### Aliases: PipeOpVIM_kNN

### ** Examples

{
  graph <- PipeOpVIM_kNN$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpVIM_kNN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpVIM_regrImp")
### * PipeOpVIM_regrImp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpVIM_regrImp
### Title: PipeOpVIM_regrImp
### Aliases: PipeOpVIM_regrImp

### ** Examples

{
  graph <- PipeOpVIM_regrImp$new() %>>% mlr3learners::LearnerClassifGlmnet$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpVIM_regrImp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpmissForest")
### * PipeOpmissForest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpmissForest
### Title: PipeOpmissForest
### Aliases: PipeOpmissForest

### ** Examples

## Not run: 
##D 
##D   # Using debug learner for example purpose
##D 
##D   graph <- PipeOpmissForest$new() %>>% LearnerClassifDebug$new()
##D   graph_learner <- GraphLearner$new(graph)
##D 
##D   # Task with NA
##D 
##D   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpmissForest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpmissMDA_MFA")
### * PipeOpmissMDA_MFA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpmissMDA_MFA
### Title: PipeOpmissMDA_MFA
### Aliases: PipeOpmissMDA_MFA PipeOpMissMDA_MFA

### ** Examples

{

 # Using debug learner for example purpose

  graph <- PipeOpMissMDA_MFA$new() %>>% LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA

  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpmissMDA_MFA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpmissMDA_PCA_MCA_FMAD")
### * PipeOpmissMDA_PCA_MCA_FMAD

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpmissMDA_PCA_MCA_FMAD
### Title: PipeOpmissMDA_PCA_MCA_FMAD
### Aliases: PipeOpmissMDA_PCA_MCA_FMAD PipeOpMissMDA_PCA_MCA_FMAD

### ** Examples

{

 # Using debug learner for example purpose


  graph <- PipeOpMissMDA_PCA_MCA_FMAD$new() %>>% LearnerClassifDebug$new()
  graph_learner <- GraphLearner$new(graph)

  # Task with NA
  set.seed(1)
  resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpmissMDA_PCA_MCA_FMAD", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PipeOpmissRanger")
### * PipeOpmissRanger

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PipeOpmissRanger
### Title: PipeOpmissRanger
### Aliases: PipeOpmissRanger

### ** Examples

## Not run: 
##D 
##D  # Using debug learner for example purpose
##D 
##D   graph <- PipeOpmissRanger$new() %>>% LearnerClassifDebug$new()
##D   graph_learner <- GraphLearner$new(graph)
##D 
##D   # Task with NA
##D 
##D   resample(tsk("pima"), graph_learner, rsmp("cv", folds = 3))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PipeOpmissRanger", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_Amelia")
### * autotune_Amelia

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_Amelia
### Title: Perform imputation using Amelia package and EMB algorithm.
### Aliases: autotune_Amelia

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_Amelia(raw_data, col_type, percent_of_missing)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_Amelia", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_VIM_Irmi")
### * autotune_VIM_Irmi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_VIM_Irmi
### Title: Perform imputation using VIM package and irmi function
### Aliases: autotune_VIM_Irmi

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_VIM_Irmi(raw_data, col_type, percent_of_missing)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_VIM_Irmi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_VIM_hotdeck")
### * autotune_VIM_hotdeck

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_VIM_hotdeck
### Title: Hot-Deck imputation using VIM package.
### Aliases: autotune_VIM_hotdeck

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_VIM_hotdeck(raw_data, percent_of_missing)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_VIM_hotdeck", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_VIM_kNN")
### * autotune_VIM_kNN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_VIM_kNN
### Title: K nearest neighbor imputation using VIM package.
### Aliases: autotune_VIM_kNN

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_VIM_kNN(raw_data, percent_of_missing)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_VIM_kNN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_VIM_regrImp")
### * autotune_VIM_regrImp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_VIM_regrImp
### Title: Perform imputation using VIM package and regressionImp function.
### Aliases: autotune_VIM_regrImp

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_VIM_regrImp(raw_data, col_type, percent_of_missing)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_VIM_regrImp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_mice")
### * autotune_mice

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_mice
### Title: Automatical tuning of parameters and imputation using mice
###   package.
### Aliases: autotune_mice

### ** Examples

{
  raw_data <- mice::nhanes2

  col_type <- 1:ncol(raw_data)
  for (i in col_type) {
    col_type[i] <- class(raw_data[, i])
  }

  percent_of_missing <- 1:ncol(raw_data)
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }
  col_no_miss <- colnames(raw_data)[percent_of_missing == 0]
  col_miss <- colnames(raw_data)[percent_of_missing > 0]
  imp_data <- autotune_mice(raw_data, optimize = FALSE, iter = 2,
   col_type = col_type, percent_of_missing = percent_of_missing,
   col_no_miss = col_no_miss, col_miss = col_miss)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_mice", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_missForest")
### * autotune_missForest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_missForest
### Title: Perform imputation using missForest form missForest package.
### Aliases: autotune_missForest

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_missForest(raw_data, col_type, percent_of_missing, optimize = FALSE)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_missForest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_missRanger")
### * autotune_missRanger

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_missRanger
### Title: Perform imputation using missRenger form missRegnger package.
### Aliases: autotune_missRanger

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_missRanger(raw_data, percent_of_missing, optimize = FALSE)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_missRanger", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("autotune_softImpute")
### * autotune_softImpute

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: autotune_softImpute
### Title: Perform imputation using softImpute package
### Aliases: autotune_softImpute

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- autotune_softImpute(raw_data, percent_of_missing, col_type)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("autotune_softImpute", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("missMDA_FMAD_MCA_PCA")
### * missMDA_FMAD_MCA_PCA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: missMDA_FMAD_MCA_PCA
### Title: Perform imputation using MCA, PCA, or FMAD algorithm.
### Aliases: missMDA_FMAD_MCA_PCA

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- missMDA_FMAD_MCA_PCA(raw_data, col_type, percent_of_missing, optimize_ncp = FALSE)
  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("missMDA_FMAD_MCA_PCA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("missMDA_MFA")
### * missMDA_MFA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: missMDA_MFA
### Title: Perform imputation using MFA algorithm.
### Aliases: missMDA_MFA

### ** Examples

{
  raw_data <- data.frame(
    a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
    b = as.integer(1:1000),
    c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
    d = runif(1000, 1, 10),
    e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
    f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))

  # Prepering col_type
  col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")

  percent_of_missing <- 1:6
  for (i in percent_of_missing) {
    percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
  }


  imp_data <- missMDA_MFA(raw_data, col_type, percent_of_missing)

  # Check if all missing value was imputed
  sum(is.na(imp_data)) == 0
  # TRUE
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("missMDA_MFA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulate_missings")
### * simulate_missings

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulate_missings
### Title: Generate MCAR missings in dataset.
### Aliases: simulate_missings

### ** Examples

{
  data_NA <- simulate_missings(iris, 20)

  # check
  sum(is.na(data_NA)) > 0
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulate_missings", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
