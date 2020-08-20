library(dplyr)
library(janitor)
library(tidyselect)
library(EMMA)
library(mlr3learners)
library(OpenML)
library("mlr3tuning")


####Setings#####

out_file <- 'log_mice.txt' #out put file location
n <- 10 # Number of try for RandomSearch
datasets <- c(13,15,24,927,1111,1053) # datasets ID for test


#### Pre PROCESING ####
preprocess <- function(df_oml, miss_in_var_threshold = 0.9) {
  ### Params
  # - df_oml: object from getOMLDataSet()
  # - miss_in_var_threshold: values [0, 1]; defines threshold of missings in columns to remove
  ### Output
  # - df: ready, cleaned dataframe
  # - data_types_troubles: flag for column types trouble
  # - miss_in_target: flag for missing values in target variable

  df <- df_oml$data
  df_desc <- df_oml$desc
  id <- df_desc$id

  ### Preprocessing step ###

  check_type <- function(x){
    is.numeric(x) | is.factor(x)
  }

  #Flags
  data_types_troubles <- FALSE
  miss_in_target <- FALSE

  #Columns to ignore according to tags
  to_ignore <- df_desc$ignore.attribute
  if (all(!is.na(to_ignore))) {
    df <- select(df,-all_of(to_ignore))
  }

  #Removing row ID column according to tags
  row_id <- df_desc$row.id.attribute
  row_id <- row_id[!row_id %in% to_ignore]
  if (all(!is.na(row_id))) {
    df <- select(df,-all_of(row_id))
  }

  #Data types
  data_types_check <- sapply(df, check_type)

  if (any(!data_types_check)) {
    wrong_columns <- colnames(df)[!data_types_check]

    #Trying possible conversions
    for (col in wrong_columns) {
      if (is.logical(df[, col]) | is.character(df[, col])) {
        df[, col] <- as.factor(df[, col])
      } else{
        warning(paste("Data type trouble ID: ", id))
        data_types_troubles <- TRUE
      }
    }
  }

  #Removing possible duplicate rows
  df <- df[!duplicated(df),]

  #Removing constant columns
  df <- remove_constant(df)

  #Removing empty columns
  df <- remove_empty(df, "cols")

  #All to lowercase
  df <- mutate_if(df, is.factor, function(x)
    factor(tolower(x)))

  #Missing values in target variable
  target <- df_oml$target.features
  if (any(is.na(df[, target]))) {
    miss_in_target <- TRUE
    warning(paste("Missing values in target variable ID: ", id))
  }

  #Removing columns with % of missing higher than "miss_in_var_threshold"
  df <- df[, which(colMeans(!is.na(df)) > miss_in_var_threshold)]

  #Categorical columns with high fraction of unique values
  #?

  return(list("df" = df,
              "data_types_troubles" = data_types_troubles,
              "miss_in_target" = miss_in_target))
}


#### LOG CRESTING #####
write(paste0('LOG_micetest',Sys.Date()),file = out_file)


#### TEST LOOP ###
for (id in datasets){

  df_oml <- getOMLDataSet(id)

  df <- preprocess(df_oml,0.9)[[1]]



  col_type <- 1:ncol(df)
  for ( i in col_type){
    col_type[i] <- class(df[,i])
  }

  percent_of_missing  <- 1:ncol(df)
  for ( i in percent_of_missing){
    percent_of_missing[i] <- (sum(is.na(df[,i]))/length(df[,1]))*100
  }
  write(paste0('-------------------------',id,'---------------------------------'),file = out_file,append = T)

  cat('Imputation on whole dataset  :',file = out_file,append = T)
  tryCatch({
    autotune_mice(df,col_miss = colnames(df)[percent_of_missing>0],col_no_miss =colnames(df)[percent_of_missing==0],percent_of_missing = percent_of_missing,col_type = col_type,
                  optimize = F)
    write('OK',file = out_file,append = T)
  },error=function(e){
    write(as.character(e),file = out_file,append = T)
  }

  )

  learner_po = po("learner", learner = lrn("classif.rpart"))
  test_imp <- PipeOpMice$new()
  test = test_imp %>>%  learner_po
  glrn =GraphLearner$new(test)

  test_task = TaskClassif$new('test',backend = df,target = df_oml$target.features)

  write('----random search----')

  tune_ps<- ParamSet$new(params = list(
    ParamDbl$new('imput_mice.set_cor',lower = 0.01,upper = 0.99),
    ParamLgl$new('imput_mice.correlation',special_vals = list(T,F)))
  )



  glrn$encapsulate = c(train='evaluate',predict='evaluate')

  instance = TuningInstance$new(
    task = test_task,
    learner = glrn,
    resampling = rsmp('cv',folds=10L),
    measure = msr('classif.ce'),

    param_set = tune_ps,
    terminator = term("evals", n_evals = n)

  )
  tuner <- TunerRandomSearch$new()


  tuner$tune(instance)
  for (i in 1:n){
  cor <- instance$bmr$rr_data$tune_x[i][[1]][[1]]
  use_cor <-  instance$bmr$rr_data$tune_x[[i]][[2]]


  errors <-  instance$bmr$resample_result(i)
  cat(paste0('Corr :',cor, '   Use_CORR: ', use_cor),file = out_file,append = T)
  cat('     ERRORS : ',file = out_file,append = T)
  write(length(as.data.frame(errors$errors)[,1]),file = out_file,append = T)
  }
}
