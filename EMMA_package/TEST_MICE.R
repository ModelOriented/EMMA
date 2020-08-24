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
datasets <-c(15,25,38,802,930,957,961,40954,41162) # datasets ID for test
error_csv_location <- 'errors.csv'

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





  write('----random search----',file = out_file,append = T)










  iteration <- 1
  fold <- 99999999999999
  iter <- 9999999999999
  error <- NA
  data_id <- 9999999999999999
  Errors_CSV <- data.frame(data_id,iter,fold,error)
  write_csv(Errors_CSV,path = error_csv_location)
  ##### RADNOM SEARCH WRITED BY ME #####
  for (i in 1:n){
  test_task = TaskClassif$new('test',backend = df,target = df_oml$target.features)
  test_imp <- PipeOpMice$new()
  learner_po = po("learner", learner = lrn("classif.rpart"))
  test = test_imp %>>%  learner_po
  glrn =GraphLearner$new(test)
  glrn$encapsulate <- c(train='evaluate',predict='evaluate')

  glrn$param_set$values$imput_mice.set_cor <- runif(1)
  glrn$param_set$values$imput_mice.correlation <- sample(c(T,F),1)

  d<- resample(test_task,glrn,rsmp('cv'))

  #NUMBER OF ERRORS
  n_error <- nrow(as.data.frame(d$errors))

  massure <- msr('classif.acc')
  score <- massure$aggregate(d)
  cor <- glrn$param_set$values$imput_mice.set_cor
  use_cor <-  glrn$param_set$values$imput_mice.correlation


  cat(paste0(i,'  :'),file = out_file,append = T)
  cat(paste0('Corr :',cor, '   Use_CORR: ', use_cor),file = out_file,append = T)
  cat(paste0('  SCORE:',score),file = out_file,append = T)
  cat('     ERRORS : ',file = out_file,append = T)
  write(n_error,file = out_file,append = T)
  for (j in 0:n_error){
    if(j==0){next}
    errors <- as.data.frame(d$errors)$msg[j]

    write_csv(data.frame(id,i,j,as.character(errors)),path = error_csv_location,append = T)

  }




  }




}

