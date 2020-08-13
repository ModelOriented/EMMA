library(dplyr)
library(janitor)
library(tidyselect)

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
