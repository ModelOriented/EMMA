require(EMMA)
library(OpenML)
library(readr)
library(dplyr)
library(janitor)
library(tidyselect)

#### COPY FORM PREPROCESING ####

#Reading list of selected data sets
read
datasets <- read.csv("../../datasets_store/datasets_selection/selected_datasets.csv")

check_type <- function(x){
  is.numeric(x) | is.factor(x)
}

data_types_troubles <- c()
miss_in_target <- c()
### OUT FILE
out_file <- 'LOG_PIPELINIE'

write(paste0('LOG',Sys.Date()),file = out_file)
for(id in datasets$ID[1:20]){

  df_oml <- getOMLDataSet(id)
  df_desc <- df_oml$desc
  df <- df_oml$data

  ### Preprocessing step ###

  #Columns to ignore according to tags
  to_ignore <- df_desc$ignore.attribute
  if(!is.na(to_ignore)){
    df <- select(df, -all_of(to_ignore))
  }

  #Removing row ID column according to tags
  row_id <- df_desc$row.id.attribute
  if(!is.na(row_id)){
    df <- select(df, -all_of(row_id))
  }

  #Data types
  data_types_check <- sapply(df, check_type)

  if(any(!data_types_check)){
    wrong_columns <- colnames(df)[!data_types_check]

    #Trying possible conversions
    for(col in wrong_columns){

      if(is.logical(df[,col]) | is.character(df[,col])){
        df[,col] <- as.factor(df[,col])
      }else{
        warning(paste("Data type trouble ID: ", as.character(id)))
        data_types_troubles <- c(data_types_troubles, id)
      }
    }
  }

  #Removing possible duplicate rows
  df <- df[!duplicated(df), ]

  #Removing constant columns
  df <- remove_constant(df)

  #Removing empty columns
  df <- remove_empty(df, "cols")

  #All to lowercase
  df <- mutate_if(df, is.factor, function(x) factor(tolower(x)))

  #Missing values in target variable
  target <- df_oml$target.features
  if(any(is.na(df[, target]))){
    miss_in_target <- c(miss_in_target, id)
    warning(paste("Missing values in target variable ID: ", as.character(id)))
  }

 ##
 ### OUTPUT FILE LOCATION
 ##


 col_type <- 1:ncol(df)
 for ( i in col_type){
  col_type[i] <- class(df[,i])
 }

 percent_of_missing  <- 1:ncol(df)
 for ( i in percent_of_missing){
   percent_of_missing[i] <- (sum(is.na(df[,i]))/length(df[,1]))*100
 }
 single_set_Pipeline(df,id,col_type,percent_of_missing,out_file_location = out_file,single_set = FALSE)


}
