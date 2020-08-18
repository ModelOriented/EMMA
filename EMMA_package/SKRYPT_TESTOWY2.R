    library(dplyr)
    library(janitor)
    library(tidyselect)
    library(EMMA)
    library(mlr3learners)
    library(OpenML)

    ##### DO ZMIANY#####
    OutLogLocation <- getwd()
    outfilename <- '/out_log.txt'

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






    #######
    out_file <- paste0(OutLogLocation,outfilename)

    datasets_Ids <- c(13,15,24,927,1111,1053)

    write(paste0('LOG',Sys.Date()),file = out_file)

    check_type <- function(x){
      is.numeric(x) | is.factor(x)
    }

    data_types_troubles <- c()
    miss_in_target <- c()




    list_of_pipe <- c(PipeOpMice,PipeOpMissMDA_MFA,PipeOpMissMDA_PCA_MCA_FMAD,PipeOpmissForest,PipeOpVIM_HD,PipeOpVIM_IRMI,
                      PipeOpVIM_kNN,PipeOpVIM_regrImp,PipeOpmissRanger,PipeOpSoftImpute)


    for(id in datasets_Ids){

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
      # write('----------------------IMPUTACJE-----------------------',append = T,file=out_file)
      # single_set_Pipeline(df,id,col_type,percent_of_missing,out_file_location = out_file,single_set = FALSE)
      write('----------------------PIPLINE-----------------------',append = T,file=out_file)
      for (i in list_of_pipe){
        tryCatch({
        learner_po = po("learner", learner = lrn("classif.rpart"))
        test_imp <- i$new()
        test = test_imp %>>%  learner_po
        glrn =GraphLearner$new(test)
        test_task = TaskClassif$new('test',backend = df,target = df_oml$target.features)
        cat(test_imp$id,file = out_file,append = T)

        resample(test_task,glrn,rsmp('cv',folds=2L))
        write('ok',file=out_file,append = T)
        },error=function(e){
          write(as.character(e),file = out_file,append = T)
        })
      }



    }







