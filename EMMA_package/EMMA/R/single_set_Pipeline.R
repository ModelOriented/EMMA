#' Pipline for one data set. FUNCTION FOR TESTING
#'
#' @description Function is Work in progress.
#'
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param in openml dataset id.
#' @param col_type character vector. Vector containing column type names.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param out_file_location Location of output file.
#' @param single_set Set as False if Pipeline is used in a bigger pipeline for multiple datasets.
#' @param cores Number of threads used by missForest.
#' @param OTHERS Function will be containing much more params responsible for settings for now all no essential parameters are set on default.
#' @details ---
#' @return Return list of data.frame or NULLs if the imputation method didn't work.

single_set_Pipeline <- function(df, id, col_type, percent_of_missing, out_file_location, single_set = TRUE, cores = NULL) {

  col_miss <- colnames(df)[percent_of_missing > 0]
  col_no_miss <- colnames(df)[percent_of_missing == 0]
  # creating log_file
  if (single_set) {
    out_file_location <- paste0(out_file_location, paste0("log_", id))

    write(paste0("LOG_FROM", Sys.Date()), file = out_file_location)
  }

  write(paste0("Dataset id :", id), file = out_file_location, append = T)
  # if some imputation faile
  mice_result <- NULL
  missMDA_result_1 <- NULL
  missMDA_result_2 <- NULL
  missForest_result <- NULL
  VIM_HD <- NULL
  VIM_IRMI <- NULL
  VIM_KNN <- NULL
  VIM_REGRIMP <- NULL
  softImpute_res <- NULL
  missRanger_res <- NULL
  cat("Mice :", file = out_file_location, append = T)
  tryCatch({
    mice_result <- autotune_mice(df, col_miss = col_miss, col_no_miss = col_no_miss, percent_of_missing = percent_of_missing, col_type = col_type, iter = 5, verbose = F)
    if (sum(is.na(mice_result)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })


  cat("missMDA_FMAD_MCA_PCA :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    missMDA_result_1 <- missMDA_FMAD_MCA_PCA(df, col_type, percent_of_missing)
    if (sum(is.na(missMDA_result_1)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })


  cat("missMDA_MFA :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    missMDA_result_2 <- missMDA_MFA(df, col_type, percent_of_missing)
    if (sum(is.na(missMDA_result_2)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  cat("missForest :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    missForest_result <- autotune_missForest(df, percent_of_missing, cores = cores, verbose = F)
    if (sum(is.na(missForest_result)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  cat("VIM_HD :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    VIM_HD <- autotune_VIM_hotdeck(df, percent_of_missing)
    if (sum(is.na(VIM_HD)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  cat("VIM_KNN :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    VIM_KNN <- autotune_VIM_kNN(df, percent_of_missing)
    if (sum(is.na(VIM_KNN)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  cat("VIM_Irmi :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    VIM_IRMI <- autotune_VIM_Irmi(df, percent_of_missing)
    if (sum(is.na(VIM_IRMI)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  cat("VIM_regrImp :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    VIM_REGRIMP <- autotune_VIM_regrImp(df, percent_of_missing = percent_of_missing, col_type = col_type)
    if (sum(is.na(VIM_REGRIMP)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  cat("softImpute :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    softImpute_res <- autotune_softImpute(df, percent_of_missing, col_type)
    if (sum(is.na(softImpute_res)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  cat("missRanger :", file = out_file_location, append = TRUE, sep = "")
  tryCatch({
    missRanger_res <- autotune_missRanger(df, percent_of_missing)
    if (sum(is.na(missRanger_res)) == 0) {
      write("OK", file = out_file_location, append = TRUE)
    }
    else {
      write("NO Errors and no all data imputed", file = out_file_location, append = TRUE)
    }
  }, error = function(e) {
    write(as.character(e), file = out_file_location, append = TRUE)
  })

  write("----------------------", file = out_file_location, append = TRUE)
  return(list(mice_result, missMDA_result_1, missMDA_result_2, missForest_result))

}





#### TESTING ####
# df <- df
# id <- id
# df_info<- read_json(paste("./datasets_store/information_base/dataset_", id, ".json", sep = "")) # not realy usfull
#
# col_type <- 1:ncol(df)
# for ( i in col_type){
#  col_type[i] <- class(df[,i])
# }
#
# percent_of_missing  <- 1:ncol(df)
# for ( i in percent_of_missing){
#   percent_of_missing[i] <- (sum(is.na(df[,i]))/length(df[,1]))*100
# }
#
# col_miss <- colnames(df)[percent_of_missing>0]
# col_no_miss <- colnames(df)[percent_of_missing==0]
#
#
# test <- Pipeline(df,id,col_type,percent_of_missing,out_file_location = '/home/jan/Pulpit/')
# library(missMDA)
