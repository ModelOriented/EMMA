#' Perform imputation using VIM package and regressionImp function.
#'
#' @description Function use Regression models to impute missing data.
#'
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param col_0_1 Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False. (Works only for returning one dataset).
#' @param col_type Character vector with types of columns.
#' @param robust TRUE/FALSE if robust regression should be used.
#' @param mod_cat TRUE/FALSE if TRUE for categorical variables the level with the highest prediction probability is selected, otherwise it is sampled according to the probabilities.
#' @param use_imputed TRUE/FALSe if TURE already imputed columns will be used to impute another.
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @import VIM
#'
#' @return Return one data.frame with imputed values.
autotune_VIM_regrImp <- function(df,col_type,percent_of_missing,col_0_1=F,robust=F,mod_cat=F,use_imputed=F,out_file=NULL){
  if (!is.null(out_file)){
    write('regrImp',file = out_file,append = T)
  }
  tryCatch({
  # Cheking if imputation can be perform
  if(sum(percent_of_missing==0)==0){
    stop('No values with no missing values')
  }

  if (sum(is.na(df))==0){return(df)}


  final <- df
  iter_columns <-  (1:ncol(df))[percent_of_missing>0]
  for (i in iter_columns ){
    DONT_WORK <- TRUE
    full_formula <- paste(colnames(df)[i],paste(colnames(df)[percent_of_missing==0],collapse = '+'),sep = '~')

    tryCatch({
      final <- VIM::regressionImp(as.formula(full_formula),final,robust = robust,mod_cat = mod_cat,imp_var = F)
      DONT_WORK <- FALSE

    },error=function(e){
      numeric_formula <- paste(colnames(df)[i],paste(colnames(df)[percent_of_missing==0 & col_type=='numeric'],collapse = '+'),sep = '~')
      final <- VIM::regressionImp(as.formula(numeric_formula),final,robust = robust,mod_cat = mod_cat,imp_var = F)
      DONT_WORK <- TRUE
    })
    if (DONT_WORK){

      final_formula <- paste(colnames(df)[i],paste(colnames(df)[percent_of_missing==0 & col_type=='numeric'][1],collapse = '+'),sep = '~')
      final <- VIM::regressionImp(as.formula(numeric_formula_formula),final,robust = robust,mod_cat = mod_cat,imp_var = F)
    }

    if(use_imputed){
      percent_of_missing[i] <- 0
    }


  }
  if(!is.null(out_file)){
    write('  OK',file = out_file,append = T)
  }
  },error=function(e){
    if(!is.null(out_file)){
      write(as.character(e),file = out_file,append = T)
    }
    stop(e)

  })



  if(col_0_1){

    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)

  }

  return(final)



}



# test <- df
#
# col_type <- 1:ncol(test)
#
# for (i in col_type){
#   col_type[i] <- class(test[,i])
#
# }
#
# percent_of_missing <- 1:ncol(test)
#
# for (i in percent_of_missing){
#   percent_of_missing[i] <- (sum(is.na(test[,i]))/length(test[,1]))*100
# }
#
# wynik_test <- autotune_VIM_regrImp(test,col_type = col_type,percent_of_missing)
# sum(is.na(wynik_test))
#




