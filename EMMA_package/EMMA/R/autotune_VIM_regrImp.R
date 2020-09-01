#' Perform imputation using VIM package and regressionImp function.
#'
#' @description Function use Regression models to impute missing data.
#' @details Function impute one column per iteration to allow more control of imputation. All columns with missing values can be imputed with different formulas. For every new column to imputation one of four formula is used \cr
#' 1. col to impute ~ all columns without missing  \cr
#' 2. col to impute ~ all numeric columns without missing \cr
#' 3. col to impute ~ first of columns without missing \cr
#' 4. col to impute ~ first of numeric columns without missing \cr
#' For example, if formula 1 and 2 can't be used algorithm will try with formula 3. If all formula can't be used function will be stoped and error form tries with formula 4 or 3 presented. In some case, setting use_imputed on TRUE can solve this problem but in general its lower quality of imputation.
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
  int_col_names <- c()
  # Converting integer to numeric
  final <- lapply(df, function(x){

    if(class(x)=='integer'){
      return(as.numeric(x))

    }
    else{return(x)}
  })


  final <- as.data.frame(final)
  iter_columns <-  (1:ncol(df))[percent_of_missing>0]
  for (i in iter_columns ){
    error <- NULL
    WORK <- FALSE
    full_formula <- paste(colnames(df)[i],paste(colnames(df)[percent_of_missing==0],collapse = '+'),sep = '~')
    numeric_formula <- paste(colnames(df)[i],paste(colnames(df)[percent_of_missing==0 & (col_type=='numeric' | col_type =='integer')],collapse = '+'),sep = '~')
    tryCatch({
      final <- VIM::regressionImp(as.formula(full_formula),final,robust = robust,mod_cat = mod_cat,imp_var = F)
      WORK <- TRUE

    },error=function(e){error <<- e})

    if(!WORK & sum(percent_of_missing==0 & (col_type=='numeric' | col_type =='integer'))>0){
      tryCatch({
      final <-  VIM::regressionImp(as.formula(numeric_formula),final,robust = robust,mod_cat = mod_cat,imp_var = F)
      WORK <- TRUE},error=function(e){error <<- e})
    }
    if (!WORK){
      tryCatch({
      part_formula <- paste(colnames(df)[i],paste(colnames(df)[percent_of_missing==0][1],collapse = '+'),sep = '~')
      final <-  VIM::regressionImp(as.formula(part_formula),final,robust = robust,mod_cat = mod_cat,imp_var = F)
      WORK <- TRUE},error=function(e){error <<- e})
    }
    if (!WORK & sum(percent_of_missing==0 & (col_type=='numeric' | col_type =='integer'))>0){
      tryCatch({
      numeric_part_formula <- paste(colnames(df)[i],paste(colnames(df)[percent_of_missing==0 & (col_type=='numeric' | col_type =='integer')][1],collapse = '+'),sep = '~')
      final <-  VIM::regressionImp(as.formula(part_formula),final,robust = robust,mod_cat = mod_cat,imp_var = F)
      WORK <- TRUE},error=function(e){error <<- e})

    }
    if (!WORK){
      print(as.character(error))
      stop(error)}


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
    print(as.character(e))
    stop(e)

  })



  if(col_0_1){

    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)

  }
  # converting back to integer
  if(exists('final')){
  for (i in colnames(final)[col_type=='integer']){
    final[,i] <- as.integer(final[,i])
  }
  }

  return(final)



}





