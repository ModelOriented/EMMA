#' Hot-Deck imputation using VIM package.
#'
#'
#' @description Function perform hotdeck functio from VIM packge. Hotdeck dont have any parameters to tune so function is vary simple and fast
#'
#'
#' @param df data.frame. Df to impute with column names and without  target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @import VIM
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#'
#' @return Return data.frame with imputed values.

autotune_VIM_hotdeck <- function(df,percent_of_missing,col_0_1=FALSE){
  final <- hotdeck(df,imp_var = FALSE)

  if (col_0_1){
    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)
  }
  return(final)



}

