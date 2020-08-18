#' Hot-Deck imputation using VIM package.
#'
#'
#' @description Function perform hotdeck function from VIM package. Any tunable parameters aren't available in this algorithm.
#'
#'
#' @param df data.frame. Df to impute with column names and without  target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @import VIM
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @return Return data.frame with imputed values.

autotune_VIM_hotdeck <- function(df,percent_of_missing,col_0_1=FALSE,out_file=NULL){

  if(!is.null(out_file)){
    write('VIM_HD',file = out_file,append = T)
  }
  tryCatch({
  final <- VIM::hotdeck(df,imp_var = FALSE)
  if(!is.null(out_file)){
    write('  OK',file = out_file,append = T)
  }},error=function(e){
    if(!is.null(out_file)){
      write(as.character(e),file = out_file,append = T)
    }
    stop(e)
  }

  )
  if (col_0_1){
    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)
  }
  return(final)



}

