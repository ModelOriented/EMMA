#' K nearest neighbor imputation using VIM package.
#'
#'
#' @description Function perform kNN function from VIM packge. Only K is tuned by function.
#'
#'
#' @param df data.frame. Df to impute with column names and without  target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param k Value of k use if optimize=FALSE
#' @param numFUN function for aggregating the k Nearest Neighbours in the case of a numerical variable. Defoult median.
#' @param catFUN function for aggregating the k Nearest Neighbours in the case of a categorical variable. Defoult mode.
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#'
#' @import VIM
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#'
#' @return Return data.frame with imputed values.



autotune_VIM_kNN <- function(df,percent_of_missing,k=5,numFun=median,catFun=VIM::maxCat,col_0_1=FALSE,out_file=NULL){
  if(!is.null(out_file)){
    write('VIM_kNN',file = out_file,append = T)
  }
  tryCatch({
  final <-  VIM::kNN(df,k=k,numFun = numFun,catFun = catFun,imp_var = F)
  if(!is.null(out_file)){
    write('  OK',file = out_file,append = T)
  }


  },error=function(e){
    if (!is.null(out_file)){
      write(as.character(e),file = out_file,append = T)
    }
    stop(e)
  })
  if (col_0_1){
    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)
  }
  return(final)
}



