#' K nearest neighbor imputation using VIM package.
#'
#'
#' @description Function perform kNN function from VIM packge. Only K is tuned by function.
#'
#'
#' @param df data.frame. Df to impute with column names and without  target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param k Value of k use if optimize=FALSE
#' @param optimize If True function try to optimize k. Its can be pointless in many casys becouse. Evalutaion funtion from VIM ist not vary sutible for this task.
#' If False chosen k is used.
#' @param number_of_try Number of k betwen 5 and one-thired of dataset rows teseted for best k.
#' @param
#'
#' @import VIM
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#'
#' @return Return data.frame with imputed values.



autotune_VIM_kNN <- function(df,percent_of_missing,k=5,numFun=median,catFun=maxCat,col_0_1=FALSE,optimize=FALSE,number_of_try=10){
 #UNFINISHED
  final <-  kNN(df,k=k,numFun = numFun,catFun = catFun)

  if (col_0_1){
    columns_with_missing <-  (as.data.frame(is.na(df))*1)[,percent_of_missing>0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing),'where',sep='_')
    final <- cbind(final,columns_with_missing)
  }
}



