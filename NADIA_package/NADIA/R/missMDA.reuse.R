#' @title missMDA.reuse
#'
#' @name  missMDA.reuse
#'
#' @description
#' The function allows the user access to missMDA imputation in the A approach.
#'
#'
#' @details Function use the same trick as in mice.reuse (new data are changed in NA in imputation stage and added back after it ). Because missMDA use averages to initiate
#' interactional algorithm. If we decided to use this approach all imputed data in one column will have the same values to avoid this problem. The function allows user to first
#' randomly resample existing observation in new data from the training set. This is not entirely in the missMDA algorithm but it is still A approach. Becouse missMDA dont save
#' crated model training has to be performed in this function.
#'
#' @param new_data data.frame. Df to impute with column names and without target column.
#' @param col_type character vector. Vector containing column type names.
#' @param train_data data.frame used for treining.
#' @param random.seed random seed.
#' @param coeff.ridge Value use in Regularized method.
#' @param maxiter maximal number of iteration in algortihm.
#' @param method method used in imputation algoritm.
#' @param threshold threshold for convergence.
#' @param mean_resample If TRUE average is used to initiate algorithm if FALSE random resampling is used. More about this can be found in detalist section.
#' @param ncp return when the training data set was imputed.
#'
#'
#'
#' @export
missMDA.reuse <- function(train_data,new_data,col_type,ncp,mean_resample=FALSE, random.seed = 123, maxiter = 998,
                          coeff.ridge = 1, threshold = 1e-6, method = "Regularized"){



  # Flags informing about data type
  FMAD <- FALSE # mix
  MCA <- FALSE # categorical
  PCA <- FALSE # numeric

  if ("factor" %in% col_type & ("numeric" %in% col_type | "integer" %in% col_type)) {
    FMAD <- TRUE
  }
  if ("factor" %in% col_type & !("numeric" %in% col_type | "integer" %in% col_type)) {
    MCA <- TRUE
  }
  if (!("factor" %in% col_type) & ("numeric" %in% col_type | "integer" %in% col_type)) {
    PCA <- TRUE
  }


imp_function <- function(FUN,new_data,train_data,ncp){

  # creating MAP of missing values
  map_missing <- is.na(new_data)

  # adding empty rows to traing data

  new_row_names <- paste("new_row_",1:nrow(new_data))
  train_data[new_row_names,] <- NA


  if(mean_resample){

    set.seed(random.seed)
    all_data_imp <- FUN(train_data,ncp, method = method, threshold = threshold, maxiter = maxiter, coeff.ridge = coeff.ridge, seed = random.seed)$completeObs


    new_data_clear <- all_data_imp[new_row_names,]

    iterator <- 1
    iterator <- 1
    for (i in colnames(new_data)){

      x <- new_data_clear[,i]
      x[!map_missing[,iterator]] <- new_data[!map_missing[,iterator],i]
      iterator <- iterator+1
      new_data_clear[,i] <- x

    }
    new_imputed_data <- new_data_clear
  }
    if(!mean_resample){


      for ( i in 1:ncol(train_data)){


        x <- train_data[new_row_names,i]

        x[!map_missing[,i]] <- sample(train_data[ifelse(row.names(train_data)%in% new_row_names,FALSE,TRUE),i],sum(!map_missing[,i]),replace = TRUE)

        train_data[new_row_names,i] <- x

      }



      all_data_imp <- FUN(train_data,ncp, method = method, threshold = threshold, maxiter = maxiter, coeff.ridge = coeff.ridge, seed = random.seed)$completeObs


      new_data_clear <- all_data_imp[new_row_names,]







      iterator <- 1
      for (i in colnames(new_data)){

        x <- new_data_clear[,i]
        x[!map_missing[,iterator]] <- new_data[!map_missing[,iterator],i]
        iterator <- iterator+1
        new_data_clear[,i] <- x

      }
      new_imputed_data <- new_data_clear


  }






  return(new_imputed_data)

}


tryCatch({
  if (FMAD) {
    final <- imp_function(missMDA::imputeFAMD,new_data,train_data,ncp)
  }
  if (MCA) {
    final <- imp_function(missMDA::imputeMCA,new_data,train_data,ncp)

  }
  if (PCA) {
    final <- imp_function(missMDA::imputePCA,new_data,train_data,ncp)
  }
}, error = function(e) {
  if (FMAD) {
    final <- imp_function(missMDA::imputeFAMD,new_data,train_data,1)
  }
  if (MCA) {
    final <- imp_function(missMDA::imputeMCA,new_data,train_data,1)
  }
  if (PCA) {
    final <- imp_function(missMDA::imputePCA,new_data,train_data,1)
  }
})


  row.names(final) <- row.names(new_data)
  return(final)
}
