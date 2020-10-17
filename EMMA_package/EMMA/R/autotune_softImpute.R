#' Perform imputation using softImpute package
#'
#' @description Function use softImpute to impute missing data it works only with numeric data. Columns with categorical values are imputed by a selected function.
#' @details Function use algorithm base on matrix whats meaning if only one numeric column exists in dataset imputation algorithm don't work. In that case, this column will be imputed using a function for categorical columns. Because of this algorithm is working properly only with at least two numeric features in the dataset. To specify column type argument col_type is used so it's possible to forcefully use for example numeric factors in imputation. Action like this can led to errors and its not.
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param col_0_1 Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False. (Works only for returning one dataset).
#' @param col_type Character vector with types of columns.
#' @param cat_Fun Function to impute categorical features. Default maxCat (mode). Can be every function with input one character vector and return atomic object.
#' @param lambda nuclear-norm regularization parameter. If lambda=0, the algorithm reverts to "hardImpute", for which convergence is typically slower. If null lambda is set automatically at the highest possible values.
#' @param rank.max This restricts the rank of the solution. Defoult 2 if set as NULL rank.max=min(dim(X))-1.
#' @param type Chose of algoritm 'als' or 'svd . Defoult 'als'.
#' @param thresh Threshold for convergence.
#' @param maxit Maximum number of iterations.
#' @param out_file Output log file location if file already exists log message will be added. If NULL no log will be produced.
#'
#' @import softImpute
#' @importFrom softImpute complete
#' @imports VIM
#'
#' @return Return one data.frame with imputed values.
#' @export



autotune_softImpute <- function(df, percent_of_missing, col_type, col_0_1 = F, cat_Fun = VIM::maxCat, lambda = 0, rank.max = 2, type = "als", thresh = 1e-5, maxit = 100, out_file = NULL) {

  column_order <- colnames(df)
  if (sum(is.na(df)) == 0) {
    return(df)
  }



  # Prepering matrix
  matrix <- as.matrix(df[, ifelse(col_type == "numeric" | col_type == "integer", TRUE, FALSE), drop = F])
  if (!is.null(out_file)) {
    write("softImpute", file = out_file, append = T)
  }
  tryCatch({
    if (is.null(lambda)) {
      lambda <- floor(softImpute::lambda0(matrix, thresh = thresh, maxit = maxit))
    }
    if (is.null(rank.max)) {
      rank.max <- min(dim(matrix)) - 1
      if (rank.max <= 0) {
        rank.max <- 1
      }
    }
    # Numeric Imputation
    if (sum(col_type == "numeric" | col_type == "integer") > 1) {
      result <- softImpute::softImpute(matrix, lambda = lambda, rank.max = 2, thresh = thresh, maxit = maxit)
      final <- softImpute::complete(matrix, result)

    }
    else {
      print("Not enought numeric")
      if (!is.null(out_file)) {
        write("Not engouht numeric impute with function", file = out_file, append = T)
      }
      j <- colnames(df)[col_type == "numeric" | col_type == "integer"]
      col_to_imp <- df[, j, drop = F]
      col_to_imp[is.na(col_to_imp), ] <- cat_Fun(na.omit(col_to_imp[[j]]))
      j <- col_to_imp
      final <- j
    }


    # Categorical Imputation
    iter_vec <- colnames(df)[col_type == "factor" & percent_of_missing > 0]
    for (i in iter_vec) {
      col_to_imp <- df[, i]
      col_to_imp[is.na(col_to_imp)] <- cat_Fun(col_to_imp[!is.na(col_to_imp)])
      df[, i] <- col_to_imp
    }

    # conecting df back


    final <- cbind(as.data.frame(final), df[, ifelse(col_type == "numeric" | col_type == "integer", F, T), drop = F])

    final <- final[, column_order]
    if (!is.null(out_file)) {
      write("OK", file = out_file, append = T)
    }
  }, error = function(e) {
    if (is.null(out_file)) {
      write(as.character(e), file = out_file, append = T)
    }
    stop(e)
  })

  if (col_0_1) {

    columns_with_missing <- (as.data.frame(is.na(df)) * 1)[, percent_of_missing > 0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing), "where", sep = "_")
    final <- cbind(final, columns_with_missing)

  }
  # converting back to integer
  for (i in colnames(final)[col_type == "integer"]) {
    final[, i] <- as.integer(final[, i])
  }
  return(final)



}
