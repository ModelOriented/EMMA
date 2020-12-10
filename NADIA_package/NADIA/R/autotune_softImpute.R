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
#' @import VIM
#'
#' @references Trevor Hastie and Rahul Mazumder (2015). softImpute: Matrix Completion via Iterative Soft-Thresholded SVD. R package version 1.4. https://CRAN.R-project.org/package=softImpute
#'
#' @author Trevor Hastie and Rahul Mazumder (2015).
#'
#' @examples
#' {
#'   raw_data <- data.frame(
#'     a = as.factor(sample(c("red", "yellow", "blue", NA), 1000, replace = TRUE)),
#'     b = as.integer(1:1000),
#'     c = as.factor(sample(c("YES", "NO", NA), 1000, replace = TRUE)),
#'     d = runif(1000, 1, 10),
#'     e = as.factor(sample(c("YES", "NO"), 1000, replace = TRUE)),
#'     f = as.factor(sample(c("male", "female", "trans", "other", NA), 1000, replace = TRUE)))
#'
#'   # Prepering col_type
#'   col_type <- c("factor", "integer", "factor", "numeric", "factor", "factor")
#'
#'   percent_of_missing <- 1:6
#'   for (i in percent_of_missing) {
#'     percent_of_missing[i] <- 100 * (sum(is.na(raw_data[, i])) / nrow(raw_data))
#'   }
#'
#'
#'   imp_data <- autotune_softImpute(raw_data, percent_of_missing, col_type)
#'
#'   # Check if all missing value was imputed
#'   sum(is.na(imp_data)) == 0
#'   # TRUE
#' }
#' @return Return one data.frame with imputed values.
#' @export



autotune_softImpute <- function(df, percent_of_missing=NULL, col_type=NULL, col_0_1 = FALSE, cat_Fun = VIM::maxCat, lambda = 0, rank.max = 2, type = "als", thresh = 1e-5, maxit = 100, out_file = NULL) {


  # Column informations
  if(is.null(col_type)){
    col_type <- 1:ncol(df)
    for ( i in col_type){
      col_type[i] <- class(df[,i])
    }
  }

  if(is.null(percent_of_missing)){
    percent_of_missing <- 1:ncol(df)
    for ( i in percent_of_missing){
      percent_of_missing[i] <- sum(is.na(df[,i]))/nrow(df)
    }
  }

  if (sum(col_type == "numeric" | col_type == "integer") < 2) {
    print("Not enought numeric for softimpute given function will be used")
  }


  column_order <- colnames(df)
  if (sum(is.na(df)) == 0) {
    return(df)
  }



  # Prepering matrix
  matrix <- as.matrix(df[, ifelse(col_type == "numeric" | col_type == "integer", TRUE, FALSE), drop = FALSE])
  if (!is.null(out_file)) {
    write("softImpute", file = out_file, append = TRUE)
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
        write("Not engouht numeric impute with function", file = out_file, append = TRUE)
      }
      if (sum(col_type == "numeric" | col_type == "integer") > 0) {
        j <- colnames(df)[col_type == "numeric" | col_type == "integer"]
        col_to_imp <- df[, j, drop = FALSE]
        col_to_imp[is.na(col_to_imp), ] <- cat_Fun(stats::na.omit(col_to_imp[[j]]))
        j <- col_to_imp
        final <- j
      }
    }


    # Categorical Imputation
    iter_vec <- colnames(df)[col_type == "factor" & percent_of_missing > 0]
    for (i in iter_vec) {
      col_to_imp <- df[, i]
      col_to_imp[is.na(col_to_imp)] <- cat_Fun(col_to_imp[!is.na(col_to_imp)])
      df[, i] <- col_to_imp
    }

    # conecting df back

    if (exists("final")) {
      final <- cbind(as.data.frame(final), df[, ifelse(col_type == "numeric" | col_type == "integer", FALSE, TRUE), drop = FALSE])
    }
    else {
      final <- df[, ifelse(col_type == "numeric" | col_type == "integer", FALSE, TRUE), drop = FALSE]
    }
    final <- final[, column_order]
    if (!is.null(out_file)) {
      write("OK", file = out_file, append = TRUE)
    }
  }, error = function(e) {
    if (is.null(out_file)) {
      write(as.character(e), file = out_file, append = TRUE)
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
