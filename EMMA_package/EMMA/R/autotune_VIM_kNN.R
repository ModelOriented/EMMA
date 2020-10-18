#' K nearest neighbor imputation using VIM package.
#'
#'
#' @description Function perform kNN function from VIM packge.
#'
#'  @details  Function don't perform any inside param tuning. Users can change important param for kNN like number or nearest or aggregation functions.
#' @param df data.frame. Df to impute with column names and without  target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param k Value of k use if optimize=FALSE
#' @param numFUN function for aggregating the k Nearest Neighbours in the case of a numerical variable. Default median.
#' @param catFUN function for aggregating the k Nearest Neighbours in the case of a categorical variable. Default mode.
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#'
#' @import VIM
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#'
#' @export



autotune_VIM_kNN <- function(df, percent_of_missing, k = 5, numFun = median, catFun = VIM::maxCat, col_0_1 = FALSE, out_file = NULL) {
  if (!is.null(out_file)) {
    write("VIM_kNN", file = out_file, append = T)
  }
  if (sum(percent_of_missing == 100) > 0) {
    if (!is.null(out_file)) {
      write("Feature contains only NA", file = out_file, append = T)
    }
    stop("Feature contains only NA")
  }

  tryCatch({
    final <- VIM::kNN(df, k = k, numFun = numFun, catFun = catFun, imp_var = F)
    if (!is.null(out_file)) {
      write("  OK", file = out_file, append = T)
    }
  }, error = function(e) {
    if (!is.null(out_file)) {
      write(as.character(e), file = out_file, append = T)
    }
    stop(e)
  })
  if (col_0_1) {
    columns_with_missing <- (as.data.frame(is.na(df)) * 1)[, percent_of_missing > 0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing), "where", sep = "_")
    final <- cbind(final, columns_with_missing)
  }
  return(final)
}
