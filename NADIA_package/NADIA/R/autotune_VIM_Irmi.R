
#' Perform imputation using VIM package and irmi function
#'
#' @description Function use IRMI (Iterative robust model-based imputation ) to impute missing data.
#' @details Function can work with various different times depending on data size and structure. In some cases when selected param wouldn't work function try to run on default.  Most important param for both quality and reliability  its eps.
#'
#' @param df data.frame. Df to impute with column names and without target column.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param col_0_1 Decaid if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False. (Works only for returning one dataset).
#' @param eps threshold for convergency
#' @param col_type character vector. Vector containing column type names.
#' @param maxit maximum number of iterations
#' @param step  stepwise model selection is applied when the parameter is set to TRUE
#' @param robust if TRUE, robust regression methods will be applied (it's impossible to set step=TRUE and robust=TRUE at the same time)
#' @param init.method Method for initialization of missing values (kNN or median)
#' @param force if TRUE, the algorithm tries to find a solution in any case, possible by using different robust methods automatically. (should be set FALSE for simulation)
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @import VIM
#'
#' @references{  Alexander Kowarik, Matthias Templ (2016). Imputation with the R Package VIM. Journal of Statistical Software, 74(7), 1-16. doi:10.18637/jss.v074.i07}
#'
#'
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
#'   imp_data <- autotune_VIM_Irmi(raw_data, col_type, percent_of_missing)
#'
#'   # Check if all missing value was imputed
#'   sum(is.na(imp_data)) == 0
#'   # TRUE
#' }
#'
#' @author{ Alexander Kowarik, Matthias Templ (2016) \doi{10.18637/jss.v074.i07}}
#'
#' @return Return one data.frame with imputed values.
#' @export
autotune_VIM_Irmi <- function(df, col_type, percent_of_missing, eps = 5, maxit = 100, step = FALSE, robust = FALSE, init.method = "kNN", force = FALSE, col_0_1 = FALSE, out_file = NULL) {

  if (!is.null(out_file)) {
    write("VIM_IRMI", file = out_file, append = TRUE)
  }

  tryCatch({
    final <- VIM::irmi(df, eps = eps, maxit = maxit, step = step, robust = robust, init.method = init.method, force = force, imp_var = FALSE)
    if (!is.null(out_file)) {
      write("  OK ", file = out_file, append = TRUE)
    }
  }, error = function(e) {
    if (!is.null((out_file))) {
      write(as.character(e), file = out_file, append = TRUE)
      write("IRMI dont work on selcted params runing on defoult", file = out_file, append = TRUE)
    }
    print("IRMI dont work on selcted params runing on defoult")

    tryCatch({
      final <- VIM::irmi(df, imp_var = FALSE)
    }, error = function(e) {
      if (!is.null((out_file))) {
        write(as.character(e), file = out_file, append = TRUE)

      }

      if(as.character(e)=="argument of length 0")
      {
        print("Problem with not working algorithm not an easy way to solve")
      }
      stop(e)
    })
  })



  if (col_0_1) {

    columns_with_missing <- (as.data.frame(is.na(df)) * 1)[, percent_of_missing > 0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing), "where", sep = "_")
    final <- cbind(final, columns_with_missing)

  }
  for (i in colnames(final)[col_type == "integer"]) {
    final[, i] <- as.integer(final[, i])
  }


  return(final)

}
