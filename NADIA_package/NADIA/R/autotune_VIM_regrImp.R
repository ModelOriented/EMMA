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
#'
#'
#' @references    Alexander Kowarik, Matthias Templ (2016). Imputation with the R Package VIM. Journal of Statistical Software, 74(7), 1-16. doi:10.18637/jss.v074.i07
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
#'   imp_data <- autotune_VIM_regrImp(raw_data, col_type, percent_of_missing)
#'
#'   # Check if all missing value was imputed
#'   sum(is.na(imp_data)) == 0
#'   # TRUE
#' }
#' @return Return one data.frame with imputed values.
#'
#' @author{ Alexander Kowarik, Matthias Templ (2016) \doi{10.18637/jss.v074.i07}}
#'
#' @export
autotune_VIM_regrImp <- function(df, col_type, percent_of_missing, col_0_1 = FALSE, robust = FALSE, mod_cat = FALSE, use_imputed = FALSE, out_file = NULL) {

  if (!is.null(out_file)) {
    write("regrImp", file = out_file, append = T)
  }
  tryCatch({
    # Cheking if imputation can be perform
    if (sum(percent_of_missing == 0) == 0) {
      stop("No values with no missing values")
    }

    if (sum(is.na(df)) == 0) {
      return(df)
    }
    int_col_names <- c()
    # Converting integer to numeric
    final <- lapply(df, function(x) {
      if (class(x) == "integer") {
        return(as.numeric(x))

      }
      else {
        return(x)
      }
    })


    final <- as.data.frame(final)
    iter_columns <- (1:ncol(df))[percent_of_missing > 0]

    for (i in iter_columns) {
      error <- NULL
      WORK <- FALSE
      full_formula <- paste(colnames(df)[i], paste(colnames(df)[percent_of_missing == 0], collapse = "+"), sep = "~")
      numeric_formula <- paste(colnames(df)[i], paste(colnames(df)[percent_of_missing == 0 & (col_type == "numeric" | col_type == "integer")], collapse = "+"), sep = "~")
      tryCatch({
        final <- VIM::regressionImp(stats::as.formula(full_formula), final, robust = robust, mod_cat = mod_cat, imp_var = FALSE)
        WORK <- TRUE
      }, error = function(e) {
        print(as.character(e))
      })


      if (!WORK & sum(percent_of_missing == 0 & (col_type == "numeric" | col_type == "integer")) > 0) {
        tryCatch({
          final <- VIM::regressionImp(stats::as.formula(numeric_formula), final, robust = robust, mod_cat = mod_cat, imp_var = FALSE)
          WORK <- TRUE
        }, error = function(e) {
          print(as.character(e))
        })
      }
      if (!WORK) {
        tryCatch({
          part_formula <- paste(colnames(df)[i], paste(colnames(df)[percent_of_missing == 0][1], collapse = "+"), sep = "~")
          final <- VIM::regressionImp(stats::as.formula(part_formula), final, robust = robust, mod_cat = mod_cat, imp_var = FALSE)
          WORK <- TRUE
        }, error = function(e) {
          print(as.character(e))
        })
      }
      if (!WORK & sum(percent_of_missing == 0 & (col_type == "numeric" | col_type == "integer")) > 0) {
        tryCatch({
          numeric_part_formula <- paste(colnames(df)[i], paste(colnames(df)[percent_of_missing == 0 & (col_type == "numeric" | col_type == "integer")][1], collapse = "+"), sep = "~")
          final <- VIM::regressionImp(stats::as.formula(numeric_part_formula), final, robust = robust, mod_cat = mod_cat, imp_var = FALSE)
          WORK <- TRUE
        }, error = function(e) {
          print(as.character(e))
        })

      }
      if (!WORK) {
        print(as.character(error))
        stop(error)
      }


      if (use_imputed) {
        percent_of_missing[i] <- 0
      }


    }
    if (!is.null(out_file)) {
      write("  OK", file = out_file, append = TRUE)
    }
  }, error = function(e) {
    if (!is.null(out_file)) {
      write(as.character(e), file = out_file, append = TRUE)
    }
    print(as.character(e))
    if(as.character(e)=="dim(X) must have a positive lengthdim(X) must have a positive length"){
      print("Problem caused by one problematic column removing it should solve the problem ")
    }

    if(as.character(e)=="argument of length 0"){

      print("No column without missing values")
    }

    stop(e)



  })



  if (col_0_1) {

    columns_with_missing <- (as.data.frame(is.na(df)) * 1)[, percent_of_missing > 0]
    colnames(columns_with_missing) <- paste(colnames(columns_with_missing), "where", sep = "_")
    final <- cbind(final, columns_with_missing)

  }
  # converting back to integer
  if (exists("final")) {
    for (i in colnames(final)[col_type == "integer"]) {
      final[, i] <- as.integer(final[, i])
    }
  }

  return(final)



}
