#' Perform imputation using missForest form missForest package.
#'
#' @description Function use missForest package for data imputation. OBBerror (more in  \code{\link{autotune_mice}}) is used to perform grid search.
#' @details Function try to use parallel backend if it's possible. Half of the available cores are used or number pass as cores param. (Number of used cores can't be higher then number of variables in df. If it happened a number of cores will be set at ncol(df)-2 unless this number is <= 0 then cores =1).  To perform parallel calculation function use  \code{\link[doParallel]{registerDoParallel}} to create parallel backend.
#' Creating backend can have significant time cost so for very small df cores=1 can speed up calculation. After calculation function turns off parallel backend. \cr \cr   Gride search is used to chose a sample for each tree and the number of trees can be turn off. Params in grid search have significant influence on imputation quality but function should work on any reasonable values of this parameter.
#'
#'
#' @param df data.frame. Df to impute with column names.
#' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
#' @param cores integer.  Number of threads used by parallel calculations. By default approximately half of available CPU cores.
#' @param ntree_set integer vector. Vector contains numbers of tree for grid search.
#' @param mtry_set integer vector. Vector contains numbers of variables randomly sampled at each split.
#' @param parallel logical. If TRUE parallel calculation is using.
#' @param optimize optimize inside function
#' @param ntree ntree from missForest function
#' @param mtry mtry form missforest function
#' @param verbose If FALSE funtion didn't print on console.
#' @param maxiter maxiter form missForest function.
#' @param maxnodes maxnodes from missForest function.
#' @param out_file  Output log file location if file already exists log message will be added. If NULL no log will be produced.
#' @import missForest
#' @import doParallel
#' @param col_0_1 decide if add bonus column informing where imputation been done. 0 - value was in dataset, 1 - value was imputed. Default False.
#'
#' @return Return data.frame with imputed values.
#' @export
autotune_missForest <- function(df, col_type, percent_of_missing, cores = NULL, ntree_set = c(100, 200, 500, 1000), mtry_set = NULL, parallel = TRUE, col_0_1 = FALSE,
  optimize = TRUE, ntree = 100, mtry = NULL, verbose = FALSE, maxiter = 20, maxnodes = NULL, out_file = NULL) {

  # Checking if parallel backed is runing and starting it if not
  do_things <- function(df, col_type, percent_of_missing, cores = NULL, ntree_set = c(100, 200, 500, 1000), mtry_set = NULL, parallel = TRUE, col_0_1 = FALSE,
    optimize = TRUE, ntree = 100, mtry = NULL, maxiter = 20, maxnodes = NULL, out_file = NULL) {

    if (is.null(cores)) {
      if (parallel) {
        veribles = ncol(df)
        if (ceiling(parallel::detectCores() / 2) >= veribles) {
          cores <- (veribles - 2)
        }
        doParallel::registerDoParallel(cores = cores)
      }
    }
    if (!is.null(cores)) {
      if (parallel) {
        veribles = ncol(df)
        if (cores >= veribles) {
          cores <- (veribles - 2)
        }
        doParallel::registerDoParallel(cores = cores)
      }
    }

    # Prepering mtry_set if not given
    if (is.null(mtry_set)) {
      mtry_set <- 1:4
      mtry_set[1] <- floor(sqrt(ncol(df)))
      if (mtry_set[1] > 1) {
        mtry_set[2] <- ceiling(mtry_set[1] / 2)
        vector <- (mtry_set[1]:ncol(df))
        mtry_set[3] <- floor(length(vector) / 3)
        mtry_set[4] <- floor(2 * length(vector) / 3)
      }
      else {
        vector <- (mtry_set[1]:ncol(df))
        mtry_set[2] <- floor(length(vector) / 4)
        mtry_set[3] <- floor(2 * length(vector) / 4)
        mtry_set[4] <- floor(3 * length(vector) / 4)
      }

    }
    # If parallel=TRUE
    parallelize <- "no"
    if (parallel) {
      parallelize <- "forest"
    }
    if (!is.null(cores)) {
      if (cores <= 1) {
        parallelize <- "no"
      }
    }


    if (!is.null(out_file)) {
      write("missForest", file = out_file, append = T)
    }
    if (sum(percent_of_missing == 100) > 0) {
      if (!is.null(out_file)) {
        write("column with only missing values error", file = out_file, append = T)
      }
      stop("column with only missing values error")
    }
    tryCatch({
      if (optimize) {
        # Grid search using mean OBBerror
        best_params <- c(-11, -11)
        best_OBB <- 10
        for (i in ntree_set)
        {
          for (j in mtry_set) {
            skip_to_next <- FALSE

            tryCatch({
              iteration <- mean(missForest::missForest(df, maxiter = maxiter, ntree = i, mtry = j, parallelize = parallelize, maxnodes = maxnodes, verbose = verbose)$OOBerror)
              if (iteration < best_OBB) {
                best_OBB <- iteration
                best_params[1] <- i
                best_params[2] <- j
              }
            }, error = function(e) {
              skip_to_next <<- TRUE
            })

            if (skip_to_next) {
              next
            }
          }
        }

        # fianl imputation

        final <- missForest::missForest(df, maxiter = maxiter, maxnodes = maxnodes, ntree = best_params[1], mtry = best_params[2], parallelize = parallelize, verbose = verbose)$ximp
      }
      if (!optimize) {
        if (is.null(mtry)) {
          final <- missForest::missForest(df, maxiter = maxiter, ntree = ntree, maxnodes = maxnodes, mtry = floor(sqrt(ncol(df))), parallelize = parallelize, verbose = verbose)$ximp
        }
        else {
          final <- missForest::missForest(df, maxiter = maxiter, ntree = ntree, maxnodes = maxnodes, mtry = mtry, parallelize = parallelize, verbose = verbose)$ximp
        }
      }
      if (!is.null(out_file)) {
        write(c(best_params[1], best_params[2]), file = out_file, append = T)
        write(" OK", file = out_file, append = T)
      }
    }, error = function(e) {
      if (!is.null(out_file)) {
        write(as.character(e), file = out_file, append = T)
      }
      stop(e)
    })
    # adding 0_1_cols
    if (col_0_1) {
      columns_with_missing <- (as.data.frame(is.na(df)) * 1)[, percent_of_missing > 0]
      colnames(columns_with_missing) <- paste(colnames(columns_with_missing), "where", sep = "_")
      final <- cbind(final, columns_with_missing)
    }

    # turn off paralllel
    if (parallel) {
      foreach::registerDoSEQ()
    }


    return(final)
  }

  if (verbose) {
    return(do_things(
      df = df, col_type = col_type, percent_of_missing = percent_of_missing, cores = cores, ntree_set = ntree_set, mtry_set = mtry_set, parallel = parallel, col_0_1 = col_0_1,
      optimize = optimize, ntree = ntree, mtry = mtry, maxiter = maxiter, maxnodes = maxnodes, out_file = out_file))
  }
  if (!verbose) {
    capture.output(final <- do_things(
      df = df, col_type = col_type, percent_of_missing = percent_of_missing, cores = cores, ntree_set = ntree_set, mtry_set = mtry_set, parallel = parallel, col_0_1 = col_0_1,
      optimize = optimize, ntree = ntree, mtry = mtry, maxiter = maxiter, maxnodes = maxnodes, out_file = out_file))
    for (i in colnames(final)[col_type == "integer"]) {
      final[, i] <- as.integer(final[, i])
    }
    return(final)
  }
}
