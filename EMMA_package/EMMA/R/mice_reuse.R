#' @title Reuseble mice function
#' @description Reuse a previously fit multivariate imputation by chained equations to
#' impute values for previously unseen data without changing the imputation
#' fit (i.e. solely use the original training data to guide the imputation
#' models).
#'
#' Note: see https://github.com/stefvanbuuren/mice/issues/32 for discussion
#'
#'
#'
#' @param mids : mids object
#'    An object of class mids, typically produces by a previous call to mice() or mice.mids()
#'  newdata : data.frame
#'    Previously unseen data of the same structur as used to generate `mids`
#' @param maxit : integer scalar
#'    The number of additional Gibbs sampling iterations to refine the new imputations
#'  printFlag : logical scalar
#'    A Boolean flag. If TRUE, diagnostic information during the Gibbs sampling iterations
#'    will be written to the command window. The default is TRUE.
#'  seed : integer scalar
#'    An integer that is used as argument by the set.seed() for offsetting the random
#'    number generator. Default is to use the last seed value stored in `mids`
#'
#' @return
#'
#' data : list of data.frames
#'   the imputations of newdata
#'
#'
#'  lastSeedValue : integer vector
#'    the random seed at the end of the procedure
#' @export
#' @author Patrick Rockenschaub git https://github.com/prockenschaub
mice.reuse <- function(mids, newdata, maxit = 5, printFlag = TRUE, seed = NA) {

  newdata <- as.data.frame(lapply(newdata, function(x) {
    if (is(x, "numeric")) {
      return(as.numeric(x))
    }
    return(x)
  }))
  # Reuse a previously fit multivariate imputation by chained equations to
  # impute values for previously unseen data without changing the imputation
  # fit (i.e. solely use the original training data to guide the imputation
  # models).
  #
  # Note: see https://github.com/stefvanbuuren/mice/issues/32 for discussion
  #
  # Parameters
  # ----------
  #  mids : mids object
  #    An object of class mids, typically produces by a previous call to mice() or mice.mids()
  #  newdata : data.frame
  #    Previously unseen data of the same structur as used to generate `mids`
  #  maxit : integer scalar
  #    The number of additional Gibbs sampling iterations to refine the new imputations
  #  printFlag : logical scalar
  #    A Boolean flag. If TRUE, diagnostic information during the Gibbs sampling iterations
  #    will be written to the command window. The default is TRUE.
  #  seed : integer scalar
  #    An integer that is used as argument by the set.seed() for offsetting the random
  #    number generator. Default is to use the last seed value stored in `mids`
  #
  # Returns
  # -------
  #  data : list of data.frames
  #    the imputations of newdata
  #  lastSeedValue : integer vector
  #    the random seed at the end of the procedure#

  if (is.na(seed)) {
    assign(".Random.seed", mids$lastSeedValue, pos = 1)
  } else {
    set.seed(seed)
  }

  # Check that the newdata is the same as the old data
  rows <- nrow(newdata)
  cols <- ncol(newdata)
  testthat::expect_equal(cols, ncol(mids$data))

  nm <- names(newdata)
  testthat::expect_equal(nm, names(mids$data))

  # Set up a mids object for the newdata, but set all variables to missing
  all_miss <- matrix(TRUE, rows, cols, dimnames = list(seq_len(rows), nm))
  mids.new <- mice::mice(newdata, mids$m, where = all_miss, maxit = 0, predictorMatrix = mids$predictorMatrix)
  # Combine the old (trained) and the new mids objects
  mids.comb <- EMMA::mids.append(mids, mids.new)

  new_idx <- mids.comb$app_idx
  mids.comb <- mids.comb$mids

  mids.comb$lastSeedValue <- .Random.seed # set the seed to the current value

  # Set the newdata to missing (so it doesn't influence the imputation)
  # but remember the actual values
  actual_data <- mids.comb$data
  mids.comb$data[mids.comb$where] <- NA

  # Also make sure all observed variables in the newdata are set to their
  # true values in the imputations
  for (j in names(mids.comb$imp)) {
    for (i in seq_len(mids.comb$m)) {
      mids.comb$imp[[j]][, i] <-
        EMMA::replace_overimputes(actual_data, mids.comb$imp, j, i)
    }
  }

  # At each imputation of a variable, all new values are by design considered
  # to be missing (so they do not influence the imputation model). An imputation
  # of the variable is thus created for each single new observation. However,
  # we only want to impute the values in the new observations that were _actually_
  # missing. Use conditional imputation
  # (https://stefvanbuuren.name/fimd/sec-knowledge.html)
  # to replace those values that we did observe with their actual values that
  # we stored in the variable `actual_data` after each sampling.
  cond_imp <- "imp[[j]][, i] <- EMMA::replace_overimputes(EMMA::fetch_data(), imp, j, i)"
  mids.comb$post <- sapply(
    mids.comb$post,
    function(x) {
      if (x != "") {
        paste0(x, "; ", cond_imp)
      } else {
        cond_imp
      }
    })

  # Run the procedure for a few times

  mids.comb <- mice::mice.mids(mids.comb, maxit = maxit, printFlag = printFlag)

  # Return the imputed test dataset

  res <- lapply(mice::complete(mids.comb, "all"), function(x) {
    x[new_idx, ]
  }
  )
  class(res) <- c("mild", "list")
  res
}

#' @export
mids.append <- function(x, y) {
  # Append one mids object to another. Both objects are expected to have
  # the same variables.
  #
  # Note: Only the data specific aspects are copied (i.e. $data, $imp, $where,
  #       $nmis), all other information in `y` is discarded. Therefore, only
  #       the imputation model of `x` is kept and `y` must not contain missing
  #       data in variables that did not have missing data in `x` (but the
  #       reverse is allowed).
  #
  # Parameters
  # ----------
  #  x : mids object
  #    provides both data and specification of imputation procedure
  #  y : mids object
  #    only data information will be retained in the combined object
  #
  # Returns
  # -------
  #  mids object
  #    a new mids object that contains all of `x` and the additional data in `y`

  testthat::expect_equal(names(x$data), names(y$data))
  app <- x

  miss_xy <- intersect(names(x$nmis), names(y$nmis))
  testthat::expect_true(all(names(y$nmis) %in% miss_xy))

  # Append `data`
  app$data <- rbind(x$data, y$data)
  x_idx <- rownames(x$data)
  y_idx <- base::setdiff(rownames(app$data), x_idx)
  names(y_idx) <- rownames(y$data)

  # Append `imp` and `nmis`
  for (i in names(x$imp)) {
    if (i %in% miss_xy) {
      # Imputations
      app_imp <- y$imp[[i]]
      rownames(app_imp) <- y_idx[rownames(app_imp)]
      app$imp[[i]] <- rbind(x$imp[[i]], app_imp)

      # nmis
      app$nmis[[i]] <- x$nmis[[i]] + y$nmis[[i]]
    }
  }

  # Append `where`
  app$where <- rbind(x$where, y$where)
  rownames(app$where) <- rownames(app$data)

  res <- list(mids = app, app_idx = setNames(y_idx, NULL))
  res$method <- x$method
  res
}

#' @export
replace_overimputes <- function(data, imp, j, i) {
  # Replace all overimputed data points in the mice imputation
  # of one variable. Overimputed data points are those data
  # that were not missing in the original but were marked for
  # imputation manually and imputed by the imputation procedure.
  #
  # Parameters
  # ----------
  #  data : data.frame
  #    the original, non-imputed dataset (mids$data)
  #  imp : list of data.frames
  #    all imputations stored in the mids object
  #  j : character scalar
  #    the name of the variable whose imputations should be
  #    replaced
  #  i : character or integer scalar
  #    the number of the current imputation (can be 1:m)

  # Find those values that weren't missing but imputed

  overlap <- base::intersect(
    rownames(data[!is.na(data[, j]), j, drop = FALSE]),
    rownames(imp[[j]])
  )

  # Replace them with the true values and return
  imp[[j]][overlap, i] <- data[overlap, j]
  imp[[j]][, i]
}

#' @export
fetch_data <- function() {
  # Retrieve the main imputation object when within the
  # `mice:::sampler` post-imputation calling environment
  # and return the data object (including missingness)
  # stored within.
  #
  # Note: mainly to be used in `mids$post` calling strings
  #
  # Returns
  # -------
  #  data.frame
  #    the original, non-imputed dataset of the mids object

  get("actual_data", pos = parent.frame(5))
}


# set_test_A <- mice.reuse(model,set_test)$`1`
