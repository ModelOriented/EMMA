#' @title missMDA.reuse
#'
#' @name  missMDA.reuse
#'
#' @description
#' The function allows the user access to missMDA imputation in the A approach.
#'
#'
#' @details Function use the same trick as in mice.reuse (new data are changed in NA in imputation stage and added back after it ). Because in missMDA is impossible to completely ignore new rows. We set their weights on 1e-300 when weights in the training set equal 1.
#'
#' @param new_data data.frame. Df to impute with column names and without target column.
#' @param col_type character vector. Vector containing column type names.
#' @param train_data data.frame used for treining.
#' @param random.seed Integer, by default random.seed = NULL implies that missing values are initially imputed by the mean of each variable. Other values leads to a random initialization
#' @param coeff.ridge Value use in Regularized method.
#' @param maxiter maximal number of iteration in algortihm.
#' @param method method used in imputation algoritm.
#' @param threshold threshold for convergence.
#' @param ncp return when the training data set was imputed.
#' @param MFA If TRUE MFA is used if not MCA, PCA, or FMAD algorithm.
#' @param MFA_Object Object produce by missMDA_MFA required to perform MFA imputation.
#'
#' @export
missMDA.reuse <-
  function(train_data,
           new_data,
           col_type = NULL,
           ncp,
           random.seed = NULL,
           maxiter = 998,
           coeff.ridge = 1,
           threshold = 1e-6,
           method = "Regularized",
           MFA =FALSE,
           MFA_Object = NULL) {




    if (is.null(col_type)) {
      col_type <- 1:ncol(train_data)
      for (i in col_type) {
        col_type[i] <- class(train_data[, i])

      }
    }
    if (MFA){
      if(is.null(MFA_Object)){stop("MFA imputation required MFA_object")}

      type <- MFA_Object$type
      groups <- MFA_Object$groups
      ncp <- MFA_Object$ncp

      new_row_names <- paste0(1:nrow(new_data), '_new')

      full_data <- train_data
      full_data[new_row_names, ] <- new_data

      imputed_full_data <- missMDA::imputeMFA(
        full_data,
        group = groups,
        ncp,
        type=type,
        seed = random.seed,
        maxiter = maxiter,
        coeff.ridge = coeff.ridge,
        threshold = threshold,
        method = method,
        row.w = c(rep(1, nrow(train_data)), rep(1e-300, nrow(new_data)))
      )$completeObs

      final <- imputed_full_data[new_row_names, ]


      row.names(final) <- row.names(new_data)
      return(final)

    }

    # Flags informing about data type
    FMAD <- FALSE # mix
    MCA <- FALSE # categorical
    PCA <- FALSE # numeric

    if ("factor" %in% col_type &
        ("numeric" %in% col_type | "integer" %in% col_type)) {
      FMAD <- TRUE
    }
    if ("factor" %in% col_type &
        !("numeric" %in% col_type | "integer" %in% col_type)) {
      MCA <- TRUE
    }
    if (!("factor" %in% col_type) &
        ("numeric" %in% col_type | "integer" %in% col_type)) {
      PCA <- TRUE
    }


    imp_function <- function(FUN, new_data, train_data, ncp) {
      #browser()
      new_row_names <- paste0(1:nrow(new_data), '_new')

      full_data <- train_data
      full_data[new_row_names, ] <- new_data

      imputed_full_data <- FUN(
        full_data,
        ncp,
        seed = 123,
        maxiter = 998,
        coeff.ridge = 1,
        threshold = 1e-6,
        method = "Regularized",
        row.w = c(rep(1, nrow(train_data)), rep(1e-300, nrow(new_data)))
      )$completeObs

      new_imputed_data <- imputed_full_data[new_row_names, ]


      return(new_imputed_data)

    }


    tryCatch({
      if (FMAD) {
        final <- imp_function(missMDA::imputeFAMD, new_data, train_data, ncp)
        row.names(final) <- row.names(new_data)
        return(final)
      }
      if (MCA) {
        final <- imp_function(missMDA::imputeMCA, new_data, train_data, ncp)
        row.names(final) <- row.names(new_data)
        return(final)
      }
      if (PCA) {
        final <- imp_function(missMDA::imputePCA, new_data, train_data, ncp)
        row.names(final) <- row.names(new_data)
        return(final)
      }
    }, error = function(e) {
      if (FMAD) {
        final <- imp_function(missMDA::imputeFAMD, new_data, train_data, 1)
        row.names(final) <- row.names(new_data)
        return(final)
      }
      if (MCA) {
        final <- imp_function(missMDA::imputeMCA, new_data, train_data, 1)
        row.names(final) <- row.names(new_data)
        return(final)
      }
      if (PCA) {
        final <- imp_function(missMDA::imputePCA, new_data, train_data, 1)
        row.names(final) <- row.names(new_data)
        return(final)
      }
    })



  }
