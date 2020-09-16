  #' @title PipeOpMice_A
  #'
  #' @name PipeOpMice_A
  #'
  #' @description
  #' Implements mice methods as mlr3 in A approach (training imputation model on training data and used a trained model on test data).
  #'
  #' @details
  #' Code of used function was writen by \url{https://github.com/prockenschaub} more information aboute this aproche can be found here \url{https://github.com/amices/mice/issues/32}
  #'
  #' @section Input and Output Channels:
  #' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
  #'
  #'
  #' @section Parameters:
  #' The parameters include inherited from [`PipeOpImpute`], as well as: \cr
  #' \itemize{
  #' \item \code{id} :: \code{character(1)}\cr
  #' Identifier of resulting object, default \code{"imput_mice_A"}.
  #' \item \code{m} :: \code{integer(1)}\cr
  #' Number of datasets produced by mice, default \code{5}.
  #' \item \code{maxit} :: \code{integer(1)}\cr
  #' Maximum number of iterations for mice, default \code{5}.
  #' \item \code{set_corr} :: \code{double(1)}\cr
  #' Correlation or fraction of features used when optimize=FALSE. When correlation=FALSE, it represents a fraction of case to use in imputation for each variable, default \code{0.5}.
  #' \item \code{random.seed} :: \code{integer(1)}\cr
  #' Random seed, default \code{123}.
  #' \item \code{correlation} :: \code{logical(1)}\cr
  #' If set TRUE correlation is used, if set FALSE then fraction of case, default \code{TRUE}.
  #'}
  #'
  #' @export
  PipeOpMice_A <- R6::R6Class("mice_A_imputation",lock_objects=FALSE,
                                    inherit = PipeOpImpute,  # inherit from PipeOp
                                    public = list(
                                      initialize = function(id = "imput_mice_A",set_cor=0.5,m=5,maxit=5,random.seed=123,correlation=F
                                      ) {
                                        super$initialize(id,whole_task_dependent=TRUE, param_vals = list(set_cor=set_cor,m=m,maxit=maxit,random.seed=random.seed,correlation=correlation),
                                                         param_set= ParamSet$new(list(


                                                           'set_cor'=ParamDbl$new('set_cor', lower = 0, upper = 1, special_vals = list(), default = 0.5, tags = 'mice'),

                                                           'm'=ParamInt$new('m',lower = 1,upper = Inf,default = 2,tags='mice'),
                                                           'maxit'=ParamInt$new('maxit',lower = 5,upper = 100,default = 5,tags='mice'),



                                                           'random.seed'=ParamInt$new('random.seed',-Inf,Inf,default = 123,tags='mice'),

                                                           'correlation'=ParamLgl$new('correlation',default = F,tags='mice')




                                                         )),

                                        )


                                        self$imputed <- FALSE
                                        self$column_counter <- NULL
                                        self$data_imputed <- NULL

                                      }),private=list(

                                        .train_imputer=function(feature, type, context){
                                          imp_function <- function(data_to_impute){




                                            data_to_impute <- as.data.frame(data_to_impute)
                                            # prepering arguments for function
                                            col_type <- 1:ncol(data_to_impute)
                                            for (i in col_type){
                                              col_type[i] <- class(data_to_impute[,i])
                                            }
                                            percent_of_missing <- 1:ncol(data_to_impute)
                                            for (i in percent_of_missing){
                                              percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                            }
                                            col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                            col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                            if(self$param_set$values$correlation){
                                            model <- mice::mice(data_to_impute,m = self$param_set$values$m,maxit = self$param_set$values$maxit,
                                                                printFlag = T,seed = self$param_set$values$random.seed,predictorMatrix =mice::quickpred(data_to_impute, mincor=self$param_set$values$set_cor,method = 'spearman'))}
                                            else{ model <- mice::mice(data_to_impute,m = self$param_set$values$m,maxit = self$param_set$values$maxit,
                                                                      printFlag = T,seed = self$param_set$values$random.seed,predictorMatrix =mice::quickpred(data_to_impute, minpuc=self$param_set$values$set_cor,method = 'spearman'))}
                                            data_imputed <- mice::complete(model)





                                            return(list('data'=data_imputed
                                                   , 'model'=model))
                                          }

                                          self$imputed_predict <- TRUE
                                          self$flag <- 'train'
                                          if(!self$imputed){
                                            self$column_counter <- ncol(context)+1
                                            self$imputed <- TRUE
                                            data_to_impute <- cbind(feature,context)
                                            colnames(data_to_impute)[1] <- setdiff(self$state$context_cols,colnames(context))
                                            data_to_impute <- as.data.frame(data_to_impute)[,self$state$context_cols]
                                            function_call <-  imp_function(data_to_impute)
                                            self$data_imputed <-function_call$data
                                            self$model <- function_call$model


                                          }
                                          if(self$imputed){
                                            self$column_counter <- self$column_counter -1

                                          }
                                          if  (self$column_counter==0){
                                            self$imputed <- FALSE
                                          }
                                          self$train_s <- TRUE
                                          return(NULL)

                                        },
                                        .impute=function(feature, type, model, context){

                                          imp_function <- function(data_to_impute){




                                            data_to_impute <- as.data.frame(data_to_impute)
                                            # prepering arguments for function
                                            col_type <- 1:ncol(data_to_impute)
                                            for (i in col_type){
                                              col_type[i] <- class(data_to_impute[,i])
                                            }
                                            percent_of_missing <- 1:ncol(data_to_impute)
                                            for (i in percent_of_missing){
                                              percent_of_missing[i] <- (sum(is.na(data_to_impute[,i]))/length(data_to_impute[,1]))*100
                                            }
                                            col_miss <- colnames(data_to_impute)[percent_of_missing>0]
                                            col_no_miss <- colnames(data_to_impute)[percent_of_missing==0]

                                            if(nrow(data_to_impute)==1){
                                              data_train <- mice::complete(self$model)
                                              data_train <- rbind(data_train,data_to_impute[,self$state$context_cols])

                                              data_imputed <- mice.reuse(self$model,data_train,maxit=self$param_set$values$maxit,printFlag = T)$`1`[nrow(data_train),]
                                            }else{
                                            data_imputed <- mice.reuse(self$model,data_to_impute,maxit=self$param_set$values$maxit,printFlag = T)$`1`
                                            }




                                            return(data_imputed)
                                          }
                                          if (self$imputed){

                                            feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]



                                          }
                                          if((nrow(self$data_imputed)!=nrow(context) | !self$train_s) & self$flag=='train') {
                                            self$imputed_predict <- FALSE
                                            self$flag <- 'predict'
                                          }

                                          if(!self$imputed_predict){
                                            data_to_impute <- cbind(feature,context)
                                            colnames(data_to_impute)[1] <- setdiff(self$state$context_cols,colnames(context))
                                            # its important to keep the same columns order

                                            data_to_impute <- as.data.frame(data_to_impute)[,self$state$context_cols]
                                            self$data_imputed <- imp_function(data_to_impute)

                                        if(self$column_counter == 0 & self$flag=='train'){
                                          feature <- self$data_imputed[,setdiff(colnames(self$data_imputed),colnames(context))]
                                          self$flag <- 'predict'
                                          self$imputed_predict <- FALSE
                                        }
                                        self$train_s <- FALSE

                                        return(feature)
                                      }
                                    )
)

mlr_pipeops$add("miceA_imputation", PipeOpMice_A)



  #
  #   w <- PipeOpMice_A$new(m=1)
  #   gr <- w %>>% lrn('classif.rpart')
  #   gr <- GraphLearner$new(gr)
  # #
  # # gr
  #   resample(task,gr,rsmp('cv',folds=746))
  # # #
  # # # library(testthat)
  #
  # #w <- mice::mice(test[-sap,])
  #
  #
  # #d <- mice.reuse(w,test[sap,]
  #
  # w <- mice(iris)
  # w$predictorMatrix



  mice.reuse <- function(mids, newdata, maxit = 5, printFlag = TRUE, seed = NA){
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
    #    the random seed at the end of the procedure

    if(is.na(seed)){
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
    mids.new <- mice::mice(newdata, mids$m, where = all_miss, maxit = 0,predictorMatrix = mids$predictorMatrix)

    # Combine the old (trained) and the new mids objects
    mids.comb <- mids.append(mids, mids.new)
    new_idx <- mids.comb$app_idx
    mids.comb <- mids.comb$mids

    mids.comb$lastSeedValue <- .Random.seed # set the seed to the current value

    # Set the newdata to missing (so it doesn't influence the imputation)
    # but remember the actual values
    actual_data <- mids.comb$data
    mids.comb$data[mids.comb$where] <- NA

    # Also make sure all observed variables in the newdata are set to their
    # true values in the imputations
    for(j in names(mids.comb$imp)){
      for(i in seq_len(mids.comb$m)){
        mids.comb$imp[[j]][, i] <-
          replace_overimputes(actual_data, mids.comb$imp, j, i)
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
    cond_imp <- "imp[[j]][, i] <- replace_overimputes(fetch_data(), imp, j, i)"
    mids.comb$post <- sapply(mids.comb$post,
                             function(x) if(x != "") paste0(x, "; ", cond_imp)
                             else cond_imp)

    # Run the procedure for a few times
    mids.comb <- mice::mice.mids(mids.comb, maxit = maxit, printFlag = printFlag)

    # Return the imputed test dataset
    res <- lapply(complete(mids.comb, "all"), function(x) x[new_idx, ])
    class(res) <- c("mild", "list")
    res
  }


  mids.append <- function(x, y){
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
    for(i in names(x$imp)){
      if(i %in% miss_xy){
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

    list(mids = app, app_idx = setNames(y_idx, NULL))
  }


  replace_overimputes <- function(data, imp, j, i){
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


  fetch_data <- function(){
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

    get('actual_data', pos = parent.frame(5))
  }



  # w <- mice(test[-3,])
  # c <- complete(w)


