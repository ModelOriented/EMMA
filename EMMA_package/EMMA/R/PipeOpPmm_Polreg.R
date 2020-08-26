#' @title FOR TESTING ONLY . Function implement PMM and Polregr imputation methods
#'
#' @name PipeOpPmm_Polreg
#'
#' @description
#' PipeOp imputation of PMM and Polregr methods with fit transform interface.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from \code{\link{PipeOpImpute}}.
#'
#' @details Function use Predictive mean meaching to impute numerical veriblse and Polregr for numerical.
#' In PMM categorical veribles are labeld and treated as integers. Imputations work with mice algorithm. More can be found here : https://stefvanbuuren.name/fimd/
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], as well as: \cr
#' \itemize{
#' \item \code{id} :: \code{character(1)}\cr
#' Identifier of resulting object, default \code{"imput_pmm"}.
#' \item \code{m} :: \code{integer(1)}\cr
#' number of iterations for mice, default \code{5}.
#' \item \code{donors} :: \code{integer(1)}\cr
#' number of donors for pmm , default \code{5}.
#'
#'}
#'
#' @export

PipeOpPmm <-  R6::R6Class("pmm_imputation",lock_objects=FALSE,
                           inherit = PipeOpImpute,
                           public = list(
                             initialize = function(id = "imput_pmm",m=5,donors=5
                             ) {
                               super$initialize(id, whole_task_dependent=TRUE,param_vals=list(m=m,donors=donors),
                                                param_set = ParamSet$new(list('m'=ParamInt$new('m',lower = 1,upper = 50,default = 5),
                                                                              'donors'=ParamInt$new('donors',lower = 1,upper = 20,default = 5)))
                                            )






                               self$trained <- FALSE

                             }),private=list(

                               .train_imputer=function(feature, type, context){
                                 #### RESETING MODEL AFTER IMPUTATION
                                 if(setdiff(self$state$context_cols,colnames(context))==self$state$context_cols[[1]]){
                                   self$trained <- F
                                 }
                                if (!self$trained){

                                ####  Creting df
                                df <- as.data.frame(cbind(feature,context))
                                colnames(df)[1] <- setdiff(self$state$context_cols,colnames(context))


                                where_df <- as.data.frame(is.na(df))



                                ### Random valu sampling
                                df <- lapply(df,function(x){

                                  rx <- !is.na(x)
                                  if( class(x)=='factor'){

                                    x[!rx] <- sample(x[rx],sum(!rx),replace = T)
                                    return(as.factor(x))

                                  }
                                  else{
                                  x[!rx] <- sample(x[rx],sum(!rx),replace = T)
                                  return(x)
                                  }

                                })
                                df <- as.data.frame(df)


                                params <- list()
                                # First mice loop
                                 for (i in 1:(self$param_set$values$m+1)){

                                   #Second mice loop
                                   for (j in 1:ncol(df)){

                                     #### SELECTING METHOD ####
                                     numeric <- FALSE
                                     factor <- FALSE
                                     factor_2 <- F


                                     if (class(df[,j]) %in% c("factor", "ordered", "character")) {
                                       d <- as.factor(df[,j])


                                       if (length(levels(d))<=2){factor_2 <- T}
                                       else{factor <- T}
                                     }
                                     else{numeric <- T}
                                     ry <- !where_df[,j]
                                     if (numeric){



                                      df_n <- df
                                     # to intiger
                                     df_n <- lapply(df_n, FUN=function(x){
                                       if(class(x ) %in% c("factor", "ordered", "character")){
                                         x <- as.factor(x)
                                         x  <- as.integer(x)
                                       }
                                       x
                                     })


                                     df_n <- as.data.frame(df_n)

                                     y <- df_n[,j]
                                     par <- mice::norm.draw(y,ry,as.matrix(df_n[,-j]))

                                     if(i == (self$param_set$values$m+1)){

                                       params[[colnames(df)[j]]] <- par
                                     }
                                     else{
                                       if(sum(!ry)==0){next}

                                       yhatobs <- as.matrix(df_n[,-j][ry, , drop = FALSE]) %*% par$coef
                                       yhatmis <- as.matrix(df_n[,-j][!ry, , drop = FALSE]) %*% par$beta

                                       idx  <- .Call("_mice_matcher", PACKAGE = "mice", yhatobs, yhatmis,self$param_set$values$donors)

                                       df[,j][!ry] <- df_n[,j][ry][idx]
                                     }

                                     }
                                     if(factor){
                                        if(sum(!ry)==0 & i < (self$param_set$values$m+1)){next}
                                        polreg <- mlr3learners::LearnerClassifMultinom$new()
                                        train_task <- TaskClassif$new('task',df[ry,],colnames(df)[j])

                                        capture.output(polreg$train(train_task))

                                        if(i == (self$param_set$values$m+1)){

                                          params[[colnames(df)[j]]] <- polreg
                                        }
                                        else{

                                        df[!ry,j] <- polreg$predict(TaskClassif$new('task',df[!ry,],colnames(df)[j]))$response
                                        }

                                     }
                                     if(factor_2){
                                       if(sum(!ry)==0 & i < (self$param_set$values$m+1)){next}
                                       polreg <- mlr3learners::LearnerClassifLogReg$new()
                                       train_task <- TaskClassif$new('task',df[ry,],colnames(df)[j])

                                       capture.output(polreg$train(train_task))

                                       if(i == (self$param_set$values$m+1)){

                                         params[[colnames(df)[j]]] <- polreg
                                       }
                                       else{

                                         df[!ry,j] <- polreg$predict(TaskClassif$new('task',df[!ry,],colnames(df)[j]))$response
                                       }

                                     }


                                     }


                                 }
                                df <- lapply(df, FUN=function(x){
                                  if(class(x ) %in% c("factor", "ordered", "character")){
                                    x <- as.factor(x)
                                    x  <- as.integer(x)
                                  }
                                  x
                                })


                                df <- as.data.frame(df)

                                self$model_n <- list('params'=params,'data_n' =df)
                                self$trained_n <- T
                                }





                                return(NULL)
                               }
                               ,
                                .impute=function(feature, type, model, context){
                                # Column number


                                if(!self$imputed){

                               ry <- !is.na(feature)
                               f_name <- setdiff(self$state$context_cols,colnames(context))
                               for (i in 1:self$param_set$values$m){

                               #### SELECTING METHOD ####
                               numeric <- FALSE
                               factor <- FALSE
                               factor_2 <- F



                               if (class(feature) %in% c("factor", "ordered", "character")) {
                                 feature <- as.factor(feature)


                                if (length(levels(feature))<=2){factor_2 <- T}
                                else{factor <- T}
                               }
                               else{numeric <- T}

                               ###
                               if(numeric){

                                 f_param <- self$model_n$params[[f_name]]

                                 yhatobs <- as.matrix(self$model_n$data_n[,ifelse(colnames(self$model_n$data_n)==f_name,F,T)]) %*% f_param$coef

                                 df <- lapply(context, FUN=function(x){
                                   if(class(x ) %in% c("factor", "ordered", "character")){
                                     x <- as.factor(x)
                                     x  <- as.integer(x)
                                   }
                                   x
                                 })
                                 context <- as.data.frame(df)

                                 yhatmis <- as.matrix(context[!ry, , drop = FALSE]) %*% f_param$beta


                                 idx  <- .Call("_mice_matcher", PACKAGE = "mice", yhatobs, yhatmis,self$param_set$values$donors)
                                 feature[!ry] <- self$model_n$data_n[,f_name][idx]
                               }

                               if (factor){


                                 feature[!ry] <- self$model_n$params[[f_name]]$predict(TaskClassif$new('test',as.data.frame(cbind(feature,context))[!ry,],'feature'))$response

                               }


                               if (factor_2){


                                feature[!ry] <- self$model_n$params[[f_name]]$predict(TaskClassif$new('test',as.data.frame(cbind(feature,context))[!ry,],'feature'))$response

                               }
                               return(feature)
                                }}}




                             )
)
#  test_pmm <- PipeOpPmm$new()
# # #
#   gr <- test_pmm %>>% lrn('classif.rpart')
# # #
#  grln <- GraphLearner$new(gr)
# # # glrn$encapsulate =c(train='evalute',predict='evalute')
# # #
# # # glrn
# # #
# # # summary(as.data.frame(test_task$data()))
# # # sum(is.na(df))
# #
# #
# #
#  resample(TaskClassif$new('t',df,colnames(df)[ncol(df)]),grln,rsmp('cv',folds=10))



