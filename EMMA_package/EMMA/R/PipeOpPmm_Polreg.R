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

PipeOpPmm <-  R6::R6Class("mice_self_imputation",lock_objects=FALSE,
                                   inherit = PipeOpTaskPreproc,  # inherit from PipeOp
                                   public = list(
                                     initialize = function(id = "imput_mice_self",m=5
                                     ) {
                                       super$initialize(id,param_vals=list(m=5),param_set =ParamSet$new(list('m'=ParamInt$new('m',lower = 1,upper = 100,default = 5)))



                                                        )







                                     }),private=list(

                                       .train_task=function(task){

                                         data_to_impute <- as.data.frame( task$data(cols = task$feature_names))
                                         targer <- as.data.frame(task$data(cols = task$target_names))

                                         #### Data Imputation ####

                                         # missing data position
                                         where_miss <- is.na(data_to_impute)


                                         # Random data imputation
                                         data_to_impute <- lapply(data_to_impute, function(x){

                                           x[is.na(x)] <- sample(x[!is.na(x)],sum(is.na(x)),replace = T)
                                           x

                                         })
                                         data_to_impute <- as.data.frame(data_to_impute)

                                         model <- list()

                                         # First mice loop
                                         for (i in 1:(self$param_set$values$m+1)){


                                           ### Second mice loop
                                           for (j in 1:ncol(data_to_impute)){



                                           numeric <- F
                                           factor <- F
                                           factor_2 <- F

                                           ### Chosing model
                                           vector_to_impute <- data_to_impute[,j]
                                           ry <- !where_miss[,j]
                                           if(class(vector_to_impute)=='factor'){
                                             if(length(levels(na.omit(vector_to_impute)))==2){factor_2 <- T}
                                             else{factor <- T}
                                           }
                                           else{numeric <- T}


                                           ### numeric
                                           if(numeric){


                                             numeric_df <- data_to_impute

                                             ### converting factor to int
                                             numeric_df <- lapply(numeric_df, function(x){

                                               if(class(x)=='factor'){return(as.integer(x))}
                                               return(x)
                                             })
                                             numeric_df <- as.data.frame(numeric_df)

                                             #### performin alghoritm
                                             par <- mice::norm.draw(vector_to_impute,ry,as.matrix(numeric_df[,-j]))

                                             if(i == (self$param_set$values$m+1)){
                                               model[[colnames(data_to_impute)[j]]] <- par
                                               next
                                             }
                                             if(sum(!ry)==0){next}


                                             yhatobs <- as.matrix(numeric_df[,-j][ry, , drop = FALSE]) %*% par$coef
                                             yhatmis <- as.matrix(numeric_df[,-j][!ry, , drop = FALSE]) %*% par$beta

                                             idx  <- .Call("_mice_matcher", PACKAGE = "mice", yhatobs, yhatmis,5)

                                             data_to_impute[,j][!ry] <- numeric_df[,j][ry][idx]


                                           }

                                           #### factor
                                           if (factor ){
                                             if(sum(!ry)==0 & i < (self$param_set$values$m+1)){next}

                                             polreg <- mlr3learners::LearnerClassifMultinom$new()
                                             train_task <- TaskClassif$new('task',data_to_impute[ry,],colnames(data_to_impute)[j])

                                             capture.output(polreg$train(train_task))


                                           if(i == (self$param_set$values$m+1)){

                                             model[[colnames(data_to_impute)[j]]] <- polreg
                                           }
                                           else{

                                             data_to_impute[!ry,j] <- polreg$predict(TaskClassif$new('task',data_to_impute[!ry,],colnames(data_to_impute)[j]))$response
                                           }



                                           }

                                          ####
                                           if(factor_2){

                                             if(sum(!ry)==0 & i < (self$param_set$values$m+1)){next}
                                             polreg <- mlr3learners::LearnerClassifLogReg$new()
                                             train_task <- TaskClassif$new('task',data_to_impute[ry,],colnames(data_to_impute)[j])

                                             capture.output(polreg$train(train_task))

                                             if(i == (self$param_set$values$m+1)){

                                               model[[colnames(data_to_impute)[j]]] <- polreg
                                             }
                                             else{

                                               data_to_impute[!ry,j] <- polreg$predict(TaskClassif$new('task',data_to_impute[!ry,],colnames(data_to_impute)[j]))$response
                                             }

                                           }
                                           }
                                         }
                                         data_imputed <- data_to_impute

                                         self$model <- list('col_mods'=model,'train_data'=data_imputed)


                                         data_imputed <-  cbind(data_imputed,task$row_ids)
                                         colnames(data_imputed)[ncol(data_imputed)] <- task$backend$primary_key
                                         task$cbind(as.data.table(data_imputed))

                                       },
                                       .predict_task=function(task){
                                         data_to_impute <- as.data.frame( task$data(cols = task$feature_names))


                                         train_data_no_numeric <- self$model$train_data

                                         train_data  <- lapply(train_data_no_numeric, function(x){

                                           if(class(x)=='factor'){return(as.integer(x))}
                                           return(x)
                                         })
                                         train_data <- as.data.frame(train_data)

                                         where_miss <- is.na(data_to_impute)
                                         # Random imputation with train data

                                         for (i in 1:ncol(data_to_impute)){

                                             data_to_impute[is.na(data_to_impute[,i]),i] <- sample(train_data_no_numeric[,i],sum(is.na(data_to_impute[,i])),replace = T)


                                         }


                                         # Only outer mice loop
                                         for( i in 1:self$param_set$values$m){

                                           for (j in 1:ncol(data_to_impute)){

                                           numeric <- F
                                           factor <- F
                                           factor_2 <- F

                                           ### Chosing model
                                           vector_to_impute <- data_to_impute[,j]
                                           ry <- !where_miss[,j]
                                           if(class(vector_to_impute)=='factor'){
                                             if(length(levels(na.omit(vector_to_impute)))==2){factor_2 <- T}
                                             else{factor <- T}
                                           }
                                           else{numeric <- T}

                                            if(numeric){

                                              if(sum(!ry)==0){next}
                                              #converting to int
                                              data_to_impute_numeric <- lapply(data_to_impute, function(x){

                                                if(class(x)=='factor'){return(as.integer(x))}
                                                return(x)
                                              })
                                              data_to_impute_numeric <- as.data.frame(data_to_impute_numeric)

                                              par <- self$model$col_mods[[colnames(data_to_impute)[j]]]

                                              yhatobs <- as.matrix(train_data[,-j]) %*% par$coef
                                              yhatmis <- as.matrix(data_to_impute_numeric[,-j][!ry, , drop = FALSE]) %*% par$beta

                                              idx  <- .Call("_mice_matcher", PACKAGE = "mice", yhatobs, yhatmis,5)

                                              data_to_impute[,j][!ry] <- train_data[,j][idx]

                                            }
                                           if(factor){

                                             if(sum(!ry)==0){next}

                                             data_to_impute[!ry,j]<- self$model$col_mods[[colnames(data_to_impute)[j]]]$predict(TaskClassif$new('task',data_to_impute[!ry,],colnames(data_to_impute)[j]))$response

                                           }
                                           if (factor_2){
                                             if(sum(!ry)==0){next}

                                             data_to_impute[!ry,j]<- self$model$col_mods[[colnames(data_to_impute)[j]]]$predict(TaskClassif$new('task',data_to_impute[!ry,],colnames(data_to_impute)[j]))$response
                                           }

                                           }
                                         }
                                         data_imputed <- data_to_impute




                                         data_imputed <-  cbind(data_imputed,task$row_ids)
                                         colnames(data_imputed)[ncol(data_imputed)] <- task$backend$primary_key
                                         task$cbind(as.data.table(data_imputed))
                                       }


                                     )
)
#
  test_pmm <- PipeOpPmm$new()
# # # #
    gr <- test_pmm %>>% lrn('classif.rpart')
# # # #
   grln <- GraphLearner$new(gr)
# # # # glrn$encapsulate =c(train='evalute',predict='evalute')
# # # #
# # # # glrn
# # # #
# # # # summary(as.data.frame(test_task$data()))
# # # # sum(is.na(df))
# # #
# # #
# # #
#
# test_pmm$train(list(task))
   resample(task,grln,rsmp('cv',folds=10))

