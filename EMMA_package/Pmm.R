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
                                 if(exists('col_with_miss ') && col_with_miss ==0){
                                   self$trained <- F
                                 }
                                if (!self$trained){

                                ####  Creting df
                                df <- as.data.frame(cbind(feature,context))
                                colnames(df)[1] <- setdiff(self$state$context_cols,colnames(df))


                                where_df <- as.data.frame(is.na(df))



                                ### Random valu sampling
                                df <- sapply(df,function(x){

                                  rx <- !is.na(x)

                                  x[!rx] <- sample(x[rx],sum(!rx),replace = T)
                                  x
                                })
                                df <- as.data.frame(df)


                                # First mice loop
                                 for (i in 1:(self$param_set$values$m+1)){

                                   #Second mice loop
                                   for (j in 1:ncol(df)){

                                     #### SELECTING METHOD ####
                                     numeric <- FALSE
                                     factor <- FALSE



                                     if (class(df[,j]) %in% c("factor", "ordered", "character")) {
                                       d <- as.factor(df[,j])

                                       factor <- T
                                     }
                                     else{numeric <- T}
                                     ry <- !where_df[,j]
                                     if (numeric){




                                     # to intiger
                                     df <- sapply(df, FUN=function(x){
                                       if(class(x ) %in% c("factor", "ordered", "character")){
                                         x <- as.factor(x)
                                         x  <- as.integer(x)
                                       }
                                       x
                                     })


                                     df <- as.data.frame(df)

                                     y <- df[,j]
                                     par <- mice::norm.draw(y,ry,as.matrix(df[,-j]))

                                     if(i == (self$param_set$values$m+1)){

                                       params[[colnames(df)[j]]] <- par
                                     }
                                     else{
                                       if(sum(!ry)==0){next}

                                       yhatobs <- as.matrix(df[,-j][ry, , drop = FALSE]) %*% par$coef
                                       yhatmis <- as.matrix(df[,-j][!ry, , drop = FALSE]) %*% par$beta

                                       idx  <- .Call("_mice_matcher", PACKAGE = "mice", yhatobs, yhatmis,self$param_set$values$donors)

                                       df[,j][!ry] <- df[,j][ry][idx]
                                     }

                                     }
                                     if(factor){
                                        if(sum(is.na(df[,j]))==0 & i < (self$param_set$values$m+1)){next}
                                        polreg <- lrn('classif.multinom')
                                        train_task <- TaskClassif$new('task',df[ry,],colnames(df)[j])

                                        capture.output(polreg$train(train_task))

                                        if(i == (self$param_set$values$m+1)){

                                          params[[colnames(df)[j]]] <- polreg
                                        }
                                        else{

                                        df[!ry,j] <- polreg$predict(TaskClassif$new('task',df[!ry,],colnames(df)[j]))
                                        }

                                     }


                                     }


                                 }
                                self$model_n <- list('params'=params,'data_n' =df)
                                self$trained_n <- T
                                }





                                return(NULL)
                               }
                               ,
                                .impute=function(feature, type, model, context){
                                # Column number
                                col_with_miss   <- 1+sum(sapply(test,FUN = function(x){

                                  as.logical(sum(is.na(x)))

                                }))
                                col_with_miss <- col_with_miss*2

                               ry <- !is.na(feature)
                               f_name <- setdiff(self$state$context_cols,colnames(context))

                               #### SELECTING METHOD ####
                               numeric <- FALSE
                               factor <- FALSE



                               if (type %in% c("factor", "ordered", "character")) {
                                 feature <- as.factor(feature)

                                factor <- T
                               }
                               else{numeric <- T}

                               ###
                               if(numeric){

                                 f_param <- self$model_n$params[[f_name]]

                                 yhatobs <- as.matrix(self$model_n$data_n[,ifelse(colnames(self$model_n$data_n)==f_name,F,T)]) %*% f_param$coef
                                 yhatmis <- as.matrix(context[!ry, , drop = FALSE]) %*% f_param$beta


                                 idx  <- .Call("_mice_matcher", PACKAGE = "mice", yhatobs, yhatmis,self$param_set$values$donors)
                                 feature[!ry] <- self$model_n$data_n[,f_name][idx]
                               }

                               if (factor){


                                 feature[!ry] <- model_n$params[[f_name]]$predict(TaskClassif$new('test',as.data.frame(cbind(feature,context)))[!ry,],'feature')

                               }
                                col_with_miss <- col_with_miss -1
                                return(feature)
                                }




                             )
)
test_pmm <- PipeOpPmm$new()

gr <- test_pmm %>>% learner_po

grln <- GraphLearner$new(gr)
glrn$encapsulate =c(train='evalute',predict='evalute')

glrn

summary(as.data.frame(test_task$data()))
sum(is.na(df))

