library(mlr3)
library(mlr3learners)
library("mlr3pipelines")
library(mlr3measures)
library(ROCR)

set.seed(123)



metrics_df <- data.frame()
write.csv(metrics_df, 'metrics_results/metrics_df.csv')
# write.csv(metrics_df, 'metrics_df.csv')
modelling_for_method <- function(imputed_list, suffix = 'mean'){
    model_results_for_method <- rep(list(), length(imputed_list))
  for(i in seq_along(imputed_list)){
    set.seed(521)
    print(names(imputed_list)[i])
    if(any(imputed_list[[i]] != 'ERROR')){
      train_index <- 1:nrow(imputed_list[[i]]$imputed_data$data_train)  
      
      data <- rbind(imputed_list[[i]]$imputed_data$data_train,
                    imputed_list[[i]]$imputed_data$data_test)
      
      class_names <- levels(data[,imputed_list[[i]]$target_name])
      
      if(!is.unsorted(class_names)){
        class_names <- sort(class_names, decreasing = TRUE)
        data[,imputed_list[[i]]$target_name] <- factor(data[,imputed_list[[i]]$target_name],
                                                               levels = class_names)
        
      }
      
      test_index <- (max(train_index) + 1):nrow(data)
      
      task = TaskClassif$new(id = paste(names(imputed_list)[i], suffix, sep = '_'), backend = data,
                             target = imputed_list[[i]]$target_name)
      
      
      learner_type_list <- c( 'classif.rpart',
                             "classif.glmnet"
                              ,'classif.kknn',
                              'classif.ranger', 
                              'classif.xgboost'
                             )
      
      model_results <- rep(list(), length(learner_type_list))
      
      for(j in seq_along(learner_type_list)){
         metrics_df <- read.csv('/data/shared/kwoznica/AutoImpute/metrics_results/metrics_df.csv', blank.lines.skip = FALSE)
        tryCatch({
          print(learner_type_list[j])
          
            encode = mlr_pipeops$get("encode")
            graph = encode %>>%
              mlr_pipeops$get("learner",
                              learner = lrn(learner_type_list[j], predict_type = 'prob'))
            
            glrn = GraphLearner$new(graph)
            glrn$train(task, row_ids = train_index)
            prediction = glrn$predict(task, row_ids = test_index)
            prediction_train = glrn$predict(task, row_ids = train_index)
            
            measure = msr("classif.auc")
            measure2 = msr("classif.fbeta")
            model_results[[j]] <- list(prediction_train = prediction_train,
                                       prediction_test = prediction,
                                       auc = prediction$score(measure),
                                       f1 = prediction$score(measure2))
            
            
         
          
          
          tmp <- list(dataset = c(names(imputed_list)[i], names(imputed_list)[i]),
                      imputation_method = c(suffix, suffix),
                      model = c(learner_type_list[j], learner_type_list[j]),
                      metric = c('auc', 'f1'),
                      metric_value = c(model_results[[j]]$auc,
                                       model_results[[j]]$f1))
          
          
          tmp <- as.data.frame(tmp,
                     row.names = NULL)
          rownames(tmp) <- NULL
          
          
          if(nrow(metrics_df) == 0){
            metrics_df <- tmp
          }else{
            metrics_df <- metrics_df[,-1]
            metrics_df <- data.table::rbindlist(list(metrics_df,
                                tmp), use.names = TRUE,
                                fill = TRUE)
          }
          print(metrics_df)


          ## update .csv file with results
          write.csv(metrics_df, '/data/shared/kwoznica/AutoImpute/metrics_results/metrics_df.csv')
        },
        error = function(cond){
          message(paste("Error in ", learner_type_list[j]))
          message(cond)
          model_results[[j]] <- 'ERROR'
        })
        
        
        
        
      }
      # print(metrics_df)
      names(model_results) <- learner_type_list
      model_results_for_method[[i]] <- model_results
    }else{
      model_results_for_method[[i]] <- list(classif.auc =  'ERROR', classif.fbeta = 'ERROR')
    }
    
    }
    
    ## save Rds object
  names(model_results_for_method) <-   names(imputed_list)
  model_results_for_method
      saveRDS(model_results_for_method, paste0('metrics_results/metrics_',suffix, '.Rds' ))
  
return(model_results_for_method)
}



 imputed_all <- lapply(list.files('dataset_imputed', full.names = TRUE), readRDS)

names(imputed_all) <- c('mean', 'mice_default', 'missForest', 'random',  'softImpute', 'VIM_hotdeck', 'VIM_kknn')
names(imputed_all$VIM_kknn) <-names(imputed_all$mean)

imputed_all_clean <- lapply(imputed_all, function(x) x[setdiff(names(x), 'openml_dataset_41278')])

no_of_imputed_data <- sapply(imputed_all_clean, function(y) length(y)-sum(sapply(y, function(x) any(x =='ERROR'))))


 imputed_all_clean_02 <- imputed_all_clean[no_of_imputed_data>0]

print(names(imputed_all_clean_02))

metric_results <- mapply(function(x, y) modelling_for_method(x,y), imputed_all_clean_02, names(imputed_all_clean_02))
names(metric_results) <- names(imputed_all_clean_02)
