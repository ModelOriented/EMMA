dataset_train_test_list <- readRDS('dataset_train_test_list.Rds')


source('R/imputation_function.R')


library(parallelMap)
no_cores <- parallel::detectCores()
parallelStartMulticore(no_cores/3, show.info = TRUE)


imputed_data_mean <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,imputation_mode_median)}, impute.error = function(x){'ERROR'})
names(imputed_data_mean) <- names(dataset_train_test_list)
saveRDS(imputed_data_mean, 'dataset_imputed/imputed_data_mean.Rds')

imputed_data_softimpute <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,imputation_softimpute)}, impute.error = function(x){'ERROR'})
names(imputed_data_softimpute) <- names(dataset_train_test_list)
saveRDS(imputed_data_softimpute, 'dataset_imputed/imputed_data_softimpute.Rds')

## random
imputed_data_random <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,impute_random)}, impute.error = function(x){'ERROR'})
names(imputed_data_random) <- names(dataset_train_test_list)
saveRDS(imputed_data_random, 'dataset_imputed/imputed_data_random.Rds')



imputed_data_mice <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,imputation_fun_mice)}, impute.error = function(x){'ERROR'})
names(imputed_data_mice) <- names(dataset_train_test_list)
saveRDS(imputed_data_mice, '/dataset_imputed/imputed_data_mice_default.Rds')


imputed_data_missForest <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,imputation_fun_missForest)}, impute.error = function(x){'ERROR'})
names(imputed_data_missForest) <- names(dataset_train_test_list)
saveRDS(imputed_data_missForest, 'dataset_imputed/imputed_data_missForest.Rds')



imputed_data_VIM <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,imputation_fun_vim)}, impute.error = function(x){'ERROR'})
names(imputed_data_mean) <- names(dataset_train_test_list)
saveRDS(imputed_data_VIM, 'dataset_imputed/imputed_data_VIM.Rds')




## vim hotdeck
imputed_data_vim_hotdeck <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,imputation_fun_vim_hotdeck)}, impute.error = function(x){'ERROR'})
names(imputed_data_vim_hotdeck) <- names(dataset_train_test_list)
saveRDS(imputed_data_vim_hotdeck, 'dataset_imputed/imputed_data_vim_hotdeck.Rds')

# ## missMDA
#
# imputed_data_missMDA <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,impute_missMDA)}, impute.error = function(x){'ERROR'})
# names(imputed_data_missMDA) <- names(dataset_train_test_list)
# saveRDS(imputed_data_missMDA, 'dataset_imputed/imputed_data_missMDA.Rds')
#
## Amelia

# imputed_data_Amelia <- parallelLapply(dataset_train_test_list, function(x){get_imputed_data(x,imputation_fun_amelia)}, impute.error = function(x){'ERROR'})
# names(imputed_data_Amelia) <- names(dataset_train_test_list)
# saveRDS(imputed_data_Amelia, 'dataset_imputed/imputed_data_Amelia.Rds')