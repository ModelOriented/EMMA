register_nadia = function() {

  x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")

  x$add("Amelia_imputation", PipeOpAmelia)
  x$add("VIM_HD_imputation", PipeOpVIM_HD)
  x$add("VIM_IRMI_imputation", PipeOpVIM_IRMI)
  x$add("VIM_kNN_imputation", PipeOpVIM_kNN)
  x$add("VIM_regrImp_imputation", PipeOpVIM_regrImp)
  x$add("impute_hist_B", PipeOpHist_B)
  x$add("impute_mean_B", PipeOpMean_B)
  x$add("impute_median_B", PipeOpMedian_B)
  x$add("impute_mode_B", PipeOpMode_B)
  x$add("impute_oor_B", PipeOpOOR_B)
  x$add("impute_sample_B", PipeOpSample_B)
  x$add("miceA_imputation", PipeOpMice_A)
  x$add("mice_imputation", PipeOpMice)
  x$add("missForest_imputation", PipeOpmissForest)
  x$add("missMDA_MCA_PCA_FMAD_imputation", PipeOpMissMDA_PCA_MCA_FMAD)
  x$add("missMDA_MFAimputation", PipeOpMissMDA_MFA)
  x$add("missRanger_imputation", PipeOpmissRanger)
  x$add("simulate_missings", PipeOpSimulateMissings)
  x$add("softImpute_imputation", PipeOpSoftImpute)
  x$add("simulate_missings", PipeOpSimulateMissings)
  x$add("missMDA_MCA_PCA_FMAD_imputation_A", PipeOpMissMDA_PCA_MCA_FMAD_A)
  x$add("missMDA_MFAimputation_A",PipeOpMissMDA_MFA_A)
}

.onLoad = function(libname, pkgname) { # nolint
  register_nadia()
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_nadia(), action = "append")
} # nocov end

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "nadia"], action = "replace")
} # nocov end
