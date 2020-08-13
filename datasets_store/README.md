# Content of datasets_store
### /datasets_selection
- searching for all datasets with missing values from OpenML
- filtering datasets to binary classification and regression tasks
- removing duplicates of datasets caused by versioning, leaving the oldest (the first uploaded to OML) version of datasets
- obtaining list of 155 datasets for further preprocessing and analysis
- **update 30.07.2020**: some of selected datasets had missings in target variable, after removing them 
133 datasets left

### /information_base
- directory with JSON files which contain information about datasets

### /patterns_base
- directory with csv files which contain missings patterns from datasets

### /clustering_experiment
- attempt at clustering to find groups of similar datasets both complete and with missings

### /patterns_analysis
- *MCAR_test*: computing Little's test on MCAR hypothesis
- *exploring_patterns*: gathering more information about single patterns of missings

### preprocess
- script with function to preprocess dataset downloaded from OpenML

### cleaning_storing
- script with automated preprocessing of datasets
- writing JSON files with datasets summary

### create_new_summary
- function which collects information about datasets to JSON, and missings patterns to csv

### datasets_test_sample.csv
- manually selected 10 datasets for pipeline tests and patterns analysis attempts
