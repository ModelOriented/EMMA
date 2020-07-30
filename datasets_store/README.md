# Content of datasets_store
### /datasets_selection
- searching for all datasets with missing values from OpenML
- filtering datasets to binary classification and regression tasks
- removing duplicates of datasets caused by versioning, leaving the oldest (the first uploaded to OML) version of datasets
- obtaining list of 155 datasets for further preprocessing and analysis

### /information_base
- directory with JSON files which contain information about datasets

### cleaning_storing
- script with automated preprocessing of datasets
- writing JSON files with datasets summary

### create_new_summary
- function which collect information about datasets and creates JSON