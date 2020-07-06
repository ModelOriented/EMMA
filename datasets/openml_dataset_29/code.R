library(OpenML)
library(readr)

source("create_summary_json.R")

datasets <- listOMLDataSets()
data_name <- "credit-approval"
openml_id <- datasets[datasets$name == data_name ,]$data.id

data <- getOMLDataSet(data.id= openml_id)
target_column <- data$target.features

df <- data$data

# change ? to NA
for (col in colnames(df)){
  df[col][df[col] == "?"] <- NA
}
# lets see if it did it

change_to_factor <- function(df){
  for (i in seq_along(colnames(df))){
    if (!is.numeric(df[,i])){
      df[,i] <- as.factor(df[,i])
    }
  }
  return(df)
}
df <- change_to_factor(df)



colnames(df) <- c("Sex",
                    "Age",
                    "Debt",
                    "Married",
                    "BankCustomer",
                    "EducationLevel",
                    "Ethicity",
                    "Years_employed",
                    "PriorDefault",
                    "Employed",
                    "CreditScore",
                    "Driverslicense",
                    "Citizen",
                    "Zipcode",
                    "Income",
                    "class")

dataset <- df

# doing summary with create json
json_summary <- CreateSummary(data=df, target_column=target_column, id=openml_id, data_name=data_name, source='openml', added_by='jakwisn')
write(json_summary, 'dataset.json')

