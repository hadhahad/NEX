# Load packages
PACKAGES <- c("dplyr")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[,"Package"])]
if(length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))


loadData <- function(FILE, DEPENDENT_VARIABLE) {

  # Load the csv-file
  data <- as.data.frame(read.csv(FILE, sep = ","))
  
  # Remove index column if present
  if ('X' %in% colnames(data)) {
    data <- data %>% 
      select(-X)
  }

  # Format factor variables
  factor_vars <- setdiff(colnames(data), c(DEPENDENT_VARIABLE))
  data[, factor_vars] <- lapply(
    data[, factor_vars],
    function(x) as.factor(x)
  )

  # Return data
  return(data)
}
