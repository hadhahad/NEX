loadData <- function(FILE) {

  # Load the csv-file
  data <- as.data.frame(read.csv(FILE, sep = ","))

  # Format factor variables
  factor_vars <- setdiff(colnames(data), c("measurement"))
  data[, factor_vars] <- lapply(
    data[, factor_vars],
    function(x) as.factor(x)
  )

  # Return data
  return(data)
}
