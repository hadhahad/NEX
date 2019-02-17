# Factorize the whole data frame with exception of the dependent variable
toFactor <- function(df, DEP_VAR, LEVELS = c("-1", "0", "1")) {

  # Format factor variables
  FACTOR_VARS <- setdiff(colnames(df), c(DEP_VAR))
  df[, FACTOR_VARS] <- lapply(
    df[, FACTOR_VARS],
    function(x) factor(x, levels = LEVELS, ordered = TRUE)
  )

  return(df)
}
