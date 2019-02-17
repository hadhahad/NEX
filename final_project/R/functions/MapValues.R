# Load packages
PACKAGES <- c("plyr")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[, "Package"])]
if (length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))


# Definition of the list of values categorized map
MAP_VALUES_1 <- list(
  "n_estimators" = list(
    actual = c("400", "450", "500"),
    mapped = c("-1", "0", "1")
  ),
  "min_samples_split" = list(
    actual = c("2", "4", "6"),
    mapped = c("-1", "0", "1")
  ),
  "max_features" = list(
    actual = c("sqrt", "log2"),
    mapped = c("-1", "1")
  ),
  "max_depth" = list(
    actual = c("20", "30", "40"),
    mapped = c("-1", "0", "1")
  ),
  "criterion" = list(
    actual = c("entropy", "gini"),
    mapped = c("-1", "1")
  ),
  "bootstrap" = list(
    actual = c("True", "False"),
    mapped = c("-1", "1")
  )
)


MAP_VALUES_2 <- list(
  "n_estimators" = list(
    actual = c("10", "55", "100"),
    mapped = c("-1", "0", "1")
  ),
  "min_samples_split" = list(
    actual = c("4", "12", "20"),
    mapped = c("-1", "0", "1")
  ),
  "max_features" = list(
    actual = c("sqrt", "log2"),
    mapped = c("-1", "1")
  ),
  "max_depth" = list(
    actual = c("10", "25", "40"),
    mapped = c("-1", "0", "1")
  ),
  "criterion" = list(
    actual = c("entropy", "gini"),
    mapped = c("-1", "1")
  ),
  "bootstrap" = list(
    actual = c("True", "False"),
    mapped = c("-1", "1")
  )
)

MAP_VALUES_3 <- list(
  "n_estimators" = list(
    actual = c("10", "255", "500"),
    mapped = c("-1", "0", "1")
  ),
  "min_samples_split" = list(
    actual = c("4", "12", "20"),
    mapped = c("-1", "0", "1")
  ),
  "max_features" = list(
    actual = c("sqrt", "log2"),
    mapped = c("-1", "1")
  ),
  "max_depth" = list(
    actual = c("10", "25", "40"),
    mapped = c("-1", "0", "1")
  ),
  "criterion" = list(
    actual = c("entropy", "gini"),
    mapped = c("-1", "1")
  ),
  "bootstrap" = list(
    actual = c("True", "False"),
    mapped = c("-1", "1")
  )
)


# Map actual values to -1, 0, 1
mapValues <- 
  function(df, VALUES_TO_MAP = MAP_VALUES_3) {
  
  if (is.na(VALUES_TO_MAP)) {
    stop("VALUES_TO_MAP list has to be specified")
  }

  for (COLNAME in names(VALUES_TO_MAP)) {
    df[, COLNAME] <- mapvalues(
      df[, COLNAME],
      from = get(COLNAME, VALUES_TO_MAP)$actual,
      to = get(COLNAME, VALUES_TO_MAP)$mapped
    )
  }

  return(df)
}
