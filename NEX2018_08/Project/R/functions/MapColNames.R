# Load packages
PACKAGES <- c('plyr')
lapply(PACKAGES, require, character.only = TRUE)
rm(PACKAGES)

# Map -1, 0, 1 to actual values
mapColNames <- function(df, COLNAMES) {
  
  for (COLNAME in COLNAMES) {
    
    df[, COLNAME] <- switch(COLNAME,
      mass = mapvalues(
        df[, COLNAME],
        from = c("-1", "0", "1"),
        to = c(50, 75, 100)
      ),
      distance = mapvalues(
        df[, COLNAME],
        from = c("-1", "0", "1"),
        to = c(3, 4, 5)
      ),
      filling = mapvalues(df[, COLNAME],
        from = c("-1", "1"),
        to = c("Hulled Grain", "Flour")
      ),
      hand = mapvalues(
        df[, COLNAME],
        from = c("-1", "1"),
        to = c("Dominant", "Non-Dominant")
      ),
      vision = mapvalues(
        df[, COLNAME],
        from = c("-1", "1"),
        to = c("Opened Eyes", "Closed Eyes")
      ),
      stance = mapvalues(
        df[, COLNAME],
        from = c("-1", "1"),
        to = c("Two Legs", "One Leg")
      )
    )
    
  }

  return(df)
}
