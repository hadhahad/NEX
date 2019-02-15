# Load packages
PACKAGES <- c("ggplot2")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[,"Package"])]
if(length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))


# The function to create a single box plot with discrete x axis
createSingleBoxPlot <- function(df, x, y, 
                                XLAB, YLAB, PLOT_TITLE,
                                PRINT_PLOT = FALSE) {
  
  # Default colors for filling and lines
  FILL    <- "#4271AE"
  LINE    <- "#1F3552"
  OUTLIER <- "#1F3552"
  
  # Create the box plot
  p <- ggplot(df, aes(x = df[, x], y = df[, y])) + 
    geom_boxplot(fill = FILL, colour = LINE, alpha = 0.7,
                 outlier.colour = OUTLIER, outlier.shape = 20) +
    scale_y_continuous(name = YLAB) +
    scale_x_discrete(name = XLAB) +
    ggtitle(PLOT_TITLE)
  
  # Draw the plot
  if (PRINT_PLOT) {
    print(p)
  }
  
  return(p)
}
