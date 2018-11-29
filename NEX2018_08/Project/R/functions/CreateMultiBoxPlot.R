# Load packages
PACKAGES <- c('ggplot2', 'gridExtra')
lapply(PACKAGES, require, character.only = TRUE)
rm(PACKAGES)

# Load sources
source('functions/MapColNames.R')
source('functions/CreateSingleBoxPlot.R')

# The function to create multiple box plots with discrete x axis
createMultiBoxPlot <- function(df, OUT_PATH, PLOT_NAME,
                               PRINT_PLOT = FALSE) {

  # Firstly, create single box plot for each variable
  message("Creating box plots for each of the variables...")

  mass_boxplot <- createSingleBoxPlot(
    df = mapColNames(
      df = df,
      COLNAMES = c("mass")
    ),
    x = which(names(df) == "mass"),
    y = which(names(df) == "measurement"),
    XLAB = "Mass, [g]",
    YLAB = "Measurement, [mm]",
    PLOT_TITLE = "Mass Box Plot",
    PRINT_PLOT = FALSE
  )

  distance_boxplot <- createSingleBoxPlot(
    df = mapColNames(
      df = df,
      COLNAMES = c("distance")
    ),
    x = which(names(df) == "distance"),
    y = which(names(df) == "measurement"),
    XLAB = "Distance, [m]",
    YLAB = "Measurement, [mm]",
    PLOT_TITLE = "Distance Box Plot",
    PRINT_PLOT = FALSE
  )

  filling_boxplot <- createSingleBoxPlot(
    df = mapColNames(
      df = df,
      COLNAMES = c("filling")
    ),
    x = which(names(df) == "filling"),
    y = which(names(df) == "measurement"),
    XLAB = "Filling",
    YLAB = "Measurement, [mm]",
    PLOT_TITLE = "Filling Box Plot",
    PRINT_PLOT = FALSE
  )

  hand_boxplot <- createSingleBoxPlot(
    df = mapColNames(
      df = df,
      COLNAMES = c("hand")
    ),
    x = which(names(df) == "hand"),
    y = which(names(df) == "measurement"),
    XLAB = "Hand",
    YLAB = "Measurement, [mm]",
    PLOT_TITLE = "Hand Box Plot",
    PRINT_PLOT = FALSE
  )

  vision_boxplot <- createSingleBoxPlot(
    df = mapColNames(
      df = df,
      COLNAMES = c("vision")
    ),
    x = which(names(df) == "vision"),
    y = which(names(df) == "measurement"),
    XLAB = "Vision",
    YLAB = "Measurement, [mm]",
    PLOT_TITLE = "Vision Box Plot",
    PRINT_PLOT = FALSE
  )

  stance_boxplot <- createSingleBoxPlot(
    df = mapColNames(
      df = df,
      COLNAMES = c("stance")
    ),
    x = which(names(df) == "stance"),
    y = which(names(df) == "measurement"),
    XLAB = "Stance",
    YLAB = "Measurement, [mm]",
    PLOT_TITLE = "Stance Box Plot",
    PRINT_PLOT = FALSE
  )

  # Put figures into the grid
  message("Creating the grid...")
  plot_full <- grid.arrange(mass_boxplot, distance_boxplot,
    filling_boxplot, hand_boxplot,
    vision_boxplot, stance_boxplot,
    ncol = 2
  )

  # Draw the plot
  if (PRINT_PLOT) {
    print(plot_full)
  }

  # Save the figure
  message("Saving the plot...")
  FINAL_PATH <- paste0(OUT_PATH, PLOT_NAME, ".png")
  ggsave(
    filename = FINAL_PATH,
    plot = plot_full,
    width = 170, height = 240, units = "mm"
  )
  message(paste0("The plot was successfully saved at:\n", 
                 getwd(), '/', FINAL_PATH))

  return(plot_full)
}
