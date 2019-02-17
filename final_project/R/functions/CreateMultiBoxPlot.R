# Load packages
PACKAGES <- c("ggplot2", "gridExtra")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[, "Package"])]
if (length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))

# Load sources
FUN_LIST <- lapply(
  c("CreateSingleBoxPlot.R"),
  function(x) paste0(PATH_GENERAL, PATH_FUNCTIONS, x)
)
lapply(FUN_LIST, source)
rm(FUN_LIST)


# The function to create multiple nested box plots
createMultiBoxPlot <- function(df, DEP_VAR, OUT_PATH, PLOT_NAME) {
  
  VARS <- setdiff(colnames(df), DEP_VAR)

  # Firstly, create single box plot for each variable
  message("Creating box plots for each of the variables...")

  for (idx in 1:length(VARS)) {
    
    boxplot <- createSingleBoxPlot(
      df,
      x = VARS[idx],
      y = DEP_VAR,
      PLOT_TITLE = paste(VARS[idx], "Box Plot")
    )
    
    assign(
      paste0("boxplot_", idx), 
      boxplot
    )
    
  }

  # Put figures into the grid
  message("Creating the grid...")
  plot_full <- do.call(
    grid.arrange,
    list(
      grobs = mget(sprintf("boxplot_%s", 1:length(VARS))),
      ncol = 2
    )
  )

  # Save the figure
  message("Saving the plot...")
  FINAL_PATH <- paste0(OUT_PATH, PLOT_NAME, ".png")
  ggsave(
    filename = FINAL_PATH,
    plot = plot_full,
    width = 170, height = 240, units = "mm"
  )
  message(paste0(
    "The plot was successfully saved at:\n",
    getwd(), "/", FINAL_PATH
  ))

  return(plot_full)
}
