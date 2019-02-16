# Load packages
PACKAGES <- c("ggplot2", "gridExtra")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[,"Package"])]
if(length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))

# Load sources
FUN_LIST <- lapply(
  c('CreateSingleBoxPlot.R'), 
  function(x) paste0(PATH_GENERAL, PATH_FUNCTIONS, x)
  )
lapply(FUN_LIST, source)
rm(FUN_LIST)


# The function to create multiple box plots (6, as we are
# working with a 2^6 design) with discrete x axis
createMultiBoxPlot <- function(df, DEP_VAR, OUT_PATH, PLOT_NAME,
                               PRINT_PLOT = FALSE) {
  
  cols_x <- setdiff(colnames(df), DEP_VAR)

  # Firstly, create single box plot for each variable
  message("Creating box plots for each of the variables...")

  boxplot_1 <- createSingleBoxPlot(
    df,
    x = which(names(df) == cols_x[1]),
    y = which(names(df) == DEP_VAR),
    XLAB = cols_x[1],
    YLAB = DEP_VAR,
    PLOT_TITLE = paste(cols_x[1],"Box Plot"),
    PRINT_PLOT = FALSE
  )
  
  boxplot_2 <- createSingleBoxPlot(
    df,
    x = which(names(df) == cols_x[2]),
    y = which(names(df) == DEP_VAR),
    XLAB = cols_x[2],
    YLAB = DEP_VAR,
    PLOT_TITLE = paste(cols_x[2],"Box Plot"),
    PRINT_PLOT = FALSE
  )
  
  boxplot_3 <- createSingleBoxPlot(
    df,
    x = which(names(df) == cols_x[3]),
    y = which(names(df) == DEP_VAR),
    XLAB = cols_x[3],
    YLAB = DEP_VAR,
    PLOT_TITLE = paste(cols_x[3],"Box Plot"),
    PRINT_PLOT = FALSE
  )
  
  boxplot_4 <- createSingleBoxPlot(
    df,
    x = which(names(df) == cols_x[4]),
    y = which(names(df) == DEP_VAR),
    XLAB = cols_x[4],
    YLAB = DEP_VAR,
    PLOT_TITLE = paste(cols_x[4],"Box Plot"),
    PRINT_PLOT = FALSE
  )
  
  boxplot_5 <- createSingleBoxPlot(
    df,
    x = which(names(df) == cols_x[5]),
    y = which(names(df) == DEP_VAR),
    XLAB = cols_x[5],
    YLAB = DEP_VAR,
    PLOT_TITLE = paste(cols_x[5],"Box Plot"),
    PRINT_PLOT = FALSE
  )
  
  boxplot_6 <- createSingleBoxPlot(
    df,
    x = which(names(df) == cols_x[6]),
    y = which(names(df) == DEP_VAR),
    XLAB = cols_x[6],
    YLAB = DEP_VAR,
    PLOT_TITLE = paste(cols_x[6],"Box Plot"),
    PRINT_PLOT = FALSE
  )

  # Put figures into the grid
  message("Creating the grid...")
  plot_full <- grid.arrange(
    boxplot_1, boxplot_2, boxplot_3,
    boxplot_4, boxplot_5, boxplot_6,
    ncol = 2
  )

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
