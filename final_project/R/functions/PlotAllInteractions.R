# Load libraries
PACKAGES <- c('ggplot2', 'gridExtra')
lapply(PACKAGES, require, character.only = TRUE)
rm(PACKAGES)

# Load source files 
source('functions/FirstUp.R')

# Function to generate interaction plot
plotAllInteractions <- function(df, RESPONSE_NAME, OUT_PATH, PLOT_NAME) {
  
  # Generate all interaction plots separately
  VARS <- setdiff(colnames(data_noncent), c(RESPONSE_NAME))
  idx <- 1
  for (VAR_1 in VARS) {
    for (VAR_2 in VARS) {
      if (VAR_1 != VAR_2) {
        assign(
          paste0("inter_plot_", idx),
          ggplot(data = data_noncent) +
            aes_string(x = VAR_1, color = VAR_2, group = VAR_2, y = RESPONSE_NAME) +
            stat_summary(fun.y = mean, geom = "point") +
            stat_summary(fun.y = mean, geom = "line") +
            labs(
              # title = paste0(
              #   paste0("Interaction Plot for ", firstUp(RESPONSE_NAME), " with Respect to "),
              #   firstUp(VAR_1), " and ", firstUp(VAR_2)
              # ),
              color = firstUp(VAR_2)
            ) +
            xlab(firstUp(VAR_1)) +
            ylab(
              paste0(firstUp(RESPONSE_NAME), ", [mm]")
              )
        )
        idx <- idx + 1
      }
    }
  }
  
  # Put figures into the grid
  message("Creating the grid...")
  plot_full_1 <- grid.arrange(inter_plot_1, inter_plot_2, inter_plot_3, 
                              inter_plot_4, inter_plot_5, 
                              ncol = 2,
                              top = textGrob("Interaction Plots for Mass", gp = gpar(fontsize=15)))
  plot_full_2 <- grid.arrange(inter_plot_6, inter_plot_7, inter_plot_8, 
                              inter_plot_9, inter_plot_10, 
                              ncol = 2,
                              top = textGrob("Interaction Plots for Distance", gp = gpar(fontsize=15)))
  plot_full_3 <- grid.arrange(inter_plot_11, inter_plot_12, inter_plot_13,
                              inter_plot_14, inter_plot_15, 
                              ncol = 2,
                              top = textGrob("Interaction Plots for Filling", gp = gpar(fontsize=15)))
  plot_full_4 <- grid.arrange(inter_plot_16, inter_plot_17, inter_plot_18, 
                              inter_plot_19, inter_plot_20, 
                              ncol = 2,
                              top = textGrob("Interaction Plots for Hand", gp = gpar(fontsize=15)))
  plot_full_5 <- grid.arrange(inter_plot_21, inter_plot_22, inter_plot_23,
                              inter_plot_24, inter_plot_25,
                              ncol = 2,
                              top = textGrob("Interaction Plots for Vision", gp = gpar(fontsize=15)))
  plot_full_6 <- grid.arrange(inter_plot_26, inter_plot_27, inter_plot_28,
                              inter_plot_29, inter_plot_30, 
                              ncol = 2,
                              top = textGrob("Interaction Plots for Stance", gp = gpar(fontsize=15)))
  
  # Save the figure
  message("Saving figures...")
  FINAL_PATH <- sprintf(paste0(OUT_PATH, "inter_plot_%s.png"), VARS[seq(1:6)])
  ggsave(
    filename = FINAL_PATH[1],
    plot = plot_full_1,
    width = 170, height = 240, units = "mm"
  )
  ggsave(
    filename = FINAL_PATH[2],
    plot = plot_full_2,
    width = 170, height = 240, units = "mm"
  )
  ggsave(
    filename = FINAL_PATH[3],
    plot = plot_full_3,
    width = 170, height = 240, units = "mm"
  )
  ggsave(
    filename = FINAL_PATH[4],
    plot = plot_full_4,
    width = 170, height = 240, units = "mm"
  )
  ggsave(
    filename = FINAL_PATH[5],
    plot = plot_full_5,
    width = 170, height = 240, units = "mm"
  )
  ggsave(
    filename = FINAL_PATH[6],
    plot = plot_full_6,
    width = 170, height = 240, units = "mm"
  )
}