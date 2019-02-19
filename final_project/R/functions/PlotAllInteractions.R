# Load packages
PACKAGES <- c("ggplot2", "gridExtra", "grid")
NEW_PACKAGES <- PACKAGES[!(PACKAGES %in% installed.packages()[, "Package"])]
if (length(NEW_PACKAGES)) install.packages(NEW_PACKAGES)
lapply(PACKAGES, require, character.only = TRUE)
rm(list = c("PACKAGES", "NEW_PACKAGES"))


# Function to generate interaction plot
plotAllInteractions <- function(df, RESPONSE_NAME, OUT_PATH) {

  # Generate all interaction plots separately
  VARS <- setdiff(colnames(df), c(RESPONSE_NAME))
  idx <- 1
  for (VAR_1 in VARS) {
    for (VAR_2 in VARS) {
      if (VAR_1 != VAR_2) {
        assign(
          paste0("inter_plot_", idx),
          ggplot(data = df) +
            aes_string(
              x = VAR_1, color = VAR_2, group = VAR_2,
              y = RESPONSE_NAME
            ) +
            stat_summary(fun.y = mean, geom = "point") +
            stat_summary(fun.y = mean, geom = "line") +
            labs(
              color = VAR_2
            ) +
            xlab(VAR_1) +
            ylab(RESPONSE_NAME) +
            scale_color_brewer(palette="Set1")
        )
        idx <- idx + 1
      }
    }
  }

  # Put figures into the grid
  message("Creating the grid and saving figures...")
  FINAL_PATH <- sprintf(
    paste0(OUT_PATH, "interaction_plot_%s.png"), VARS[seq(1:length(VARS))]
  )

  for (i in 1:length(VARS)) {

    # Create the plot
    assign(
      paste0("plot_full_", i),
      do.call(grid.arrange, list(
        grobs = mget(
          sprintf(
            "inter_plot_%s",
            seq((i - 1) * (length(VARS) - 1) + 1, i * (length(VARS) - 1))
          )
        ),
        ncol = 2,
        top = textGrob(paste("Interaction Plots for", VARS[i]),
          gp = gpar(fontsize = 15)
        )
      ))
    )

    # Save the plot
    Sys.sleep(1)
    ggsave(
      filename = FINAL_PATH[i],
      plot = get(sprintf("plot_full_%s", i)),
      width = 170, height = 240, units = "mm"
    )
    message(paste0("Successfully saved ", getwd(), FINAL_PATH[i]))
  }
}
