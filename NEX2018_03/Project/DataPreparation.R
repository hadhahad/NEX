# Loading packages
packages <- c("tidyverse", "data.table")
lapply(packages, require, character.only = TRUE)
rm(packages)


# Function to load experiment data and format it for further usage
loadExperimentData <- function(path, rename_blocks = TRUE, shuffle_rows = FALSE) {
  
  # Load the data from the specified path
  message("Loading experiment data...")
  data <- read_csv(path, col_names = TRUE)
  names(data) <- c("BLOCK", "HITS_L", "HITS_R", "HITS_SUM", "DIAMETER", "HAND")
  
  # Assign integer labels to blocks, if needed
  if (rename_blocks) {
    message("Setting integer labels to blocks...")
    data <- data %>%
      mutate(BLOCK = case_when(
        BLOCK == "V" ~ 1,
        BLOCK == "M" ~ 2,
        BLOCK == "S" ~ 3,
        BLOCK == "A" ~ 4
        ))
  }
  
  # Shuffle rows of the dataframe, if needed
  if (shuffle_rows) {
    message("Performing rows shuffle...")
    data <- data[sample(nrow(data)), ]
  }
  
  # Specify factor variables
  message("Assigning factor variables...")
  data$BLOCK <- as.factor(data$BLOCK)
  data$DIAMETER <- as.factor(data$DIAMETER)
  data$HAND <- as.factor(data$HAND)
  
  # Select required columns
  data <- data %>%
    select(-c(HITS_L, HITS_R))
  
  # Return the dataframe
  message("The dataset is ready for use!")
  return(data)
}
