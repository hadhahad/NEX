# Clear the environment
rm(list = ls())
dev.off()

# Load and install libraries 
packages <- c('rsm')
install_idx <- which(lapply(packages, require, character.only = TRUE) == FALSE)
lapply(packages[install_idx], install.packages, character.only = TRUE)
rm(list = c('packages', 'install_idx'))

# Set up the work directory
# setwd(paste0(getwd(), '/NEX2018_09'))


################################################################################
############################        LOAD DATA        ###########################
################################################################################

data        <- read.csv(file = 'Exercise_11_8.csv', sep = ';')
names(data) <- c('X1', 'X2', 'X3', 'YIELD')


################################################################################
#############        BUILDING THE SECOND ORDER MODEL         ###################
################################################################################

# Building the first model
model_1 <- lm(YIELD ~ X1 + X2 + X3 + 
                I(X1^2) + I(X2^2) + I(X3^2) +
                X1:X2 + X2:X3 + X1:X3, data = data)

# Summary of the model
summary(model_1)

# Summary of the relecant ANOVA
summary(aov(model_1))

## It seems that only X2^2 and X3^2 are significant.
## We will restrict the model to only these variables.

# Building the second model
model_2 <- lm(YIELD ~ I(X2^2) + I(X3^2), data = data)

# Summary of the model
summary(model_2)

# Summary of the relecant ANOVA
summary(aov(model_2))


################################################################################
#######        THE SECOND ORDER MODEL AND THE MAXIMUM YIELD         ############
################################################################################

# Define the function to predict values on the grid
vals_xy <- function(x, y, model = model_2) {
  
  new_data <- data.frame(X2 = x, X3 = y)
  return(predict(model, new_data))
  
}

# Set up the grid
x <- seq(0, 3, 0.1)
y <- seq(0, 3, 0.1)
z <- outer(X = x, Y = y, FUN = vals_xy)

# Perspective plot
persp(x, y, z,
      theta=30, phi=30, expand=0.5, col="lightblue", 
      xlab = "X2^2", ylab = "X3^2", zlab = "YIELD")

# Contour plot
image(x, y, z, 
      axes=FALSE, 
      xlab="X2^2", ylab="X3^2")
contour(x, y, z, 
        levels = seq(60, 120, by = 5), 
        add = TRUE, col = "peru")
axis(1, at = seq(min(x), max(x), by = 1))
axis(2, at = seq(min(y), max(y), by = 1))
box()

## Perspective plot and contour plot indicate that the maximum yield 
## is obtained at the zero level for both variables X2 and X3.
## There was no information regarding the variable X1.
## Hence, we will compute the stationary point explicitly.


################################################################################
###############        ANALYZING THE MAXIMUM YIELD         #####################
################################################################################

# Coefficients of the full second order model
model_1$coeff

# Define the system
b <- matrix(c(model_1$coeff[2], model_1$coeff[3], model_1$coeff[4]), 3, 1)
B <- matrix(c(model_1$coeff[5],    model_1$coeff[8]/2, model_1$coeff[10]/2,
              model_1$coeff[8]/2,  model_1$coeff[6],   model_1$coeff[9]/2,
              model_1$coeff[10]/2, model_1$coeff[9]/2, model_1$coeff[7]),
            3, 3) 

# Then the stationary point is the following value
x_stat  <- -1/2 * solve(B) %*% b
print(x_stat)

## We can also attempt to display it, however, the quality of the
## contour plot may not be acceptable, as almost all variables and their 
## interactions are not significant in the full model. 

# Define the grid for the perspective plot and the contour plot
x1.new <- seq(-1.75, 1.75, 0.1)
x2.new <- seq(-1.75, 1.75, 0.1)
x3.new <- seq(-1.75, 1.75, 0.1)

# Define functions to calculate predictions on 2D grids
vals_new <- function(x, y, z = x3.new, model = model_1) {
  new_data <- data.frame(X1 = x, X2 = y, X3 = z)
  return(predict(model, new_data))
}

# Compute predicted yield
z.new = outer(x1.new, x2.new, vals_new)

# Contour plot
image(x1.new, x2.new, z.new, axes = FALSE)
contour(x1.new, x2.new, z.new, 
        levels = seq(60, max(120), by = 5), 
        add = TRUE, col = "peru")
points(x_stat[1], x_stat[2], pch = 19)
text(x_stat[1] + 0.05, x_stat[2] + 0.05, 
     "Stationary point", pos = 3.75, cex = 0.6)
axis(1, at = seq(min(x1.new), max(x1.new), by = 0.5))
axis(2, at = seq(min(x2.new), max(x2.new), by = 0.5))
box()
