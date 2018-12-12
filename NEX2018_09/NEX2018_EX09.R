# Clear the environment
rm(list = ls())
dev.off()

# Load libraries 
packages <- c('qualityTools', 'lattice', 'rsm')
lapply(packages, require, character.only = TRUE)
rm(packages)

# Set up the work directory
setwd(paste0(getwd(), '/NEX2018_09'))


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
###############        ANALYZING THE MAXIMUM YIELD         #####################
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
      axes=F, 
      xlab="X2^2", ylab="X3^2")
contour(x, y, z, 
        levels = seq(60, 120, by = 5), 
        add = T, col = "peru")
axis(1, at = seq(min(x), max(x), by = 1))
axis(2, at = seq(min(y), max(y), by = 1))
box()

## Perspective plot and contour plot indicate that the maximum yield 
## is obtained at the zero level for both variables X2 and X3.
## There was no information regarding the variable X1.
## Hence, we will compute the stationary point explicitly.

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
