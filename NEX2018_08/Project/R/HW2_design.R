library(FrF2)

# Creating designs for measurements

k <- 6

design_1 <- FrF2(2^(k-2), k, replications = 1, 
               randomize = TRUE, generators = c("ABC","BCD") ,
               factor.names = c("A", "B", "C", "D", "E", "F"))

design_2 <- FrF2(2^(k-2), k, replications = 1, 
                 randomize = TRUE, generators = c("-ABC","BCD") ,
                 factor.names = c("A", "B", "C", "D", "E", "F"))

set.seed(1234)
design_3 <- FrF2(2^(k-2), k, replications = 1, 
                 randomize = TRUE, generators = c("ABC","-BCD") ,
                 factor.names = c("A", "B", "C", "D", "E", "F"))

set.seed(1234)
design_4 <- FrF2(2^(k-2), k, replications = 1, 
                 randomize = TRUE, generators = c("-ABC","-BCD") ,
                 factor.names = c("A", "B", "C", "D", "E", "F"))

# Designs for center points

k <- 4

set.seed(1234)
cent_design_1 <- FrF2(2^(k-1), k, replications = 1, 
                      randomize = TRUE, generators = c("ABC") ,
                      factor.names = c("A", "B", "C", "D"))

set.seed(1234)
cent_design_2 <- FrF2(2^(k-1), k, replications = 1, 
                      randomize = TRUE, generators = c("-ABC") ,
                      factor.names = c("A", "B", "C", "D"))


# An equivalent using blocks instead of generators
# k <- 6
# design <- FrF2(2^(k), k, replications = 1, blocks = 4, 
#                randomize = FALSE,
#                factor.names = c("A", "B", "C", "D", "E", "F"))

# Save the design in the csv format
write.csv(cent_design_2, file="NEX2018_08/Project/Data/cent_design2.csv")
