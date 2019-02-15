# Function ---
varImp_ggplot <- function(varImpPlot_matrix, importance_measure = c("%IncMSE", "IncNodePurity")) {
  
  importance_measure <- match.arg(importance_measure) # read measure of interest
  colnames(varImpPlot_matrix) <- gsub("%", "", colnames(varImpPlot_matrix))
  importance_measure <- gsub("%", "", importance_measure) # remove possible "%"
  
  
  varImpPlot_matrix <- varImpPlot_matrix %>%
    as.data.frame() %>%
    mutate(var_names = rownames(varImpPlot_matrix)) # create data.frame
  rownames(varImpPlot_matrix) <- NULL # remove row names
  
  varImpPlot_matrix
  
  ggplot(varImpPlot_matrix, aes(x = reorder(var_names, varImpPlot_matrix[,importance_measure]), 
                                  y = importance_measure), environment = environment()) +
    geom_point(size = 1, color = "royalblue") +
    geom_segment(aes(x = var_names, xend = var_names, y = 0, yend = varImpPlot_matrix[,importance_measure]),
                 size = 2, color = "royalblue") +
    ylab(importance_measure) + xlab("Variable Name") +
    ggtitle(importance_measure) +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
}

# call example ---
set.seed(4543)
data(mtcars)

library(randomForest)
library(dplyr)
library(ggplot2)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
imp <- importance(mtcars.rf) # let's save the importance object
varImp_ggplot(imp, importance_measure = "IncNodePurity")
