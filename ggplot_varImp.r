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
                 size = 1, color = "royalblue") +
    ylab(importance_measure) + xlab("Variable Name") +
    coord_flip() +
    theme_minimal()
  
}

# call example ---
set.seed(4543)
data(mtcars)

library(randomForest)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
imp <- varImpPlot(mtcars.rf) # let's save the varImp object
varImp_ggplot(imp, importance_measure = "IncNodePurity")
