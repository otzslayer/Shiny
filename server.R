library(shiny)
library(readr)
library(caret)
library(ROCR)
library(plotROC)

wine <- data.frame(read_csv("hw/wine.csv"))         # Linear Regression
occu <- data.frame(read_csv("hw/occu.csv"))         # Logistic Regression
bike <- data.frame(read_csv("hw/bike.csv"))         # Regularized Regression
diabetes <- data.frame(read_csv("hw/diabetes.csv")) # k-Nearest Neighbors
poker <- data.frame(read_csv("hw/poker.csv"))       # Tree-based Methods
car <- data.frame(read_csv("hw/car.csv"))           # Support Vector Machine

### Measure
mae <- function(actual, predict){
    length <- length(actual)
    error <- abs(actual - predict)
    return(sum(error) / length)
}

mse <- function(actual, predict){
    length <- length(actual)
    error <- (abs(actual - predict))^2
    return(sum(error) / length)
}

rmse <- function(actual, predict){
    length <- length(actual)
    error <- (abs(actual - predict))^2
    rmse <- sqrt(sum(error) / length)
    return(rmse)
}

rmsle <- function(actual, predict){
    length <- length(actual)
    act <- log(actual + 1)
    pred <- log(predict + 1)
    error <- (abs(act - pred))^2
    rmsle <- sqrt(sum(error) / length)
    return(rmsle)
}

accuracy <- function(actual, predict){
    return(sum(actual == predict)/length(actual))
}

auc <- function(actual, predict){
    prob <- predict
    class <- actual
    fpr <- 1 - prob
    roc_df <- data.frame(fpr = fpr, class = class)
    knn_roc <- ggplot(roc_df, aes(m = fpr, d = class)) + geom_roc()
    AUC <- calc_auc(knn_roc)$AUC
    return(AUC)
}

MultiLogLoss <- function(act, pred){
    eps = 1e-15;
    nr <- length(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(length(act))      
    return(ll);
}

###

options(shiny.maxRequestSize = 30*1024^2)

function(input, output) {
    
    # Dataset을 선택하는 부분
    datasetInput <- reactive({
        switch(input$dataset,
               "------ Dataset -----" = NULL,
               "Wine Quality Prediction" = wine,
               "Occupancy Detection" = occu,
               "Bike Sharing Demand" = bike,
               "Diabetes Diagnosis" = diabetes,
               "Poker Hand Prediction" = poker,
               "Car Mileage Prediction" = car)
    })
    
    # Submission 프리뷰
    output$preview <- renderTable({
        inFile <- input$file1
        
        if(is.null(inFile))
            return(NULL)
        
        dataset <- data.frame(read_csv(inFile$datapath))
        summary <- as.matrix(head(dataset[, 2], 10))
        dimnames(summary) <- list(1:10, "hand")
        t(summary)
    })
    
    output$measureAnnotation <- reactive({
        switch(input$dataset,
               "------ Dataset -----" = NULL,
               "Wine Quality Prediction" = "Mean Absolute Error",
               "Occupancy Detection" = "Logarithmic Loss Function",
               "Bike Sharing Demand" = "Root Mean Squared Logarithmic Error",
               "Diabetes Diagnosis" = "Area Under Curve",
               "Poker Hand Prediction" = "Accuracy for Binary Classifer",
               "Car Mileage Prediction" = "Accuracy for Binary Classifer")
    })
    
    output$evaluation <- reactive({
        inFile <- input$file1
        
        if(is.null(inFile))
            return(NULL)
        
        dataset <- data.frame(read_csv(inFile$datapath))
        
        switch(input$dataset,
               "------ Dataset -----" = NULL,
               "Wine Quality Prediction" = mae(datasetInput()[, 2], dataset[, 2]),
               "Occupancy Detection" = MultiLogLoss(datasetInput()[, 2], dataset[, 2]),
               "Diabetes Diagnosis" = auc(datasetInput()[, 2], dataset[, 2]),
               "Bike Sharing Demand" = rmsle(datasetInput()[, 2], dataset[, 2]),
               "Poker Hand Prediction" = accuracy(datasetInput()[, 2], dataset[, 2]),
               "Car Mileage Prediction" = accuracy(datasetInput()[, 2], dataset[, 2]))
    })
}
