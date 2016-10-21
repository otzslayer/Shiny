library(shiny)
library(readr)
library(caret)

poker <- data.frame(read_csv("poker/refer.csv"))
wine <- data.frame(read_csv("wine/refer.csv"))

### Measure
mae <- function(actual, predict){
    length <- length(actual)
    error <- abs(actual - predict)
    return(sum(error) / length)
}

accuracy <- function(actual, predict){
    return(sum(actual == predict)/length(actual))
}

options(shiny.maxRequestSize = 30*1024^2)

function(input, output) {
    
    # Dataset을 선택하는 부분
    datasetInput <- reactive({
        switch(input$dataset,
               "Poker Hand Prediction" = poker,
               "Wine Quality Prediction" = wine)
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
               "Poker Hand Prediction" = "Accuracy for Binary Classifer",
               "Wine Quality Prediction" = "Mean Absolute Error")
    })
  
    output$evaluation <- reactive({
        inFile <- input$file1
        
        if(is.null(inFile))
            return(NULL)
        
        dataset <- data.frame(read_csv(inFile$datapath))
        
        switch(input$dataset,
               "Poker Hand Prediction" = accuracy(datasetInput()[, 2], dataset[, 2]),
               "Wine Quality Prediction" = mae(datasetInput()[, 2], dataset[, 2]))
    })
}
