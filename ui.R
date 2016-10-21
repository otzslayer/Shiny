library(shiny)

# fluidPage(
#     titlePanel("Poker Hand Prediction"),
#     sidebarLayout(
#         sidebarPanel(
#             fileInput('file1', 'Submit!',
#                       accept = c('text/csv', 
#                                'text/comma-separated-values,text/plain', 
#                                '.csv')),
#             tags$hr()
#         ),
#         mainPanel(
#             tableOutput('contents')
#         )
#     )
# )

shinyUI(fluidPage(
    titlePanel("Evaluation Page for An Introduction to Machine Learning with R"),
    
    sidebarLayout(
        sidebarPanel(
            h2("Choose a Dataset"),
            
            selectInput("dataset", "Choose a dataset:", 
                        choices = c("Poker Hand Prediction", "Wine Quality Prediction")),
            
            helpText("모델 구축에 사용한 데이터를 선택해주세요."),
            br(),
            br(),
            fileInput('file1', 'Submission',
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')),
            tags$hr()
            ),
    mainPanel(
        h1("Prediction Values"),
        # Vector 출력
        tableOutput("preview"),
        p("위 결과는 제출한 결과의 첫 열 개의 값입니다."),
        br(),
        h2("Evaluation Result"),
        p("해당 모델의 평가는 다음으로 측정됩니다."),
        # Measure 종류
        verbatimTextOutput("measureAnnotation"),
        # 결과값
        verbatimTextOutput("evaluation")
    )
)))