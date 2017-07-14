library(shiny)
library(rpart)
library(rattle)
library(dplyr)


fit.titanic <- rpart(survived ~ ., data = Titanicp)

ui <- fluidPage(
  titlePanel("Titanic Prediction"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("sex", label = "Gender",
                   choices = list("Male" = "male", "Female" = "female"),
                   selected = "male"),
      sliderInput("age",
                  "Age:",
                  min = 0.0,
                  max = 100.0,
                  step = 1.00,
                  value = 20.00),
      radioButtons("pclass", label = ("Pclass"),
                   choices = list("First" = "1st", "Second" = "2nd", "Third" = "3rd"),
                   selected = "3rd"),
      sliderInput("sibsp",
                  "Number of Siblings:",
                  min = 0.0,
                  max = 10.0,
                  step = 0.50,
                  value = 2.50),
      sliderInput("parch",
                  "Number of Parents:",
                  min = 0.0,
                  max = 5.0,
                  step = 1.00,
                  value = 2.00),
      sliderInput("sample",
                  "Sample proportion:",
                  min = 0.0,
                  max = 100.0,
                  step = 10.00,
                  value = 50.00)
    ),
    mainPanel(
      textOutput("prediction"),
      plotOutput("tree")
    )
  )
)

server <- function(input, output) {
  generate_fit <- reactive({
    rpart(survived ~ ., data = sample_frac(Titanicp, input$sample, replace = TRUE))
  })
  
  output$prediction <- renderText({
    new_data = data.frame(
      sex = input$sex,
      pclass = input$pclass,
      age = input$age,
      sibsp = input$sibsp,
      parch = input$parch
    )
    predicted <- predict(generate_fit(), new_data, type = "class")
    paste(sprintf("Prediction is that the %s ", input$sex), predicted, ".", sep = " ")

  })
  output$tree <- renderPlot({
    fancyRpartPlot(generate_fit())

  })
}

shinyApp(ui = ui, server = server)

