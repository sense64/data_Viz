## Only run examples in interactive R sessions
library(shiny)

# Define UI for application that draws a boxplot
ui <- fluidPage(
  
  includeCSS("myStyle.css"),
  # Application Title
  headerPanel("Miles per Gallon!!!"),
  # Sidebar with a slider input a variable
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", h3("Choice a Variable"),
                  list("Cylinders"= "cyl", "Transmission"="am", "Gear"="gear")),
      checkboxInput("outliers", "Show outliers", FALSE),
    ),
    # Show a plot 
    mainPanel(
      h2(textOutput("caption")),
      plotOutput("mpgPlot")
    )
  )

)
#### server
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels=c("Automatic", "Manual"))

server <- function(input, output) {
  formalsText <- reactive({  #reative 함수를 사용하여 모듈화를 시킴
        paste("mpg~", input$variable)
  })
  output$caption <- renderText(formalsText())
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formalsText()), data=mpgData, outline=input$outliers)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)