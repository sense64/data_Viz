library(shiny)
library(ggplot2)
library(RColorBrewer)
data1<-readRDS(file="data1.rds")
#data1
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  headerPanel("Lotte DataViz"), 
  
  
  # Sidebar with a slider input a variable
  sidebarLayout(
    sidebarPanel(
      radioButtons("spot", "Branch office of Lotte :",
                   c("AA" = "AA",
                     "BB" = "BB",
                     "CC" = "CC",
                     "DD" = "DD")),
      hr(),
      
      tags$img(src="donga.jpg", width="200px", height="200px")),
      
    # Show a plot 
    mainPanel(
       tabsetPanel(type="tabs",
                   tabPanel("Plot", plotOutput("plot")),
                   tabPanel("Summary", verbatimTextOutput("summary")),
                   tabPanel("DataTable", DT::dataTableOutput("table"))
    )
    ) 
  ) 
)
#### server

server <- function(input, output) {
  # reactive function 
  dataInput <- reactive({
    temp <- switch(input$spot,
                   "AA"="AA",
                   "BB"="BB",
                   "CC"="CC",
                   "DD"="DD")
    subset(data1, catStore==temp)
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(dataInput()$x)
   
  })

  output$plot <- renderPlot({
    ggplot(dataInput(), aes(x=catDate, y=catProduct)) + 
      geom_tile(aes(fill=x)) +
      scale_fill_gradientn(colors =brewer.pal(n=5, name="RdBu")) +
      ggtitle("월별 거래처에 대한 매출액") + 
      xlab("거래월") +
      ylab("품목") 
  })
  
  output$table <- DT::renderDataTable({
    #dataset <- subset(data1, catStore==as.character(dataInput()))
    temp<- tapply(dataInput()$x, list(dataInput()$catDate), sum)
    DT::datatable(as.data.frame(temp))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)