library(shiny)
library(ggplot2)
library(RColorBrewer)
data1<-readRDS(file="data1.rds")
data1
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
       
    )
  )
   
)
#### server

server <- function(input, output) {
  # 
 
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(dataInput()$x)
   
  })

  

}


# Run the application 
shinyApp(ui = ui, server = server)