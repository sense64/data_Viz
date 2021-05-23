## Only run examples in interactive R sessions
library(shiny)

# Define UI for application that draws a boxplot
ui <- fluidPage(
  
  # Sidebar with a slider input a variable
  sidebarLayout(
    sidebarPanel(),
    # Show a plot 
    mainPanel(

    )
  )

)
#### server


server <- function(input, output) {
  
}


# Run the application 
shinyApp(ui = ui, server = server)