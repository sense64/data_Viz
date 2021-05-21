library(shiny)
library(ggplot2)
library(RColorBrewer)
library(GISTools)
library(rgdal)
library(shiny)
setwd("D:/R_workspace")
load(file="map/popData1.rda")
popData<-subset(popData,id!=9)

sidoshp = readOGR("map/sido/TL_SCCO_CTPRVN.shp")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("대한민국 인구데이터"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "dataset",
                        label = "Choose a attribute:",
                        choices = c("인구증가", "2007년LQ", "2018년LQ")),
            hr(),
            
            tags$img(src="donga.jpg", width="200px", height="200px")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    datasetInput <- reactive({
        switch(input$dataset,
               "인구증가" = popData$diff,
               "2007년LQ" = popData$fLQ07,
               "2018년LQ" = popData$fLQ18)
    })
    output$distPlot <- renderPlot({
        data<-datasetInput()
        # generate bins based on input$bins from ui.R
        shades=auto.shading(data, cols=brewer.pal(6, "YlGnBu"))
        # (quantile(popData$diff, prob=c(0.1, 0.25, 0.5, 0.75, 0.9)))
        # shades=shading(breaks=c(-88, -33, 9, 59, 146), cols=brewer.pal(6, "YlGnBu"))
        #2. 주어진 지도파일과 특성 데이터의 변수 인구데이터로 단계구분도를 그린다
        choropleth(sidoshp, data, shading=shades)
        #3. 범례를 넣는다.
        choro.legend("bottomright", 0, shades, cex=0.7)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
