library(raster)
library(rgdal)

landsat<- stack("Landsat7.tif")
landsat
plotRGB(landsat, r = 1, g = 2, b = 3, alpha=80,stretch="hist")

library(shiny)




ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Transparency",
              value = 100, min = 1, max = 255),
  sliderInput(inputId = "red",
              label = "Red",
              value = 1, min = 1, max = 3),
  sliderInput(inputId = "green",
              label = "Green",
              value = 2, min = 1, max = 3),
  sliderInput(inputId = "blue",
              label = "Blue",
              value = 3, min = 1, max = 3),
  selectInput('stretch', 'Stretch', c("hist","lin")),
  plotOutput("plot1")
)

server <- function(input, output) {

  output$plot1 <- renderPlot({landsat <- stack("Landsat7.tif")
    plotRGB(landsat, r = input$red, g = input$green, b = input$blue,alpha=input$num,stretch=input$stretch)
  })
}

shinyApp(ui = ui, server = server)
















