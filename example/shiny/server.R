#Aaron Benz
#11-07-2014
#show examples of point reduce at work

library(shiny)
library(data.table)
library(ggplot2)
library(timeseriesr)
print("check")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  dt <- reactive({
    dt <- data.table("index" = 1:input$size, "sin" = sin(seq(0,20,10/input$size*2))[-1])
    dt[reducePoints(dt$sin,input$tol)$index]
  })

  output$reduced_points <- renderText(paste(nrow(dt())))
  
  output$ggplot_reduced <- renderPlot({
    g <- ggplot(dt(), aes(index, sin)) + geom_step()+
    labs(title = paste("Number of Points: ", input$size, "\n Tolerance %: ", input$tol))
    print(g)
  })
  
})