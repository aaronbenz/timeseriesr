#Aaron Benz
#11-07-2014
#show examples of point reduce at work
library(shiny)
library(data.table)
library(ggplot2)
library(timeseriesr)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Reduce Points Demo"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "tol",
                        "Tolerance:",
                        min = 0.001,
                        max = .5,
                        value = .01,
                        step = .001),
            sliderInput(inputId = "size","Size",
                         value=100000,
                         min = 0,
                         max = 10000000,
                         step = 100000),
            textOutput(outputId = "reduced_points")),
      
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("ggplot_reduced")
        )
    )
))