# R_Shiny

library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Shiny Plots"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    
    selectInput("plotType", "Plot Type",  c(Scatter = "scatter", Histogram = "hist") ),
    
    fileInput("userData", "Choose File:", multiple = FALSE, accept = ".csv"),
    
    selectInput("FilterColumn", "Filter By Column name", choices = NULL),
    
    textInput("FilterValue", "Filter Value:", value=""),
    
    radioButtons("FilterValueType", "Data type for Filter Value:",
                 c("Numeric" = "Numeric",
                   "String" = "String"), selected = "Numeric"),
    
    sliderInput("PlotWidth", "Plot Width: ", min = 500, max=1200, value = 600 ), 
    
    sliderInput("PlotHeight", "Plot Height: ", min = 500, max=1200, value = 600 ) ,
  
    sliderInput("text_size", "Text Size:",min = 10, max = 20, value = 15),
    
    
    conditionalPanel(
      condition = "input.plotType == 'scatter'",
      
      selectInput("dataCol_x", "X-axis:", choices = NULL),
      radioButtons("xtype", "Data type for x-axis:",
                 c("Numeric" = "Numeric",
                   "Category" = "Category"), selected = "Category"),
    
      selectInput("dataCol_y", "y-axis:", choices = NULL),
      radioButtons("ytype", "Data type for y-axis:",
                 c("Numeric" = "Numeric",
                   "Category" = "Category"), selected = "Numeric"),
    
      selectInput("ColorBy", "Color By:", choices = NULL),
    
      selectInput("ShapeBy", "Shape By:", choices= NULL),
    
      textInput("FacetBy", "Facet By:", value = "") ),
    
    
    conditionalPanel(
    condition = "input.plotType == 'hist'",
    
    selectInput("hist_Col", "hist-axis:", choices = NULL),
    sliderInput("breaknumber", "Break Number:",min = 1, max = 20, value = 15)  )
    
    
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    # h3(textOutput("caption")),
    # 
    # plotOutput("mpgPlot"),
    # 
    # tableOutput("mydata")
    
    downloadButton('pngobj', 'Download Plot'),
    
    uiOutput("tb")
    
    # tabPanel("Plot", fluidRow(plotOutput("mpgPlot", width = input$PlotWidth, height = input$PlotHeight),
    #                           
    #                          tableOutput("mydata"),
    #                          textOutput("caption") )
             
    
    
    
  )
))
