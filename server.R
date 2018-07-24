library(shiny)
library(datasets)
library(ggplot2)

shinyServer(function(input, output, session) {

####################################
########## import data
####################################
importData <- reactive({
    
    validate(need(input$userData$datapath != "", "Upload data to start"))
    
    f <- read.csv(input$userData$datapath, na = ".")
    
    updateSelectInput(session, "FilterColumn",choices = names(f) )
    
    #updateTextInput(session, "FilterValue", value="")
    
  if(input$plotType=='scatter'){
      
      updateSelectInput(session, "dataCol_x", choices = names(f), selected = "wafer")
      
      updateSelectInput(session, "dataCol_y", choices = names(f), selected = "Ccb")
      
      updateSelectInput(session, "ColorBy",   choices = names(f), selected = "Description" )
      
      updateSelectInput(session, "ShapeBy",   choices = names(f), selected = "Description" )
      
      
  }
  else if(input$plotType=='hist'){
      
      updateSelectInput(session, "hist_Col", choices = names(f), selected = NULL)
      
  }
    
  f
   
})
  
#########################################
############ filter data
#########################################
filterdata <- reactive({
  f<-importData()
  df <-f
  if(!(toString(input$FilterValue) == "")){
      
    if(toString(input$FilterValueType)=="Numeric"){
        
      df <- f[ f[ toString(input$FilterColumn)]==as.numeric(toString(input$FilterValue) ) ,  ]
        
    }
  else if (toString(input$FilterValueType)=="String"){
        
    df <- f[ f[ toString(input$FilterColumn)]==toString(input$FilterValue) , ]
        
    }
      
  }
    df
})


####################################################
###############Summary data based on plot selection
####################################################

summarydata <- reactive({
  
  f<-filterdata()
  
  df <- f
  
  if(input$plotType=='scatter'){
    
    aa <- c(input$dataCol_x, input$dataCol_y, input$ColorBy, input$ShapeBy, input$FacetBy, input$FilterColumn)
    
    selectcolumn <- unique(aa[aa !=""])

    df <- f[, selectcolumn]
    
  }
  else if(input$plotType=='hist'){
    
    bb <- c(input$hist_Col)
    
    selectcolumn <- unique(bb[bb !=""])
    
    df <- f[, selectcolumn]
    
  }
  
  groupbylist <- selectcolumn[selectcolumn!=input$dataCol_y ]
  
  library(dplyr)
  meandata <- df %>%
    group_by_( .dots=groupbylist) %>%
    summarize_each_(funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = TRUE),  size = n()), 
                   c(input$dataCol_y) )
  
  meandata
  
  
})




  
  
# Compute the forumla text in a reactive expression since it is 
# shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    
    paste("Interactive plot for ", input$userData$name, toString(input$FilterColumn), toString(input$FilterValue), toString(input$FilterValueType))
    
  })
  

  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  
  # Generate a plot of the requested variable against mpg and only
  # include outliers if requested

  output$allmydata <- renderTable ({
    #filterdata()
    importData()
  }, width='800')
  
  output$allfiltermydata <- renderTable ({
    filterdata()
    #importData()
  }, width='800')
  
  
  output$summarydata <- renderTable ({
    summarydata()
    #importData()
  }, width='800')
  
  

  
  


#################################################
######## This is to generate the ggplot object
#################################################
PlotObject <- function(){
    
  ##############for scatter plot ##########
  a <- filterdata()
  
  if(toString(input$plotType)=="scatter"){
    #######Set x and y variable as numerical or character data from input on the side panel
    
    xxtype <- reactive({input$xtype})
    yytype <- reactive({input$ytype})
    
    if(toString(xxtype()) =="Numeric"){
      a[,toString(input$dataCol_x)] <- as.numeric(as.character(a[,toString(input$dataCol_x)]))
    }
    else if(toString(xxtype()) =="Category"){
      a[,toString(input$dataCol_x)] <- as.character(a[,toString(input$dataCol_x)])
    }
    
    if(yytype()=="Numeric"){
      a[,toString(input$dataCol_y)] <- as.numeric(as.character(a[,toString(input$dataCol_y)]))
    }
    else if(yytype() =="Category"){
      a[,toString(input$dataCol_y)] <- as.character(a[,toString(input$dataCol_y)])
    }
    #check facet option
    CheckFacet <- reactive ({
      !(input$FacetBy == "")
    })
    
    p <- ggplot(data = a,  aes_string(x = input$dataCol_x, y = input$dataCol_y, color=input$ColorBy, shape=input$ShapeBy) )
    
    if(toString(xxtype()) =="Numeric"){
      p <- p + geom_point(size=3)
    }
    else if(toString(xxtype()) =="Category"){
      p <- p + geom_point(size = 3, position = position_jitterdodge(jitter.width = 0.5))
    } 
    
    if(CheckFacet()){
      p <- p +  facet_wrap(as.formula(paste("~", input$FacetBy)))
    }
    
    p <- p + theme(legend.position="bottom", legend.title=element_blank(), text = element_text(size=input$text_size) )
    
  }
  else if(toString(input$plotType)=="hist"){
    
    maximum_col <- max(a[, input$hist_Col], na.rm = TRUE)
    minimum_col <- min(a[, input$hist_Col], na.rm = TRUE)
    
    bynn <- (maximum_col-minimum_col)/(input$breaknumber)
    
    p <- ggplot(data=a, aes_string(input$hist_Col)) 
    p <- p + geom_histogram(breaks=seq(minimum_col, maximum_col, by = bynn), alpha = 0.2)
    p <- p + geom_density(col=2)
    p <- p + theme(legend.position="bottom", legend.title=element_blank(), text = element_text(size=input$text_size) )
  }
  return(p)
}
    
    
    #############################################
    ######### output the plot object to the GUI
    #############################################
output$mpgPlot <- renderPlot({
      
  print(PlotObject())
      
})
    
    ################################################
    ########  download the plot object to .png file
    ################################################
    
output$pngobj = downloadHandler(
    filename = function() { 
      if(toString(input$plotType)=="scatter"){
        
        fname <- paste( input$dataCol_y, '_vs_', input$dataCol_x, '.png', sep = '')
        
      }
      else if(toString(input$plotType)=="hist"){
        
        fname <- paste( input$hist_Col, '_histogram', '.png', sep = '')
      }
      return (fname)
    },
    
    content = function(file) {
      png(file, width = input$PlotWidth, height = input$PlotHeight, res=120)
      PlotObject()
      dev.off()
    },
    contentType = 'image/png'
)
    
################################################
########  output all to tabset panel
################################################ 
 
output$tb <- renderUI({

  tabsetPanel( tabPanel("Plot", plotOutput("mpgPlot", width = input$PlotWidth, height = input$PlotHeight), tableOutput("summarydata")), tabPanel("all data", tableOutput("allmydata")), tabPanel("filtered data", tableOutput("allfiltermydata"))    , tabPanel("text", textOutput("caption")  ) )


 })

})
