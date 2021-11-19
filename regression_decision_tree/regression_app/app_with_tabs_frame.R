library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)


ui = fluidPage(
  tabsetPanel(
    tabPanel("Managing Response and Predictor Variables", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1','choose csv file', accept = '.csv'),
                 checkboxInput('header','Header',TRUE),
                 selectInput(
                   inputId = 'response_var',
                   label  = 'select response variable',
                   choices = NULL
                 ),
                 radioButtons(
                   'type_response','What datatype is your response variable',
                   choices = c('empty_obs'),
                   selected = NULL
                 ),
                 checkboxGroupInput(
                   inputId = 'predict_vars',
                   label = 'select response variables',
                   choices = NULL
                 )
               ),
               mainPanel(
                 
                 verbatimTextOutput('data_structure'),
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Transformations", fluid = TRUE,
             
               
                 uiOutput('interactionUI')
               
             
    ),
    tabPanel("Modeling", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
               mainPanel(fluidRow(
                 column(7,  plotlyOutput("")),
                 column(5, plotlyOutput(""))   
               )
               )
             )
    ),
    tabPanel("Model Analysis", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
               mainPanel(fluidRow(
                 column(7,  plotlyOutput("")),
                 column(5, plotlyOutput(""))   
               )
               )
             )
    )
  ),
)

server<-function(input, output, session){
  #data frame reactive element
  dataInput<-reactive({
    file<-input$file1
    if(is.null(input$file1)){
      return(NULL)
    }
    ext<-tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == 'csv','please upload a tidy csv file'))
    
    read.csv(file$datapath, header = input$header)
  })
  ## ------RESPONSE VARIABLE TAB-------
  #---
  
  #create variable list reactive element
  variable_list<-reactive({
    characterCols<-names(dataInput())
  })
  #---
  
  ## create possible predictor list reactive element
  
  possible_predictor_list<-reactive({
    variable_list()[variable_list()!=input$response_var]
  })
  #---
  
  ## get selected predictors
  
  p_list<-reactive({input$predict_vars})
  
  
  #make list for response variables
  observe({
    #characterCols<-names(dataInput())
    updateSelectInput(session, 'response_var',
                      choices = variable_list(),
                      selected = NULL
    )
  })
  #----
  
  
 
  #prints head of table on variable tab
  output$contents<-renderTable(head(dataInput()))
  #---
  output$data_structure<-renderPrint(str(dataInput()))
  observe({
  updateCheckboxGroupInput(session,'predict_vars', choices=possible_predictor_list())
  })
  # set the response variable types
  observeEvent(input$response_var,{
    if(!(is.null(input$response_var))){
      message(paste('response variable is',input$response_var)[input$response_var])
      if(n_distinct(dataInput()[input$response_var])>2){
        message('response is continuous or count')
        updateRadioButtons(session, 'type_response',
                           choices=c('continuous','count'),
                           selected = NULL)
      }else{
        message('response is count or binary')
        updateRadioButtons(session, 'type_response',
                           choices=c('binary','count'),
                           selected = NULL)
      }
      
      
    }
  })
  ##----TRANSFORMATION SERVER STUFF---
  
  ## make reactive counter to count how many predictors there are
  counter<-reactive({length(p_list())+1})
  
  
  output$interactionUI <- renderUI({
    n <- counter()
    
    plot_output_list <- lapply(1:p_list(), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 280, width = 250)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  for (i in 1:counter()) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        plot(1:my_i, 1:my_i,
             xlim = c(1, max_plots),
             ylim = c(1, max_plots),
             main = paste("1:", my_i, ".  n is ", input$n, sep = "")
        )
      })
    })
  }
}

shinyApp(ui,server)
