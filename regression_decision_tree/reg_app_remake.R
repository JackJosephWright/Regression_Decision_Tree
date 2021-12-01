library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(car)
library(dlookr)
library(tidyverse)
library(ggResidpanel)
library(leaps)
library(janitor)
library(stringr)
library(DT)
library(stringr)


ui = fluidPage(
  
  tabsetPanel(
    ## data input tab
          tabPanel("Managing Response and Predictor Variables", fluid = TRUE,
                   sidebarLayout(
                     sidebarPanel(
                       fileInput('file','choose csv file', accept = '.csv'),
                       checkboxInput('header','Header',TRUE),
                       selectInput(
                         inputId = 'response_var',
                         label  = 'select response variable',
                         choices = NULL
                       ),
                       
                       
                       checkboxGroupInput(
                         inputId = 'p_vars',
                         label = 'select response variables',
                         choices = NULL
                       )
                     ), #close sidebar panel data input tab
                     mainPanel(
                       
                       verbatimTextOutput('data_structure'),
                       tableOutput('contents')
                     )
                   )
                  ), #close data input tab
          # transformations tab
          tabPanel('Transformations', fluid = TRUE,
                   
                   sidebarLayout(
                     sidebarPanel(
                       actionButton('clear_transformations','clear all transformations',class = 'btn-danger'),
                       #response transformation
                       radioButtons(
                         inputId = 't_type_r',
                         label = 'select transformation type for response variable',
                         choices = c('log','sqrt', 'none'),
                         selected = 'none'
                         
                       ),
                       actionButton(
                         "acpt_t_r", "accept transformation", class = 'btn-success'
                                    ),# close response transformation radio
                        #x1 transformation
                       selectInput(inputId = 'x1',
                                   label = 'predictor variable x1',
                                   choices = NULL),#close select x1
                       radioButtons(
                         inputId = 't_type_x1',
                         label = 'select transformation type for x1 predictor',
                         choices = c('log','sqrt','poly', 'none'),
                         selected = 'none'
                         
                       ), # close radio button,
                       actionButton(
                         "acpt_t_x1", "accept transformation", class = 'btn-success'
                       ), # close action button,
                       
                       #x2 transformation
                       selectInput(inputId = 'x2',
                                   label = 'predictor variable x2',
                                   choices = NULL),#close select x1
                       radioButtons(
                         inputId = 't_type_x2',
                         label = 'select transformation type for x2 predictor',
                         choices = c('log','sqrt','poly', 'none'),
                         selected = 'none'
                         
                       ), # close radio button,
                       actionButton(
                         "acpt_t_x2", "accept transformation", class = 'btn-success'
                       ) # close action button,
                       ), # close sidebarPanel,
                     mainPanel(
                       fluidRow(
                         splitLayout(cellWidths = c('50%','50%')),
                         plotOutput('r_x1'),
                         plotOutput('x1_x2')
                       )# close ggplots row
                      
                     ) #close mainPanel
                   ) # close sidebarLayout
                   
                   ) # close transformation tab
      
          ) # close tabsetPanel
 
)#fluidPage Close


server<-function(input, output, session){
  ## FUNCTIONS
  
  transform_column<-function(column,fn){
    #transforms a single column and returns a named vector
    if(fn=='log'){
      return(log_transform(column))
    }
    if(fn=='sqrt'){
      return(sqrt_transform(column))
    }
    else{
      return(poly_transform(column))
    }
    
    
  }
  log_transform<-function(column){
    min_val<-min(column)
    if(minval>0){
    return(log(column))
    }else{
      column<-column(abs(min_val)+.01)
      return(log(column))
    }
  }
  sqrt_transform<-function(column){
    min_val<-min(column)
    if(minval>0){
      return(sqrt(column))
    }else{
      column<-column+(abs(min_val)+.01)
      return(sqrt(column))
    }
  }
  poly_transform<-function(column){
    return((column)^2)
  }
  
  #plotting functions
  showplot1 <- function(indata, inx, iny){
    p <- ggplot(indata, 
                aes_q(x = as.name(names(indata)[inx]), 
                      y = as.name(names(indata)[iny])))
    p + geom_point()+geom_smooth(method='loess',color='blue')+geom_smooth(method='lm',color='red')
  }
  ## REACTIVE ELEMENTS
  
  ## data input tab
  raw_data<-reactive({
    # reactive element for the raw data loading
    file<-input$file
    if(is.null(input$file)){
      return(NULL)
    }
    ext<-tools::file_ext(file$datapath)
    req(file)
    validate(need(ext=='csv','please upload a tidy csv file'))
    
    output<-read.csv(file$datapath, header = input$header)
    output<-janitor::clean_names(output)
    output<-na.omit(output)
  })
  
  predictor_list<-reactiveVal()#holds list of predictors
  p_selected <-reactiveVal()
  ## transformation tab
  
  ## transformation tab
  
  df.t<-reactiveVal()
  toListen<-reactive({
    list(input$x1,input$x2)
  })
  
  
  ## OBSERVERS
  
    ##### data input tab#####
  observeEvent(input$response_var,{
    #sets the value of predictor_list when response var is triggered
    if(is.null(input$response_var)){
      
      NULL
    }else{
      
      all_vars<-names(raw_data())
      predictor_vars<-all_vars[all_vars!=input$response_var]
      predictor_list(predictor_vars)
    }
  })
  observeEvent(input$p_vars,{
    #update list of selected predictors
    p_selected(input$p_vars)
    
  })
  
  observe( {
    #sets the dropdown for 'p_vars'
    
    updateCheckboxGroupInput(session, 'p_vars', choices = predictor_list())
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####transformations tab####
  
  observeEvent(input$p_vars,{
    # set values on transformation page
    updateSelectInput(session, 
                      'x1',
                      choices = p_selected()
    )
    updateSelectInput(session, 
                      'x2',
                      choices = p_selected()
    )
  })
  observeEvent(input$clear_transformations,{
    #set all values to none on transformations
    updateRadioButtons(session,
                       't_type_r',
                       selected='none')
    updateRadioButtons(session,
                       't_type_x1',
                       selected='none')
    updateRadioButtons(session,
                       't_type_x2',
                       selected='none')
    #delete transformation table
    
    
  })
  
  observeEvent(toListen(),{
    #create plot frame 
    d<-data.frame(cbind(raw_data()[input$x1],raw_data()[input$x2],raw_data()[input$response_var]))
    df.t(d)
    message('colnames df.t:',colnames(df.t()))
    
  })
  
  observeEvent(toListen(),{
    #generate x1 vs response
    output$r_x1<-renderPlot({showplot1(df.t(),1,3)})
  })
  observeEvent(toListen(),{
    output$x1_x2<-renderPlot(showplot1(df.t(),1,2))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##OUTPUTS
    ## data input tab
  output$contents<-renderTable(head(raw_data()))# output for raw_data table
  output$data_structure<-renderPrint(str(raw_data())) #structure for raw_data table
  
  
  observeEvent(input$file,{
    updateSelectInput(session, 'response_var',
                      choices = names(raw_data()),
                      selected = NULL
    )
  })
  
  
  
  
  
  
  
  ## TRANSFORMATION TAB
  
  #output$r_x1<-renderPlot({
  #  df<-df.t()
  # showplot1(df,input$x1,input$response_var)
  #})
  
  
  

}#close server


shinyApp(ui,server)

