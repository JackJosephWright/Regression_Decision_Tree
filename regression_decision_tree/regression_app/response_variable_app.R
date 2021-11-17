library(shiny)
library(shinydashboard)
library(shinyjs)

ui<-fluidPage(
  useShinyjs(),
  titlePanel('Linear Modeling Workflow'),
  
  fileInput('file1','choose csv file', accept = '.csv'),
  checkboxInput('header','Header',TRUE),
  selectInput(
    inputId = 'response_var',
    label  = 'select response variable',
    choices = NULL
  ),
  tableOutput('contents'),
  box(id='dtype_response_box',width='10px',
    radioButtons(
      'type_response','What datatype is your response variable',
                 choices = list('continuous','binary','count')
                 ),
    tags$div(
      "If you want to learn more about response variable datatype, ",
      tags$a(href="https://statisticsbyjim.com/regression/choosing-regression-analysis/", 
             "click here")
    )
  ),
  box(id='predictor_requirements',
      checkboxGroupInput('pred_select','select which predictors you would like to use')
      )
  
  
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
  variable_list<-reactive({
    characterCols<-names(dataInput())
  })
  #make list for response variables
  observe({
    #characterCols<-names(dataInput())
    updateSelectInput(session, 'response_var',
                      choices = variable_list(),
                      selected = NULL
                      )
  })
  output$contents<-renderTable(head(dataInput()))
  observeEvent(input$response_var,{
    shinyjs::toggle('dtype_response_box')
  })
}

shinyApp(ui,server)