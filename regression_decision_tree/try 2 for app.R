library(shiny)

ui<- fluidPage(
  
  titlePanel("Linear Modeling Workflow"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      fileInput('file1',"choose csv file", accept = ".csv"),
      checkboxInput('header',"Header",TRUE),
      selectInput(
        inputId = 'response_var',
        label = 'select response variable',
        choices = NULL
      )
      
    ),
  mainPanel(
    tableOutput('contents')
    
  )
    
    )
  
)




server<- function(input, output, session){
  dataInput<-reactive({
    file<-input$file1
    if(is.null(input$file1)){
      return(NULL)
    }
    ext<-tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == 'csv','please upload a csv file'))
    
    read.csv(file$datapath, header = input$header)
  })
  output$contents<-renderTable({
    head(dataInput())
  })
  observe({
    #isCharacter<-vapply(dataInput(),is.character,logical(1))|vapply(dataInput(),is.character,logical(1))
    characterCols<-names(dataInput())
    updateSelectInput(session,'response_var',
                 choices = characterCols,
                 selected=NULL)
    })
  
  
}

shinyApp(ui,server)