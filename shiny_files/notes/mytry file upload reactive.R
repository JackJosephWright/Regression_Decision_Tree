## Only run examples in interactive R sessions
library(shiny)

  ui <- fluidPage(
    titlePanel("Linear Modeling Workflow"),
    sidebarLayout(
      
      sidebarPanel(
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        checkboxInput("header", "Header", TRUE)
      )
      ),
      mainPanel(
        tableOutput("contents")
      )
  )
  ui<- fluidPage(
    
    titlePanel("Linear Modeling Workflow"),
    sidebarLayout(
      
      sidebarPanel(
        
        
          fileInput('file1',"choose csv file", accept = ".csv"),
          checkboxInput('header',"Header",TRUE)
        
      ),
      mainPanel(
        tableOutput('contents')
      )
    )
  )

  
  server <- function(input, output) {
    dataInput <- reactive({
      file <- input$file1
      if (is.null(input$file1)){
        return(NULL)
      }
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      read.csv(file$datapath, header = input$header)
    })
    output$contents<-renderTable({ head(dataInput())})
  }
  
  shinyApp(ui, server)
}