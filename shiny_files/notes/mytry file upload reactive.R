## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        checkboxInput("header", "Header", TRUE)
      ),
      mainPanel(
        tableOutput("contents")
      )
    )
  )
  
  server <- function(input, output) {
    dataInput <- renderTable({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      read.csv(file$datapath, header = input$header)
    })
    output$contents<-renderTable(dataInput())
  }
  
  shinyApp(ui, server)
}