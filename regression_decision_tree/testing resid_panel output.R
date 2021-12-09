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
source('linreg_FUNCTIONS.R')

df<- read.csv("./data/Housing.csv")
t.df<-data.frame(variables=c('price','area'),transformation=c('log','log'))
ui = fluidPage(
  verbatimTextOutput('base.mod'),
  plotOutput('base.resid'),
  selectInput('response_var','response variable', choices='price',selected='price'),
  actionButton('act','run stuff')

  )

server<-function(input, output, session){

  raw_data<-reactive({df})
  accepted.transformations<-reactiveValues(df=t.df)
  p_selected<-reactiveVal(c('area','bedrooms'))
  mod.list<-reactiveVal({NULL})
  df.base.mod<-reactiveVal({NULL})
  
  
  observeEvent(input$act,{
    
    df.base<-raw_data()%>%select(p_selected(),input$response_var)
    
    df.base.mod(df.base)
    message(head(df.base.mod()))
  })
  
  observeEvent(df.base.mod(),{
    
    formula_base<-paste0(colnames(df.base.mod()%>%select(last_col())),"~.")
    base.vars<-colnames(df.base.mod()%>%select(c(p_selected(),colnames(df.base.mod()[ncol(df.base.mod())]))))
    m<-list()
    x<-lm(price~.,data=df)
    res<-resid(x)
    output$base.resid<-renderPlot({
      par(mfrow = c(2, 2))
      plot(x)
      })
  })
  
  observeEvent(mod.list(),{
    
  })
  
}



shinyApp(ui,server)