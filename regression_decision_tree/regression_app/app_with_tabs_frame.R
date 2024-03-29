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
                 checkboxInput("timeSeries_TRUE", "is your response variable a time series?"),
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
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'trans_x',
                             label = 'predictor variable 1',
                             choices = NULL),
                 checkboxGroupInput(inputId='t_type_x',
                                    label = 'select transformation for x variable',
                                    choices = c('log','square root','reciprocal','polynomial')),
                 sliderInput("poly_x", "polynomial power", value = 2, min = 0, max = 5),
                 selectInput(inputId = 'trans_y',
                             label = 'variable_y',
                             choices = NULL),
                 checkboxGroupInput(inputId='t_type_y',
                                    label = 'select transformation for y variable',
                                    choices = c('log','square root','reciprocal','polynomial')),
                 sliderInput("poly_y", "polynomial power", value = 2, min = 0, max = 5),),
                 
               mainPanel(fluidRow(
                 column(4, plotOutput('vs_response') ),
                 column(4,verbatimTextOutput('pred_hist'))   
               ),
               fluidRow(
                 
                 plotOutput('correlation'),
                 'multicollinearity between predictors is certain at the 0.9 level of a correlation coefficient or higher. Consider dropping one of the predictors from your analysis '
               )
               )
             )
               
                 
               
             
    ),
    tabPanel("Modeling", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(),
               mainPanel(h1('base model'),fluidRow(
                 column(6,verbatimTextOutput('mod')),
                 column(6,plotOutput('base.resid')),
               ),
               fluidRow(
                 
                 box(h3('vif scores'),p('**VIF**


-The VIF of a predictor is a measure for how easily it is predicted from a linear regression using the other predictors. 

a VIF above 10 indicates high correlation and is cause for concern. Some authors suggest a more conservative level of 2.5 or above. 

if `VIF(predictor)>threshold`
  best practice is to *remove* the predictors with the highest VIF and rerun the model. '),tableOutput('base_VIF'))
               ),
               fluidRow(
                 h1('best subsets model'),
                 column(6,p('interpreting a `best subsets` model: \n 
                   the section `Selection Algorithm: the rows of the table are the number of variables used. A star in a column means, for a model with (x) variables, this was the best predictor.'),verbatimTextOutput('bestmod')),
                 column(6,plotOutput('best_sub_metric_plot'))
               ),
               fluidRow(
                 p('note: R^2 is best when maximized, and BIC and Cp are best when minimized'),
                 verbatimTextOutput('print_best_metrics'),
                 sliderInput("best_var_count", "select number of variables for optimized model", value = 1, min = 1, max = 10)
               ),
               fluidRow(
                 column(6,verbatimTextOutput('selectmod')),
                 column(6,plotOutput('best.resid'))
               )
               
               )
             )
    ),
    tabPanel("Model Selection and Analysis", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
               mainPanel(fluidRow(
                 tableOutput('modselect.table')   
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
    
    output<-read.csv(file$datapath, header = input$header)
    output<-janitor::clean_names(output)
    output<-na.omit(output)
  })
  dataInput_selected<-reactive({
    df<-as.data.frame(dataInput())
    selection_list<-as.list(full_vars())
    df<-df%>%select(c(unlist(selection_list)))
    #message(colnames(df))
    df
    
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
  
  ## get full variable list including response after selection
  
  full_vars<-reactive({append(p_list(),input$response_var)})
  
  ## reactive element of response name
  response_name<-reactive({input$response_var})
  
  
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
      #message(paste('response variable is',input$response_var)[input$response_var])
      if(n_distinct(dataInput()[input$response_var])>2){
        #message('response is continuous or count')
        updateRadioButtons(session, 'type_response',
                           choices=c('continuous','count'),
                           selected = NULL)
      }else{
        #message('response is count or binary')
        updateRadioButtons(session, 'type_response',
                           choices=c('binary','count'),
                           selected = NULL)
      }
      
      
    }
  })
  ##----TRANSFORMATION SERVER STUFF---
  
  ## set variable lists for transformations
  observe({
    #characterCols<-names(dataInput())
    vars_after_selection<-
    updateSelectInput(session, 'trans_x',
                      choices = full_vars(),
                      selected = NULL
    )
  })
  observe({
    #characterCols<-names(dataInput())
    vars_after_selection<-
      updateSelectInput(session, 'trans_y',
                        choices = full_vars(),
                        selected = NULL
      )
  })
  dat<-reactive({
    d<-data.frame(cbind(dataInput()[input$trans_x],dataInput()[input$trans_y],dataInput()[input$response_var]))
    
  })
  output$vs_response<-renderPlot({
    local_dat<-dat()
    x_transform_list<-as.list(input$t_type_x)
    y_transform_list<-as.list(input$t_type_y)
    if('log'%in% x_transform_list){
      local_dat[1]<-log(local_dat[1])
    }
    if('square root'%in% x_transform_list){
      local_dat[1]<-sqrt(local_dat[1])
    }
    if('reciprocal'%in% x_transform_list){
      local_dat[1]<-1/(local_dat[1])
    }
    if('polynomial'%in% x_transform_list){
      local_dat[1]<-local_dat[1]+I(local_dat[1]^input$poly_x)
    }
    if('log'%in% y_transform_list){
      local_dat[2]<-log(local_dat[2])
    }
    if('square root'%in% y_transform_list){
      local_dat[2]<-sqrt(local_dat[2])
    }
    if('reciprocal'%in% y_transform_list){
      local_dat[2]<-1/(local_dat[2])
    }
    if('polynomial'%in% y_transform_list){
      local_dat[1]<-local_dat[1]+I(local_dat[1]^input$poly_x)
    }
    #message(paste('col_1:',colnames(dat()[1]),'col_2:',colnames(dat()[2])))
    return(ggplot(local_dat,aes(x=unlist(local_dat[1]),y=unlist(local_dat[3])))+geom_point()+geom_smooth(method = 'loess', color='blue')+geom_smooth(method='lm',color='red')+labs(x=colnames(local_dat[1]),y=colnames(local_dat[3])))
  })
  output$correlation<-renderPlot({
    dlookr::plot_correlate(dataInput_selected())
  })
  mod_list<-reactive({
    m.list<-list()
    df<-dataInput_selected()%>%relocate(input$response_var, .after = last_col())
    formula_base<-paste0(input$response_var,'~.')
    #message(formula_base)
    m.list[['base']]<-lm(formula_base,data=dataInput_selected())
    max_model_variable<-length(p_list())
    #message(paste('df minus last column names:'),names(df[,-length(df)]))
    regsub<-regsubsets(as.matrix(df[,-length(df)]),df[,length(df)])
    
   
    
    m.list[['bestmod_summary']]<-regsub
    
    ## getting best subset
    best_sub_var_list<-as.list(names(coef(regsub,input$best_var_count)))[-1]
    best_vars_selected<-paste(unlist(best_sub_var_list),collapse="+")
    #message(best_vars_selected)
    #message(temp)
    bestmod_arg<-paste0(input$response_var,'~',best_vars_selected)
    message(bestmod_arg)
    m.list[['bestmod']]<-lm(bestmod_arg,data=df)
    #message(print(summary(m.list['bestmod'])))
    return(m.list)
  })
  output$mod<-renderPrint({
    
  summary(mod_list()[[1]])
  })
  output$selectmod<-renderPrint({
    
    summary(mod_list()[[3]])
  })
  output$bestmod<-renderPrint({
    summary(mod_list()[[2]])
  })
  output$best_sub_metric_plot<-renderPlot({
    results<-summary(mod_list()[[2]])
    tibble(predictors = 1:length(p_list()),
           adj_R2 = results$adjr2,
           Cp = results$cp,
           BIC = results$bic) %>%
      gather(statistic, value, -predictors) %>%
      ggplot(aes(predictors, value, color = statistic)) +
      geom_line(show.legend = F) +
      geom_point(show.legend = F) +
      facet_wrap(~ statistic, scales = "free")
  })
  output$base.resid<-renderPlot({
    ggResidpanel::resid_panel(mod_list()[[1]])
  })
  output$best.resid<-renderPlot({
    ggResidpanel::resid_panel(mod_list()[[3]])
  })
  output$base_VIF<-renderTable({
    vif_base<-car::vif(mod_list()[[1]])
    t(as.data.frame(vif_base))
  })
  output$print_best_metrics<-renderText({
    results<-summary(mod_list()[[2]])
    paste('best adjR2: ',which.max(results$adjr2),"best BIC: ",which.min(results$bic),"best Cp: ",which.min(results$cp))
    
  })
  observe({
    updateSliderInput(session, inputId='best_var_count',
                      min = 1, max = length(p_list()))
  })
  output$modselect.table<-renderTable({
    lm.list<-as.list(mod_list())[-2]
    AICs<-do.call(AIC,unname(lm.list))$AIC
    adjr2<-lapply(X=lm.list,
                  FUN=function(x) unlist(summary(x)$adj.r.squared))
    (select_df<-data.frame(Models=names(lm.list),AIC=round(AICs,2), R2=round(unlist(unname(adjr2)),2)))
  })
  
  
  
  
 
  
}

shinyApp(ui,server)
