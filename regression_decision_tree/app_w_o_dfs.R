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
  
  tabsetPanel( id='tabSwitch',
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
                              label = 'select predictor variables',
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
                              splitLayout(style = "border: 1px solid silver;",cellWidths = c("50%", "50%"),
                                          
                                          column(12,plotOutput('r_x1')),
                                          column(12,plotOutput('x1_x2'))
                              )
                            ),# close ggplots row
                            fluidRow(
                              splitLayout(style = "border: 1px solid silver;",cellWidths = c("20%", "40%","40%"),
                                          tableOutput('cor_table'),
                                          plotOutput('correlation'),
                                          tableOutput('df.trans.stored')
                              )
                            )
                            
                          ) #close mainPanel
                        ) # close sidebarLayout
                        
               ),# close transformation tab
               tabPanel("Modeling", fluid = TRUE,
                        
                        
                        mainPanel(
                          h1('base model'),
                          fluidRow(
                            splitLayout(style = "border: 1px solid silver;",cellWidths = c("50%","50%"),
                                        verbatimTextOutput('base.mod'),
                                        plotOutput('base.resid')
                            )
                          ),
                          h1('transformation model'),
                          fluidRow(
                            splitLayout(style = "border: 1px solid silver;",cellWidths = c("50%","50%"),
                                        verbatimTextOutput('trans.mod'),
                                        plotOutput('trans.resid')
                            )
                          ),
                          h1('best subsets model'),
                          actionButton(
                            "best.mod.run", "run best subsets model (this might take a while...)", class = 'btn-success'
                          ),
                          fluidRow(
                            splitLayout(style = "border: 1px solid silver;",cellWidths = c("50%","50%"),
                                        column(12,verbatimTextOutput('best.mod.summary'),fluidRow(numericInput(inputId = 'size.best','select number of variables for best subset model',value=1 ),actionButton(inputId='submit.select','select',class='btn-success')) ),
                                        column(12,em('R^2 is best when maximized, and BIC and Cp are best when minimized'),plotOutput('best.submetric.plot'),verbatimTextOutput('best.metrics'))
                            ),
                            
                          ),
                          h1('selected variable model'),
                          fluidRow(
                            splitLayout(style = "border: 1px solid silver;",cellWidths = c("50%","50%"),
                                        verbatimTextOutput('select.mod'),
                                        plotOutput('select.resid')
                            )
                          )
                          
                          
                        )
                        
               ),#close modeling tab
               tabPanel("Model Selection and Analysis", fluid = TRUE,
                        sidebarLayout(
                          sidebarPanel(),
                          mainPanel(fluidRow(
                            tableOutput('modselect.table')   
                          )
                          )
                        )
               ) #close selection and analysis panel
               
  ), # close tabsetPanel
  
)#fluidPage Close


server<-function(input, output, session){
  ## FUNCTIONS
  t.specific<-function(action=NULL,i=0){
    action_button_list<-list('x1','x2','r')
    #transform column
    #get column
    if(action=='accept'){
      col<-as.data.frame(df.t.static()[toAccept()])
      
      #identify which button to check 
      transform_func<-paste0('t_type_',action_button_list[toAccept()])
      
      fn<-input[[transform_func]]
      if(fn=='none'){
        
        toAccept(NULL)
        return(NULL)
        
      }
      #check is not 'none', if 'none'break
    }
    if(action=='plot'){
      col<-as.data.frame(df.t.static()[i])
      transform_func<-paste0('t_type_',action_button_list[i])
      fn<-input[[transform_func]]
      
      
    }
    
    
    
    
    #put column and fn into transform_column()
    
    t.col<-transform_column(col,fn)
    if(action=='accept'){
      return(list(t.col,fn))
    }else{
      
      return(t.col)
    }
  }
  
  
  transform_column<-function(column,fn){
    #transforms a single column and returns a named vector
    if(fn=='log'){
      return(log_transform(column))
    }
    if(fn=='sqrt'){
      return(sqrt_transform(column))
    }
    if(fn=='poly'){
      return(poly_transform(column))
    }
    else{
      
      return(column)
    }
    
    
  }
  log_transform<-function(column){
    minval<-min(column)
    if(minval>0){
      return(log(column))
    }else{
      column<-column(abs(minval)+.01)
      return(log(column))
    }
  }
  sqrt_transform<-function(column){
    minval<-min(column)
    if(minval>0){
      return(sqrt(column))
    }else{
      column<-column+(abs(minval)+.01)
      return(sqrt(column))
    }
  }
  poly_transform<-function(column){
    return(column+(column)^2)
  }
  
  
 
  neq_nothing<-function(x){
    
    if(x!=""){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  #plotting functions
  showplot1 <- function(indata, inx, iny, type){
    xname<-as.name(names(indata)[inx])
    yname<- as.name(names(indata)[iny])
    if(ncol(indata)<2){
      yname=xname
      iny=inx
    }
    
    transform.holder$variables<-list(input$x1,input$x2,input$response_var)
    transform.holder$transformations<-list(input$t_type_x1,input$t_type_x2,input$t_type_r)
    
    t.list<-list(input$t_type_x1,input$t_type_x2,input$t_type_r)
    
    if(type=='target'){
      indata[inx]<-transform_column(indata[inx],fn=t.list[1])
      indata[iny]<-transform_column(indata[iny],fn=t.list[3])
    }else if(type == 'interaction'){
      indata[inx]<-transform_column(indata[inx],fn=t.list[1])
      indata[iny]<-transform_column(indata[iny],fn=t.list[2])
    }
  
    if(count(unique(indata[inx]))==2 & type=='target'){
    
      p <- ggplot(indata,
                  aes_q(x = xname,
                        y = yname, fill=yname))
      return(p+geom_boxplot())
    }
    
    
    
    p <- ggplot(indata,
                aes_q(x = xname,
                      y = yname))
    p + geom_point()+geom_smooth(method='loess',color='blue')+geom_smooth(method='lm',color='red')+ggpubr::stat_cor(method = "pearson")
    #> `geom_smooth()` using formula 'y ~ x'
    
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
  
  predictor_list<-reactiveVal(NULL)#holds list of predictors
  p_selected <-reactiveVal(NULL)
  ## transformation tab
  
  ## transformation tab reactive elements
  
  df.t<-reactiveVal(NULL)
  df.t.static<-reactiveVal(NULL)
  toListen<-reactive({
    if(any(sapply(list(input$x1,input$x2),neq_nothing))){
      list(input$x1,input$x2)
    }
    
  })
  plot.t.Listen<-reactive({
    NULL
    if(is.null(input$t_type_x1)|is.null(input$t_type_x2)){
      NULL
    }else{
      list(input$t_type_x1,input$t_type_x2,input$t_type_r)
    }

  })
  
  
  
  toAccept<-reactiveVal({
    #listener that a transformation has been accepted
    NULL
    
  })
  df.t.accepted<-reactiveVal({
    NULL
  })
  
 
  
  ## model tab reactive elements
  
  mod.list<-reactiveVal({NULL})
  df.base.mod<-reactiveVal({NULL})
  df.trans.mod<-reactiveVal(NULL)
  best.mod.sum<-reactiveVal(NULL)
  transform.holder <- reactiveValues(variables = NULL, transformations = NULL)
  accepted.transformations<-reactiveValues(df=data.frame(variables=character(),transformations=character()))
  
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
  
  observeEvent(input$file,{
    #set values for possible response_vars
    updateSelectInput(session, 'response_var',
                      choices = names(raw_data()),
                      selected = NULL
    )
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
  
  t.response.name<-reactiveVal(NULL)
  
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
    df.t.accepted(NULL)
    accepted.transformations$df<-data.frame(variables=character(),transformations=character())
    #delete transformation table
    
    
  })
  
 
  
  
  
  observeEvent(df.t.static(),{
    
    updateRadioButtons(session,
                       't_type_x1',
                       selected='none')
    updateRadioButtons(session,
                       't_type_x2',
                       selected='none')
    df.t(df.t.static())
    
    
  })
  
  observeEvent(toListen(),{
    df.t.static(list(input$x1,input$x2,input$response_var))
    
  })
  
  observeEvent(plot.t.Listen(),{
    
  })
  
  ##--COMMENTED OUT SECTION
  
 
  
 

  observeEvent(plot.t.Listen(),{
    
    output$r_x1<-renderPlot({
      
     
      showplot1(raw_data()%>%select(c(as.character(toListen()[[1]]),input$response_var)),inx=1,iny=2, type='target')
      
      
    })
   output$x1_x2<-renderPlot({
        
      
      showplot1(raw_data()%>%select(c(input$x1,input$x2)),inx=1,iny=2, type='interaction')
    })
  })
  
  #set value of accepting transformation Listener
  observeEvent(input$acpt_t_r,{
      toAccept(NULL)
      toAccept(3)

    })
    observeEvent(input$acpt_t_x1,{
      toAccept(NULL)
      toAccept(1)

    })
    observeEvent(input$acpt_t_x2,{
      toAccept(NULL)
      toAccept(2)

    })
  observeEvent(toAccept(),{
    
    accepted.transformations$df<-rbind(
      if(transform.holder$variables[toAccept()]==input$response_var){
        accepted.transformations$df%>%filter(variables!=input$response_var)
        
      }else{
        accepted.transformations$df
      }
      ,
      c(transform.holder$variables[toAccept()],
        transform.holder$transformations[toAccept()])
      )
    
    
       
    
    output$df.trans.stored<-renderTable({
    colnames(accepted.transformations$df)<-c('variables','transformations')
    if(input$response_var %in% accepted.transformations$df$variables){
      accepted.transformations$df<-rbind(accepted.transformations$df%>%filter(variables!=input$response_var),accepted.transformations$df%>%filter(variables==input$response_var))
    }
    unique(accepted.transformations$df)
     
      })
    })
  
  # observeEvent(df.t(),{
  #   output$x1_x2<-renderPlot(showplot1(df.t(),1,2))
  # })
  # 
  # 
  # # create corr table and corr list
  observeEvent(p_selected(),{
    df.local<-raw_data()%>%select(p_selected(),input$response_var)
    output$correlation<-renderPlot(dlookr::plot_correlate(df.local))
    output$cor_table<-renderTable(dlookr::correlate(df.local)%>%filter(var2==input$response_var)%>%select(var1,coef_corr)%>%arrange(desc(coef_corr)))
  })
  # 
  # #setting the value of toAccept() listener
  # observeEvent(input$acpt_t_r,{
  #   toAccept(3)
  #   
  # })
  # observeEvent(input$acpt_t_x1,{
  #   toAccept(1)
  #   
  # })
  # observeEvent(input$acpt_t_x2,{
  #   toAccept(2)
  #   
  # })
  # 
  # 
  # 
  # # generate df.t.accep
  # observeEvent(toAccept(),{
  #   t.col_list<-t.specific(action='accept')
  #   
  #   if(is.null(t.col_list)){
  #     return(NULL)
  #   }
  #   t.col<-as.data.frame(t.col_list[1])
  #   
  #   fn<-t.col_list[2]
  #   
  #   
  #   
  #   oldname<-names(t.col)
  #   
  #   newname<-paste0(fn,names(t.col))
  #   colnames(t.col)<-newname
  #   if(oldname==input$response_var){
  #     t.response.name(newname)
  #   }
  #   #check that the column doesnt already exist in df.t.accepted
  #   if(newname%in%colnames(df.t.accepted())){
  #     toAccept(NULL)
  #     return(NULL)
  #   }
  #   #if it is the response, remove the previous response transformation
  #   #append column to df.t.accepted
  #   if(trans_response_check(df.t.accepted() )& oldname==input$response_var){
  #     df.t.accepted(df.t.accepted()%>%select(-contains(input$response_var)))
  #     
  #   }
  #   if(is.null(df.t.accepted())){
  #     df.t.accepted(t.col)
  #   }else{
  #     df.t.accepted(cbind(df.t.accepted(),t.col))
  #   }
  #   
  #   
  #   toAccept(NULL)
  # })
  # observeEvent(df.t.accepted(),{
  #   output$df.trans.stored<-renderTable({
  #     colnames(df.t.accepted())
  #   })
  #   
  # })
  # 
  # 
  # ## models observers
  # 
  #
   observeEvent(input$tabSwitch, {
    #generate table to model off of
     if(!is.null(predictor_list())){
     
       if(input$tabSwitch =='Modeling'){
        #message( nrow(accepted.transformations$df))
         df.base<-raw_data()%>%select(p_selected(),input$response_var)
         message('created df.base locally')
         #message(colnames(transformation.df))
         if(nrow(accepted.transformations$df)==0){
          return(df.base.mod(df.base))
           
         }else{
           transformation.df<-apply(unique(accepted.transformations$df)[,c('variables','transformations')],1,function(x) transform_column(column=raw_data()[x[1]],fn=x[2]))%>%bind_cols()
           colnames(transformation.df)<-apply(unique(accepted.transformations$df)[,c('variables','transformations')],1,function(x) paste(x[2],x[1],sep = "_"))
           message('created transformation.df')
         }
         
         if(input$response_var %in% accepted.transformations$df$variables){
           df.full<-cbind(df.base%>%select(-input$response_var),transformation.df)
           return(df.base.mod(df.full))
           #message(col
         }else{
           df.full<-cbind(df.base,transformation.df)%>%relocate(input$response_var,.after = last_col())
           message('made it through creation of df.base.mod')
           return(df.base.mod(df.full))
         }
       }
     }
   })
   # 
  # 
  # observeEvent(input$tabSwitch, {
  #   if(!is.null(predictor_list())){
  #     if(input$tabSwitch=='Modeling'){
  # 
  #       #generate df.base.mod
  #       df.base.mod<-raw_data()%>%select(p_selected(),input$response_var)
  #       #generate df.trans.mod
  #       #message(colnames(df.base.mod))
  #       # if(length(df.t.accepted()>0)){
  #       #   if(grepl(input$response_var,colnames(df.t.accepted()))){
  #       #     df.trans.mod(cbind(df.base.mod()%>%select(-input$response_var),df.t.accepted())%>%relocate(t.response.name(),.after = last_col()))
  #       #   }else{
  #       #     df.trans.mod(cbind(df.base.mod(),df.t.accepted()))
  #       #   }
  #       # }else{
  #       #   d<-df.base.mod()
  #       #   df.trans.mod(d)
  #       # 
  #       # }
  # #       
  # #       #message(colnames(df.trans.mod()))
  #     }
  # #     
  #   }
  # })
  # 
  observeEvent(df.base.mod(),{
    #message('made it into the model list observer')
# 
    message('is it failing here?')
    #(formula_base)
    formula_base<-paste0(colnames(df.base.mod()%>%select(last_col())),"~.")
    message('or here')
    m<-list()
    base.vars<-colnames(df.base.mod()%>%select(c(p_selected(),last_col())))
    message('couldn"t be here?')
    m[['base']]<-lm(formula_base,data=df.base.mod()%>%select(base.vars))
     message(nrow(accepted.transformations$df!=0))
      m[['trans']]<-if(nrow(accepted.transformations$df!=0)){
      
        lm(formula_base,data=df.base.mod())
      
     }else{
         'no transformations included in modeling'
       }

    mod.list(m)

  })

   observeEvent(mod.list(),{
     #printing summary base.mod
     output$base.mod<-renderPrint(summary(mod.list()[[1]]))
     output$base.resid<-renderPlot(ggResidpanel::resid_panel(mod.list()[[1]]))
  })

  observeEvent(mod.list(),{
    #printing summary base.mod
    output$trans.mod<-renderPrint(summary(mod.list()[[2]]))
    output$trans.resid<-renderPlot(ggResidpanel::resid_panel(mod.list()[[2]]))
  })
  # observeEvent(mod.list(),{
  #   #plot residuals base.mod
  #   output$trans.resid<-renderPlot(ggResidpanel::resid_panel(mod.list()[[2]]))
  # })
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  ##OUTPUTS
  ## data input tab
  output$contents<-renderTable(head(raw_data()))# output for raw_data table
  output$data_structure<-renderPrint(str(raw_data())) #structure for raw_data table

  # 
  # 
  # 
  observeEvent(input$best.mod.run,{
    m<-mod.list()
    df<-df.base.mod()
    
    regsub<-regsubsets(as.matrix(df[,-length(df)]),df[,length(df)])
    best.summary<-summary(regsub)
    best.mod.sum(regsub)
    output$best.mod.summary<-renderPrint(summary(regsub))
    #   best.summary<-summary(regsub)
  #   #if(!identical(colnames(df.trans.mod()),colnames(df.base.mod()))){
  #   regsub<-regsubsets(as.matrix(df[,-length(df)]),df[,length(df)])
  #   best.summary<-summary(regsub)
  #   #best.summary<-summary(regsub)
  #   best.mod.sum(regsub)
    output$best.mod.summary<-renderPrint(summary(regsub))
    if(length(df[,-length(df)])>8){
      size<-8
    }else{
      size<-length(df[,-length(df)])
      #message(size)
    }
    output$best.submetric.plot<-renderPlot({
      tibble(predictors = 1:size,
             adj_R2 = best.summary$adjr2,
             Cp = best.summary$cp,
             BIC = best.summary$bic) %>%
        gather(statistic, value, -predictors) %>%
        ggplot(aes(predictors, value, color = statistic)) +
        geom_line(show.legend = F) +
        geom_point(show.legend = F) +
        facet_wrap(~ statistic, scales = "free")
    })
  #   
  #   
  })
  # 
  observeEvent(input$best.mod.run,{
    output$best.metrics<-renderText({
      results<-summary(best.mod.sum())
      paste('best adjR2: ',which.max(results$adjr2),"best BIC: ",which.min(results$bic),"best Cp: ",which.min(results$cp))
    })

  })
  # 
  # observeEvent(input$submit.select,{
  #   m<-mod.list()
  #   regsub<-best.mod.sum()
  #   best.sub.var.list<-as.list(names(coef(regsub,input$size.best)))[-1]
  #   #message(best.sub.var.list)
  #   best.selected<-paste(unlist(best.sub.var.list),collapse="+")
  #   if(is.null(t.response.name())){
  #     
  #     t.response<-input$response_var
  #     
  #   }else{
  #     t.response<-t.response.name()
  #     #message('t.response.name:',t.response.name())
  #   }
  #   
  #   best.mod.arg<-paste0(t.response,'~',best.selected)
  #   
  #   m[['best.mod']]<-lm(best.mod.arg,data=df.trans.mod())
  #   mod.list(m)
  #   output$select.mod<-renderPrint(summary(m[['best.mod']]))
  #   output$select.resid<-renderPlot(ggResidpanel::resid_panel(mod.list()[[3]]))
  # })
  # 
  # observeEvent(input$submit.select,{
  #   m.list<-as.list(mod.list())
  #   AICs<-do.call(AIC,unname(m.list))$AIC
  #   adjr2<-lapply(X=m.list,
  #                 FUN = function(x) unlist(summary(x)$adj.r.squared))
  #   output$modselect.table<-renderTable(data.frame(Models=names(m.list),AIC=round(AICs,2),R2 = round(unlist(unname(adjr2)),2)))
  # })
  # 
  # 
  # 
  # 
  # 
  # ## TRANSFORMATION TAB
  # 
  # #output$r_x1<-renderPlot({
  # #  df<-df.t()
  # # showplot1(df,input$x1,input$response_var)
  # #})
  # 
  # 
  # 
  
}#close server


shinyApp(ui,server)

