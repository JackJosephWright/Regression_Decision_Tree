

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