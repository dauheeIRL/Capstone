source('backEndCode.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  

  model <- eventReactive(input$textin, {
    
    if(length(input$textin) > 0){
      liData<-performBackoffSearch(input$textin)
      liData
    }
  })
  
  observeEvent(input$dt_cell_clicked, {
      
    if(!is.null(input$dt_rows_selected)){
      ret<-dtReturn[input$dt_rows_selected, PREDICT]
      updateTextInput(session, "textin", value = paste0(sub("\\s+$", "", input$textin), " ", ret))
    }
    
  })
   
  output$nextwords <- renderUI({
    
    if(length(input$textin) > 0){
      HTML(model()$nextwords)
    }else{
      HTML("")
    }
    
  })
  
  output$cleaned <- renderUI({
    
    if(length(input$textin) > 0){
      HTML(model()$cleaned)
    }else{
      HTML("")
    }
    
  })
  
  output$dt <- DT::renderDataTable({
    DT::datatable(model()$dt, selection = 'single', options = list(paging = FALSE, searching = FALSE))
    
  }, server = TRUE)
  
  # output$searchwords <- renderPrint({
  #   
  #   if(length(input$textin) > 0){
  #     model()$liData[2]
  #   }
  #   
  # })
  

  hide(id = "loading-content", anim = TRUE, animType = "fade") 
  
})
