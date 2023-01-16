# Server Tabs

Aboutp = function(env_serv) with(env_serv, local({
   }))

RNAidbp = function(env_serv) with(env_serv, local({
   # Check all list for RNAi dataset checkgroupinput
   observe({
      if(input$selectall == 0) return(NULL)
      else if (input$selectall%%2 == 0)
      { updateCheckboxGroupInput(session,"show_vars", "", names(RNAidb), selected = col_list) }
      else
      { updateCheckboxGroupInput(session,"show_vars", "", names(RNAidb), selected = names(RNAidb))}
   })

    output$LastUpdate <- renderText("The dataset was last updated on: Jan 16 2023")
   
    output$mytable1 <- DT::renderDataTable({
    DT::datatable(RNAidb[, input$show_vars, drop = TRUE], 
                  filter = 'top', 
                  options = list(orderClasses = TRUE, 
                                 scrollX=TRUE,
                                 autoWidth = FALSE,
                                 pageLength = 20))
  })
  }))

Explorerp = function(env_serv) with(env_serv, local({
  #add reactive data information. 
  dataset <- reactive({
    Explorer[sample(nrow(Explorer), input$sampleSize),]
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point()
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE,
             width = input$plotWidth, autosize=TRUE) 
  })
}))