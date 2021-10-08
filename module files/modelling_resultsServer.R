modelling_resultsServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      observeEvent(input$refresh,
                   {
                     
                     scores <- reactive(read.csv('static/tables/scores.csv'))
                     LASSO_FI <- reactive(read.csv('static/tables/LASSO_FI.csv'))
                     MLR_FI <- reactive(read.csv('static/tables/MLR_FI.csv'))
                     
                     output$scoresTable <- renderDataTable(as.data.frame(scores()))
                     
                     LASSO_FI_df <- as.data.frame(LASSO_FI())
                     #LASSO_FI_df <- LASSO_FI_df[order(LASSO_FI_df$Importance, decreasing = TRUE), ]
                     MLR_FI_df <- as.data.frame(MLR_FI())
                     #MLR_FI_df <- MLR_FI_df[order(MLR_FI_df$Importance, decreasing = TRUE), ]
                     
                     # output$lassoFI_Plot <- renderPlot(ggplot(LASSO_FI_df, aes(LASSO_FI_df$Importance,  reorder(LASSO_FI_df$Feature, LASSO_FI_df$Importance))) +    
                     #   geom_bar(stat = "identity",color='white',fill='#009E73')+ xlab('Importance')+ ylab('Feature'))
                     # 
                     # output$mlrFI_Plot <- renderPlot(ggplot(MLR_FI_df, aes(MLR_FI_df$Importance, reorder(MLR_FI_df$Feature, MLR_FI_df$Importance))) +    
                     #                                     geom_bar(stat = "identity",color='white',fill='#009E73')+ xlab('Importance')+ ylab('Feature'))
                     
                     output$mlrFI <- renderDataTable(as.data.frame(MLR_FI()))
                     output$lassoFI <- renderDataTable(as.data.frame(LASSO_FI()))
                     
                     
                     output$lassoFI_Plot1 <- renderPlotly(plot_ly(x = LASSO_FI_df$Importance, y = reorder(LASSO_FI_df$Feature, LASSO_FI_df$Importance), type = 'bar', orientation = 'h'))
                    
                     output$mlrFI_Plot1 <- renderPlotly(plot_ly(x = MLR_FI_df$Importance, y = reorder(MLR_FI_df$Feature, MLR_FI_df$Importance), type = 'bar', orientation = 'h'))
      
                  })
    }
  )
}