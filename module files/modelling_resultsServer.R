modelling_resultsServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {

      output$model_title_Text <- renderText('Modelling Results')
      
      output$model_perf_Text <- renderText('Run the model to see results here.
                                            If run already, click on refresh to populate results.')
      
      
      
      observeEvent(input$refresh,
      {                
        
                       scores <- reactive(read.csv('static/tables/scores.csv'))
                       LASSO_FI <- reactive(read.csv('static/tables/LASSO_FI.csv'))
                       MLR_FI <- reactive(read.csv('static/tables/MLR_FI.csv'))
                       EN_FI <- reactive(read.csv('static/tables/EN_FI.csv'))
                       RG_FI <- reactive(read.csv('static/tables/RG_FI.csv'))

                       output$scoresTable <- renderDataTable(head(as.data.frame(scores()),2))

                       LASSO_FI_df <- as.data.frame(LASSO_FI())
                       MLR_FI_df <- as.data.frame(MLR_FI())
                       EN_FI_df <- as.data.frame(EN_FI())
                       RG_FI_df <- as.data.frame(RG_FI())
                        
                       output$mlrFI <- renderDataTable(MLR_FI_df,
                                                       extensions = "Buttons", 
                                                       options = list(paging = TRUE,
                                                                      scrollX=TRUE, 
                                                                      searching = TRUE,
                                                                      ordering = TRUE,
                                                                      dom = 'Bfrtip',
                                                                      buttons = list(
                                                                        list(extend = 'excel',
                                                                             filename = 'MLR Feature Importances',
                                                                             title = "MLR Feature Importances",
                                                                             header = FALSE),
                                                                        list(extend = 'pdf',
                                                                             filename = 'MLR Feature Importances',
                                                                             title = "MLR Feature Importances",
                                                                             header = FALSE)),
                                                                      pageLength=10, 
                                                                      lengthMenu=c(3,5,10)))
                       
                       
                       output$lassoFI <- renderDataTable(LASSO_FI_df,
                                                         extensions = "Buttons", 
                                                         options = list(paging = TRUE,
                                                                        scrollX=TRUE, 
                                                                        searching = TRUE,
                                                                        ordering = TRUE,
                                                                        dom = 'Bfrtip',
                                                                        buttons = list(
                                                                          list(extend = 'excel',
                                                                               filename = 'LASSO Feature Importances',
                                                                               title = "LASSO Feature Importances",
                                                                               header = FALSE),
                                                                          list(extend = 'pdf',
                                                                               filename = 'LASSO Feature Importances',
                                                                               title = "LASSO Feature Importances",
                                                                               header = FALSE)),
                                                                        pageLength=10, 
                                                                        lengthMenu=c(3,5,10)))

                       output$lassoFI_Plot1 <- renderPlotly(plot_ly(x = LASSO_FI_df$Importance, y = reorder(LASSO_FI_df$Feature, LASSO_FI_df$Importance), type = 'bar', orientation = 'h'))

                       output$mlrFI_Plot1 <- renderPlotly(plot_ly(x = MLR_FI_df$Importance, y = reorder(MLR_FI_df$Feature, MLR_FI_df$Importance), type = 'bar', orientation = 'h'))

                       output$mlrFI_Plot1_Text <- renderText('MLR Feature Importances (Plot):')
                       output$lassoFI_Plot1_Text <- renderText('LASSO Feature Importances (Plot):')
                       output$mlrFI_Text <- renderText('MLR Feature Importances :')
                       output$lassoFI_Text <- renderText('LASSO Feature Importances :')
                       output$model_perf_Text <- renderText('Model Performance (Top 2) :')

                       # if (file.exists("static/tables/scores.csv")){
                       #     scores <- reactive(read.csv('static/tables/scores.csv'))}
                       # if (file.exists("static/tables/LASSO_FI.csv")){
                       #     LASSO_FI <- reactive(read.csv('static/tables/LASSO_FI.csv'))}
                       # if (file.exists("static/tables/MLR_FI.csv")){
                       #     MLR_FI <- reactive(read.csv('static/tables/MLR_FI.csv'))}
                       #   
                       # observe(req(scores()),{
                       #   output$model_perf_Text <- renderText('Model Performance :')
                       #   output$scoresTable <- renderDataTable(as.data.frame(scores()))
                       #   
                       # })
                       # 
                       # observe(req(LASSO_FI()),{
                       #   
                       #   LASSO_FI_df <- as.data.frame(LASSO_FI())
                       #   output$lassoFI <- renderDataTable(LASSO_FI_df)
                       #   output$lassoFI_Plot1 <- renderPlotly(plot_ly(x = LASSO_FI_df$Importance, y = reorder(LASSO_FI_df$Feature, LASSO_FI_df$Importance), type = 'bar', orientation = 'h'))
                       #   output$lassoFI_Plot1_Text <- renderText('LASSO Feature Importances (Plot):')
                       #   output$lassoFI_Text <- renderText('LASSO Feature Importances :')
                       #   
                       # })
                       # 
                       # observe(req(MLR_FI()),{
                       #  MLR_FI_df <- as.data.frame(MLR_FI())
                       #  output$mlrFI <- renderDataTable(MLR_FI_df)
                       #  output$mlrFI_Plot1 <- renderPlotly(plot_ly(x = MLR_FI_df$Importance, y = reorder(MLR_FI_df$Feature, MLR_FI_df$Importance), type = 'bar', orientation = 'h'))
                       #  output$mlrFI_Plot1_Text <- renderText('MLR Feature Importances (Plot):')
                       #  output$mlrFI_Text <- renderText('MLR Feature Importances :')
                       #  
                       # })
                       
                       
           })
     
})}