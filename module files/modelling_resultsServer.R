modelling_resultsServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      source_python("python_modeller.py")
      
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
           
           scoresTable <- head(as.data.frame(scores()),2)

           output$scoresTable <- renderDataTable(scoresTable)

           LASSO_FI_df <- as.data.frame(LASSO_FI())
           MLR_FI_df <- as.data.frame(MLR_FI())
           EN_FI_df <- as.data.frame(EN_FI())
           RG_FI_df <- as.data.frame(RG_FI())
           
           model1 <- MLR_FI_df
           
           output$model1FI <- renderDataTable(model1,
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
           
       
           
           if(scoresTable$Algorithm[2] == 'MLR + Lasso')
           {
             model2 <- LASSO_FI_df
           }
           if(scoresTable$Algorithm[2] == 'MLR + Elastic Net')
           {
             model2 <- EN_FI_df
           }
           if(scoresTable$Algorithm[2] == 'MLR + Ridge')
           {
             model2 <- RG_FI_df
           }
           
            output$model2FI <- renderDataTable(model2,
                                             extensions = "Buttons", 
                                             options = list(paging = TRUE,
                                                            scrollX=TRUE, 
                                                            searching = TRUE,
                                                            ordering = TRUE,
                                                            dom = 'Bfrtip',
                                                            buttons = list(
                                                              list(extend = 'excel',
                                                                   filename = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                                                                   title = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                                                                   header = FALSE),
                                                              list(extend = 'pdf',
                                                                   filename = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                                                                   title = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                                                                   header = FALSE)),
                                                            pageLength=10, 
                                                            lengthMenu=c(3,5,10)))

           output$model2FI_Plot1 <- renderPlotly(plot_ly(x = model2$Importance, y = reorder(model2$Feature, model2$Importance), type = 'bar', orientation = 'h'))

           output$model1FI_Plot1 <- renderPlotly(plot_ly(x = model1$Importance, y = reorder(model1$Feature, model1$Importance), type = 'bar', orientation = 'h'))
           
           
           output$model1FI_Plot1_Text <- renderText('MLR Feature Importances (Plot):')
           output$model2FI_Plot1_Text <- renderText(paste0(scoresTable$Algorithm[2],' Feature Importances (Plot):'))
           output$model1FI_Text <- renderText('MLR Feature Importances :')
           output$model2FI_Text <- renderText(paste0(scoresTable$Algorithm[2],' Feature Importances:'))
           output$model1_Eqn_Text <- renderText('MLR Model Equation')
           output$model2_Eqn_Text <- renderText(paste0(scoresTable$Algorithm[2],' Model Equation:'))
           output$model_perf_Text <- renderText('Model Performance (Top 2):')
           
           
           model1Eqn <- get_equation(model1)
           model2Eqn <- get_equation(model2)
           
           output$model1_Eqn <- renderText(model1Eqn)
           output$model2_Eqn <- renderText(model2Eqn)
             
      })
     
})}