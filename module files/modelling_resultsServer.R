modelling_resultsServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      source_python("python_modeller.py")
      
      #source all files in back_end
      for(f in list.files(path="./static/plots/*",
                          recursive=TRUE, full.names=TRUE)){
        print(as.character(f))
        source(f)
      }
      
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
           
           
           output$model_perf_Text <- renderText('Model Performance (Top 2):')
           
           output$model1FI_Text <- renderText('MLR Feature Importances :')
           output$model2FI_Text <- renderText(paste0(scoresTable$Algorithm[2],' Feature Importances:'))
           
           output$model1FI_Plot1_Text <- renderText('MLR Feature Importances (Plot):')
           output$model2FI_Plot1_Text <- renderText(paste0(scoresTable$Algorithm[2],' Feature Importances (Plot):'))
           
           output$model1_Eqn_Text <- renderText('MLR Model Equation:')
           output$model2_Eqn_Text <- renderText(paste0(scoresTable$Algorithm[2],' Model Equation:'))

           output$model1_Residuals_Text <- renderText('MLR Model Residuals')
           output$model2_Residuals_Text <- renderText(paste0(scoresTable$Algorithm[2],' Model Residuals:'))
           
           output$model1_ActPre_Text <- renderText('MLR Model Real v/s Predicted')
           output$model2_ActPre_Text <- renderText(paste0(scoresTable$Algorithm[2],' Model Real v/s Predicted:'))
           
           output$material_heatmap_Text <- renderText('Correlation Heatmap')
           
           model1Eqn <- get_equation(model1)
           model2Eqn <- get_equation(model2)
        
           output$model1_EqnUI <- renderUI({
             wellPanel(
               HTML(paste("<b><i>",model1Eqn,"</i></b>"))
             )
           })
           
           output$model2_EqnUI <- renderUI({
             wellPanel(
               HTML(paste("<b><i>",model2Eqn,"</i></b>"))
             )
           })
           
          output$model1_Eqn_Button <- renderUI({
             ns <- NS(id) 
             downloadBttn(
               outputId = ns('model1_Eqn_Download'),
               label = "Download",
               color = "primary",
               style = "material-circle",
               size= 'sm',
               block = FALSE
             )
           })
           
           output$model2_Eqn_Button <- renderUI({
              ns <- NS(id) 
              downloadBttn(
                 outputId = ns('model2_Eqn_Download'),
                 label = "Download",
                 color = "primary",
                 style = "material-circle",
                 size= 'sm',
                 block = FALSE
               )
            })
           
           output$model1_Eqn_Download <-
             downloadHandler(
               filename = "Equation1.txt",
               content = function(con){
                 writeLines(model1Eqn, con)
               }
             )
           
           output$model2_Eqn_Download <-
             downloadHandler(
               filename = "Equation2.txt",
               content = function(con){
                 writeLines(model2Eqn, con)
               }
             )
           
           output$material_heatmap <- renderUI({
                 tags$img(src = 'Correlation_Heatmap.jpg',width="100%")
             }
            )
           output$model1_Residuals <- renderUI({
             tags$img(src = 'Plot_MLR_Residuals.jpg',width="100%")
           }
           )
           output$model1_ActPre <- renderUI({
             tags$img(src = 'Plot_MLR_Predicted.jpg',width="100%")
           }
           )
      })
     
})}