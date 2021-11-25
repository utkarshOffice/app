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
                 
                 if (file.exists('./www/tables/scores.csv'))
                 {
                     
                     scores <- reactive(read.csv('./www/tables/scores.csv'))
                     LASSO_FI <- reactive(read.csv('./www/tables/LASSO_FI.csv'))
                     MLR_FI <- reactive(read.csv('./www/tables/MLR_FI.csv'))
                     EN_FI <- reactive(read.csv('./www/tables/EN_FI.csv'))
                     RG_FI <- reactive(read.csv('./www/tables/RG_FI.csv'))
                     
               
                     scoresTable <- head(as.data.frame(scores()),2)
                     
                     output$scoresTable <- renderDataTable(scoresTable)
                     
                     LASSO_FI_df <- as.data.frame(LASSO_FI())
                     MLR_FI_df <- as.data.frame(MLR_FI())
                     EN_FI_df <- as.data.frame(EN_FI())
                     RG_FI_df <- as.data.frame(RG_FI())
                     
                     
                     if(scoresTable$Algorithm[1] == 'MLR + Lasso')
                     {
                       model1 <- LASSO_FI_df
                     }
                     if(scoresTable$Algorithm[1] == 'MLR + Elastic Net')
                     {
                       model1 <- EN_FI_df
                     }
                     if(scoresTable$Algorithm[1] == 'MLR + Ridge')
                     {
                       model1 <- RG_FI_df
                     }
                     if(scoresTable$Algorithm[1] == 'MLR')
                     {
                       model1 <- MLR_FI_df
                     }
                     
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
                     if(scoresTable$Algorithm[2] == 'MLR')
                     {
                       model2 <- MLR_FI_df
                     }
                     
                     
                     # output$model1FI <- renderDataTable(model1,
                     #                                 extensions = "Buttons", 
                     #                                 options = list(paging = TRUE,
                     #                                                scrollX=TRUE, 
                     #                                                searching = TRUE,
                     #                                                ordering = TRUE,
                     #                                                dom = 'Bfrtip',
                     #                                                buttons = list(
                     #                                                  list(extend = 'excel',
                     #                                                       filename = 'MLR Feature Importances',
                     #                                                       title = "MLR Feature Importances",
                     #                                                       header = FALSE),
                     #                                                  list(extend = 'pdf',
                     #                                                       filename = 'MLR Feature Importances',
                     #                                                       title = "MLR Feature Importances",
                     #                                                       header = FALSE)),
                     #                                                pageLength=10, 
                     #                                                lengthMenu=c(3,5,10)))
                     # output$model2FI <- renderDataTable(model2,
                     #                                  extensions = "Buttons", 
                     #                                  options = list(paging = TRUE,
                     #                                                 scrollX=TRUE, 
                     #                                                 searching = TRUE,
                     #                                                 ordering = TRUE,
                     #                                                 dom = 'Bfrtip',
                     #                                                 buttons = list(
                     #                                                   list(extend = 'excel',
                     #                                                        filename = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                     #                                                        title = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                     #                                                        header = FALSE),
                     #                                                   list(extend = 'pdf',
                     #                                                        filename = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                     #                                                        title = paste0(scoresTable$Algorithm[2],' Feature Importances'),
                     #                                                        header = FALSE)),
                     #                                                 pageLength=10, 
                     #                                                 lengthMenu=c(3,5,10)))
                     
                     model1_plotDf <- subset(model1, Feature!="Intercept")
                     model2_plotDf <- subset(model2, Feature!="Intercept")
                     
                     output$model2FI_Plot1 <- renderPlotly(plot_ly(x = model2_plotDf$Importance, y = reorder(model2_plotDf$Feature, model2_plotDf$Importance), type = 'bar', orientation = 'h'))
                     output$model1FI_Plot1 <- renderPlotly(plot_ly(x = model1_plotDf$Importance, y = reorder(model1_plotDf$Feature, model1_plotDf$Importance), type = 'bar', orientation = 'h'))
                     
                     
                     output$model_perf_Text <- renderText('Model Performance (Top 2):')
                     
                     # output$model1FI_Text <- renderText(paste0(scoresTable$Algorithm[1],' Feature Importances :'))
                     # output$model2FI_Text <- renderText(paste0(scoresTable$Algorithm[2],' Feature Importances:'))
                     
                     output$model1FI_Plot1_Text <- renderText(paste0(scoresTable$Algorithm[1],' Feature Importances (Plot):'))
                     output$model2FI_Plot1_Text <- renderText(paste0(scoresTable$Algorithm[2],' Feature Importances (Plot):'))
                     
                     output$model1_Eqn_Text <- renderText(paste0(scoresTable$Algorithm[1],' Model Equation:'))
                     output$model2_Eqn_Text <- renderText(paste0(scoresTable$Algorithm[2],' Model Equation:'))
                     
                     output$model1_Residuals_Text <- renderText(paste0(scoresTable$Algorithm[1],' Model Residuals:'))
                     output$model2_Residuals_Text <- renderText(paste0(scoresTable$Algorithm[2],' Model Residuals:'))
                     
                     output$model1_ActPre_Text <- renderText(paste0(scoresTable$Algorithm[1],' Model Real v/s Predicted:'))
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
                     
                     output$ssUI <- renderUI({
                       ns <- NS(id) 
                       actionBttn(
                         inputId = ns('ss'),
                         label = "Capture Results",
                         color = "primary",
                         style = "simple",
                         icon = icon("camera"),
                         size= 'md',
                         block = FALSE
                       )
                     })
                     #-----------------------------------Rendering images------------------------------------------
                     
                     if(scoresTable$Algorithm[1] == 'MLR + Lasso')
                     {
                       model1_path <- 'LASSO'
                     }
                     if(scoresTable$Algorithm[1] == 'MLR + Elastic Net')
                     {
                       model1_path <- 'EN'
                     }
                     if(scoresTable$Algorithm[1] == 'MLR + Ridge')
                     {
                       model1_path <- 'RG'
                     }
                     if(scoresTable$Algorithm[1] == 'MLR')
                     {
                       model1_path <- 'MLR'
                     }
                     
                     
                     
                     if(scoresTable$Algorithm[2] == 'MLR + Lasso')
                     {
                       model2_path <- 'LASSO'
                     }
                     if(scoresTable$Algorithm[2] == 'MLR + Elastic Net')
                     {
                       model2_path <- 'EN'
                     }
                     if(scoresTable$Algorithm[2] == 'MLR + Ridge')
                     {
                       model2_path <- 'RG'
                     }
                     if(scoresTable$Algorithm[2] == 'MLR')
                     {
                       model2_path <- 'MLR'
                     }
                     
                     
                     
                     output$material_heatmap <- renderImage({
                       # When input$n is 1, filename is ./images/image1.jpeg
                       filename <- normalizePath(file.path('./www/Correlation_Heatmap.jpg'))
                       # Return a list containing the filename
                       list(src = filename,
                            width = "32.5%",height="100%")
                     }, deleteFile = FALSE
                     )
                     
                     # 
                     output$model1_Residuals <- renderImage({
                       # When input$n is 1, filename is ./images/image1.jpeg
                       filename <- normalizePath(file.path(gsub(" ", "", paste('./www/Plot_',model1_path,'_Residuals.jpg'),fixed = TRUE)))
                       list(src = filename,
                            width = "65%",height="100%")
                     }, deleteFile = FALSE)
                     # 
                     # #output$model1_Residuals <- renderImage('Plot_MLR_Residuals.jpg')
                     # 
                     output$model1_ActPre <- renderImage({
                       # When input$n is 1, filename is ./images/image1.jpeg
                       filename <- normalizePath(file.path(gsub(" ", "", paste('./www/Plot_',model1_path,'_Predicted.jpg'),fixed = TRUE)))
                       # Return a list containing the filename
                       list(src = filename,
                            width = "65%",height="100%")
                     }, deleteFile = FALSE)
                     
                     
                     
                     output$model2_Residuals <- renderImage({
                       # When input$n is 1, filename is ./images/image1.jpeg
                       filename <- normalizePath(file.path(gsub(" ", "", paste('./www/Plot_',model2_path,'_Residuals.jpg'),fixed = TRUE)))
                       list(src = filename,
                            width = "65%",height="100%")
                     }, deleteFile = FALSE)
                     # 
                     # #output$model1_Residuals <- renderImage('Plot_MLR_Residuals.jpg')
                     # 
                     output$model2_ActPre <- renderImage({
                       # When input$n is 1, filename is ./images/image1.jpeg
                       filename <- normalizePath(file.path(gsub(" ", "", paste('./www/Plot_',model2_path,'_Predicted.jpg'),fixed = TRUE)))
                       # Return a list containing the filename
                       list(src = filename,
                            width = "65%",height="100%")
                     }, deleteFile = FALSE)
                     
                     
                     observeEvent(input$ss, {
                       screenshot()
                     })
                 }
                 
                 else{
                   showModal(modalDialog(HTML(paste0("<b> No results generated yet! </b> <br/>
                                              Please make sure you've built the models first in the prevoius tab"))))
                   
                 }
                     
            })
               
               
    
      
      
     
})}