modellerServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      source_python("python_modeller.py")
      unlink("static/tables/*")
      
      #-------------------------Download Sample Dataset--------------------------------
      
      
      sampleDatasetPaper<- as.data.frame(read_excel('www/Packaging Models Dataset Sample.xlsx',sheet = 'stack_2'))
      sampleDatasetPoly<- as.data.frame(read_excel('www/Packaging Models Dataset Sample.xlsx',sheet = 'stack_3'))
      
      

      output$sampleDataset_Download <-
        downloadHandler(
          filename = "Packaging Models Dataset Sample.xlsx",
          content = function(con){
            write_xlsx(list("stack_3" = sampleDatasetPoly, "stack_2" = sampleDatasetPaper),con)
          }
        )
      
      #--------------------------------Get Materials-----------------------------------
      
      observeEvent(input$polyFlag,
                   {
      observeEvent(req(input$dataset),{

        if(input$polyFlag == FALSE)
          {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))}
        if(input$polyFlag == TRUE)
          {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_3'))}
        
        
        material_names <- get_materials(data())
        #View(material_names)

        material_names_list <- strsplit(material_names, " +")                  
        #View(material_names_list)
        updateSelectInput(session,'material',choices = material_names_list, selected = material_names_list[1])
        
        output$materialNote <- renderUI({
          wellPanel(
              HTML(paste(em("Fetched "),"<b><i>",length(material_names_list),"</i></b>",em("materials from the uploaded dataset.")))
          )
        })
        
      })
    })
      
      #--------------------------------Get Predictors ----------------------------------
      
      observeEvent(input$polyFlag,
                   {
      observeEvent(req(input$dataset),{
        
        if(input$polyFlag == FALSE)
        {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))
          #View(data)
          }
        if(input$polyFlag == TRUE)
          {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_3'))}
        
        predictor_v <- get_predictors(data(),input$polyFlag, input$valFlag)
        #View(predictor_v)
        predictor_v_list <- strsplit((predictor_v), " +")
        #View(predictor_v_list)
        updateSelectInput(session,'predictor_vars',choices = predictor_v_list, selected = predictor_v_list[1])
  
        output$predictorNote <- renderUI({
          wellPanel(
            HTML(paste(em("Fetched "),"<b><i>",length(predictor_v_list),"</i></b>",em("statistically significant predictors. (Categoricals with only one category throughout the dataset are dropped)")))
          )
        })
        
      })
    })
      
      #--------------------------------Get Model Results ----------------------------------
  
      observeEvent(input$polyFlag,
                   {
        observeEvent(req(input$dataset),{
        
                observeEvent(input$build,ignoreInit = TRUE,

                  {
                       
                        if(input$polyFlag == FALSE)
                        {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))
                        #View(data)
                        }
                        if(input$polyFlag == TRUE)
                        {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_3'))}
                       
                       unlink("static/tables/*")
                        
                       output$results <- renderUI({ns <- NS(id)
                       shinycssloaders::withSpinner(dataTableOutput(ns("resultstable")),type = 3,color.background = 'lightblue')})
                       data_sent <- data()
                       # View(data_sent)
                       # View(input$predictor_vars)
                       # View(input$material)
                       waiter_show( # show the waiter
                         #id = 'build',
                         html = tagList(
                           spin_puzzle(),
                           h3("Building models, please wait..."),
                           br(),
                           h5("Estimated Wait Time : < 1 min")
                         ) # use a spinner
                       )
                       data_sent_df <- as.data.frame(data_sent,row.names = NULL)
                       #View(1)
                       model_results <- run_model(data_sent_df,input$predictor_vars,input$material,input$polyFlag, input$valFlag)
                       #View(2)
                       scores <- model_results[[1]]
                       #View(3)
                       MLR_FI <- model_results[[2]]
                       LASSO_FI <- model_results[[3]]
                       EN_FI <- model_results[[4]]
                       RG_FI <- model_results[[5]]
                       
                       write.csv(scores,file='static/tables/scores.csv',append=FALSE, row.names = FALSE)
                       write.csv(MLR_FI,file='static/tables/MLR_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(LASSO_FI,file='static/tables/LASSO_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(EN_FI,file='static/tables/EN_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(RG_FI,file='static/tables/RG_FI.csv',append=FALSE, row.names = FALSE)
                       #View(4)
                       
                       waiter_hide()
                       
                       output$model_title1_Text <- renderText('Results Summary ')
                       output$model_title2_Text <- renderText('Models Built - ')
                       
                       if(input$valFlag == TRUE){
                          scoresTable <- as.data.frame(c(scores['Algorithm'],scores['Train_R2'],scores['Validation_R2'],scores['Train_RMSE'],scores['Validation_RMSE']))
                       }
                       if(input$valFlag == FALSE){
                          scoresTable <- as.data.frame(c(scores['Algorithm'],scores['R2'],scores['RMSE']))
                       }
                       
                       output$resultstable <- renderDataTable(scoresTable)
                       
                       # observeEvent(req(scores),{
                       #   output$images <- renderUI(img(src= "static/plots/Plot_LASSO_coefs.jpg"))
                       #      
                       # })
                       output$changeTabButton <- renderUI({ns <- NS(id)
                       actionBttn(
                         inputId = ns('goToResults'),
                         label = "Go to Results",
                         color = "success",
                         style = "simple",
                         icon = icon("paper-plane"),
                         size= 'md',
                         block = FALSE
                       )
                       })  
                       
                       # attr(input$build, "readonly") <- FALSE
                       # input$build <- FALSE
     
      })  
      # --------------------------------- Move to results tab --------------------------------------  
        
        
      })
      })
      observeEvent(input$goToResults,{
        updateTabsetPanel(session = top_session, 
                          "tabs",
                          selected = 'RESULTS')
      })
      
      # observeEvent(input$polyFlag,{
      #           js$reset()
      #   updateTabsetPanel(session = top_session, 
      #                     "tabs",
      #                     selected = 'BUILDER')
      # }
      #)
    }
  )
}