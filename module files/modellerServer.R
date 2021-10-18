modellerServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      source_python("python_modeller.py")
      unlink("static/tables/*")
      
      #--------------------------------Get Materials-----------------------------------
      observeEvent(req(input$dataset),{

        if(input$laminateFamily == FALSE)
          {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))}
        if(input$laminateFamily == TRUE)
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
      
      #--------------------------------Get Predictors ----------------------------------
      observeEvent(req(input$dataset),{
        if(input$laminateFamily == FALSE)
          {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))}
        if(input$laminateFamily == TRUE)
          {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_3'))}
        
        predictor_v <- get_predictors(data(),input$laminateFamily)
        predictor_v_list <- strsplit((predictor_v), " +")
        #View(predictor_v_list)
        updateSelectInput(session,'predictor_vars',choices = predictor_v_list, selected = predictor_v_list[1])
  
        output$predictorNote <- renderUI({
          wellPanel(
            HTML(paste(em("Fetched "),"<b><i>",length(predictor_v_list),"</i></b>",em("statistically significant predictors. (Categoricals with only one category throughout the dataset are dropped)")))
          )
        })
        
      })
      
      #--------------------------------Get Model Results ----------------------------------
      
      observeEvent(req(input$dataset),{
        
 
        data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))
        observeEvent(input$build,

                      {
                       #updateTabsetPanel(top_session, "tabs", selected = "modellingResults")
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
                       model_results <- run_model(data_sent,input$predictor_vars,'SS',input$material,FALSE)
                       
                       scores <- model_results[[1]]
                       MLR_FI <- model_results[[2]]
                       LASSO_FI <- model_results[[3]]
                       EN_FI <- model_results[[4]]
                       RG_FI <- model_results[[5]]
                       
                       write.csv(scores,file='static/tables/scores.csv',append=FALSE, row.names = FALSE)
                       write.csv(MLR_FI,file='static/tables/MLR_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(LASSO_FI,file='static/tables/LASSO_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(EN_FI,file='static/tables/EN_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(RG_FI,file='static/tables/RG_FI.csv',append=FALSE, row.names = FALSE)
                       
                       
                       #View(x)
                       waiter_hide()
                       
                       output$model_title1_Text <- renderText('Results Summary ')
                       output$model_title2_Text <- renderText('Models Built - ')
                       
                       output$resultstable <- renderDataTable(as.data.frame(c(scores['Algorithm'],scores['R2'],scores['RMSE'])))
                       
                       # observeEvent(req(scores),{
                       #   output$images <- renderUI(img(src= "static/plots/Plot_LASSO_coefs.jpg"))
                       #      
                       # })
                      
     
      })  
      # --------------------------------- Move to results tab --------------------------------------  
        
        
      })
      observeEvent(input$goToResults,{
        updateTabsetPanel(session = top_session, 
                          "tabs",
                          selected = 'RESULTS')
      })
    }
  )
}