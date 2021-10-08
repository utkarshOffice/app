modellerServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      source_python("python_modeller.py")
      unlink("static/tables/*")
      
      #--------------------------------Get Materials-----------------------------------
      observeEvent(req(input$dataset),{
        data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))
        
        
        material_names <- get_materials(data())
        #View(material_names)
        material_names_list <- strsplit(material_names, " +")                  
        #View(material_names_list)
        updateSelectInput(session,'material',choices = material_names_list, selected = material_names_list[0])
      })
      
      #--------------------------------Get Predictors ----------------------------------
      observeEvent(req(input$dataset),{
        data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))
        predictor_v <- get_predictors(data())
        predictor_v_list <- strsplit((predictor_v), " +")
        #View(predictor_v_list)
        
        updateSelectInput(session,'predictor_vars',choices = predictor_v_list, selected = predictor_v_list[0])
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
                           h3("Building Model ...")
                         ) # use a spinner
                       )
                       model_results <- run_model(data_sent,input$predictor_vars,'SS',input$material,FALSE)
                       
                       scores <- model_results[[1]]
                       MLR_FI <- model_results[[2]]
                       LASSO_FI <- model_results[[3]]
                       
                       write.csv(scores,file='static/tables/scores.csv',append=FALSE, row.names = FALSE)
                       write.csv(MLR_FI,file='static/tables/MLR_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(LASSO_FI,file='static/tables/LASSO_FI.csv',append=FALSE, row.names = FALSE)
                       
                       #View(x)
                       waiter_hide()
                       
                       output$resultstable <- renderDataTable(as.data.frame(c(scores['Method'],scores['R2'],scores['RMSE'])))
                       
                       # observeEvent(req(scores),{
                       #   output$images <- renderUI(img(src= "static/plots/Plot_LASSO_coefs.jpg"))
                       #      
                       # })
                      
     
      })      
      })
    }
  )
}