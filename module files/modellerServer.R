modellerServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      source_python("python_modeller.py")
      unlink("./www/tables/*")
      essential_features <- c("Material_Name", "Sealing_Pressure_N_cm2",
                              "Sealing_Force_N" , "Sealing_Time_ms",
                              "Sealing_Temperature_C" ,"Seal_Strength_N_15mm", "Validation")
      
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
      
      #--------------------------------Check Data Format-------------------------------

  
      flag <- reactive(NULL)

    flag <- eventReactive( c(req(input$dataset),input$polyFlag), {


         tryCatch({

           if(input$polyFlag == FALSE)
           {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))}
           if(input$polyFlag == TRUE)
           {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_3'))}

           missing_features <- essential_features[!is.element(essential_features,colnames(data()))]
           missing_features_list <- as.character(strsplit(missing_features, " +"))
           missing_features_str <- ""
           for(ele in missing_features_list)
           {
             missing_features_str <- paste(ele, missing_features_str, sep = ', \n')
           }

              if(length(missing_features)>=1){
                showModal(modalDialog(HTML(paste0("<b> Some Features are missing! </b> <br/>",
                                                  length(missing_features), " essential features are missing in imported data.
                                                  Kindly reupload the dataset containing them.<br/>",
                                                  "<b>The missing features are: </b> <br/>",
                                                  missing_features_str))))
              }
           else{
             flag <- reactive(input$dataset)
           }
         },
         error=function(e) {
           showModal(modalDialog(HTML(paste0("<b> File Upload Error! </b> <br/>
                                              Please upload the correct file again.
                                  Download sample dataset for reference. <br/><b>  More Details: </b> <br/> ", e))))
         }
        )
      })
    
  
      #flag <- reactive(input$dataset)
    

      #--------------------------------Get Materials-----------------------------------
      
      #observeEvent(input$polyFlag,
                   #{
      observeEvent(req(flag()),{

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
    #})
      
      #--------------------------------Get Predictors ----------------------------------
      
      #observeEvent(input$polyFlag,
                   #{
      observeEvent(req(flag()),{
        
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
    #})
      #-----------------------Check if Validation column is empty--------------------------
      
        observeEvent(c(input$valFlag,input$polyFlag),
                  {
                     observeEvent(req(flag()),{
                       
                       if(input$polyFlag == FALSE)
                       {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))
                       #View(data)
                       }
                       if(input$polyFlag == TRUE)
                       {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_3'))}
                       
                       data_df <- as.data.frame(data(),row.names = NULL)
                       
                       #View(length(unique(data_df$Validation)))
                       
                       if((length(unique(data_df$Validation))==1)&(input$valFlag==TRUE))
                       {
                         output$nullValNote <- renderUI({
                           wellPanel(
                             HTML(paste(em("NOTE: The data uploaded does not contain any datapoints for validation & hence all points will be used for training.")))
                           )
                         })
                       }
                       else{
                         output$nullValNote <- renderUI({
                           NULL
                         })
                       }
                     })
                     
                   })
      
      #---------------------Display Note on Validation for Paper---------------------------
      
      observeEvent(c(input$valFlag,input$polyFlag),
              {
                     
                if((input$valFlag==TRUE) & (input$polyFlag==FALSE))
                {
                    output$paperValNote <- renderUI({
                      wellPanel(
                        HTML(paste(em("NOTE: The DOE data for paper models does not support validation, which may result in unexpected scores.")))
                      )
                    })
                }
              else{
                output$paperValNote <- renderUI({
                  NULL
              })
              }
      })
      
      
      #--------------------------------BUILD MODEL---------------------------------------
  
      #observeEvent(input$polyFlag,once = TRUE,
                   #{
        observeEvent(req(flag()),once = TRUE,{
        
                observeEvent(input$build,ignoreInit = TRUE,

                  {
                       
                        if(input$polyFlag == FALSE)
                        {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_2'))
                        #View(data)
                        }
                        if(input$polyFlag == TRUE)
                        {data <- reactive(read_excel(input$dataset$datapath, sheet='stack_3'))}
                       
                       unlink("./www/tables/*")
                        
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
                       
                       write.csv(scores,file='./www/tables/scores.csv',append=FALSE, row.names = FALSE)
                       write.csv(MLR_FI,file='./www/tables/MLR_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(LASSO_FI,file='./www/tables/LASSO_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(EN_FI,file='./www/tables/EN_FI.csv',append=FALSE, row.names = FALSE)
                       write.csv(RG_FI,file='./www/tables/RG_FI.csv',append=FALSE, row.names = FALSE)
                       #View(4)
                       
                       waiter_hide()
                       
                       output$model_title1_Text <- renderText('Results Summary ')
                       output$model_title2_Text <- renderText('Models Built - ')
                       
                       if((length(unique(data_sent_df$Validation))!=1)&(input$valFlag==TRUE)){
                         scoresTable <- as.data.frame(c(scores['Algorithm'],scores['Train_R2'],scores['Validation_R2'],scores['Train_RMSE'],scores['Validation_RMSE']))
                       }
                       if((length(unique(data_sent_df$Validation))==1)|(input$valFlag==FALSE)){
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
     
      #})  
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