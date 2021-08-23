SD_powderServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      manualinput <- reactiveVal(NULL)
      manual <- reactiveVal(NULL)
      importresults <- reactiveVal(NULL)
      optimise <- reactiveVal(NULL)
      optimise1 <- reactiveVal(NULL)
      optimise2 <- reactiveVal(NULL)
      uday_proxy <- DT::dataTableProxy('optimiser_table1_uday')

      x_uday_sd <- data.frame(Models <- c("BD_Prediction_by_Model = 9.3 + 0.104 * Base_Factor * Filler (Sulphate/ Salt as Balancing ingredient) + 0.905 * Base_Powder_Bulk_Density + 4.99 * Post_Dosing_Ingredients_Majors_( >1% in FG other than Filler) + 10.5 * Post_Dosing_Ingredient_Minor_ (<1% in FG other than Filler)"
      ))  
      # function for parsing the equations
      predictors <- function(str){
        y <- strsplit(str,"=")[[1]][2]
        y <- gsub('[[:digit:]]+', '', gsub("[^[:alnum:]]", " ", y))
        y <- unique(strsplit(str_squish(y)," ")[[1]])
        return(y)
      }
      
      b_uday_sd <- c("basefactor", "filler", "basepowderbulkdensity", "postdosingingredientsmajors", "postdosingingredientsminors")
      
      all_uday_sd <- c("Base_Factor","Filler_Sulphate_Salt_as_Balancing_ingredient","Base_Powder_Bulk_Density","Post_Dosing_Ingredients_Majors","Post_Dosing_Ingredients_Minors")
      
      
      # model bank table
      output$models_uday_sd <- renderDataTable({
        datatable(x_uday_sd, colnames = c("Models"))
      })
      
      # go to simulation
      observeEvent(input$commit_uday_sd,{
        updateTabsetPanel(top_session, "tabs_uday_sd", selected = "Simulation")
      })
      # go to visualization
      observeEvent(input$commit2_uday_sd,{
        updateTabsetPanel(top_session, "tabs_uday_sd", selected = "Visualization")
      })
      
      colors <- c("red","black", "green","yellow","violet")
      
      # Profiler renderings
      observeEvent(req(input$Base_Factor),{
        eqn1 <- "9.3 + 0.104 * Base_Factor * Filler_Sulphate_Salt_as_Balancing_ingredient() + 0.905 * Base_Powder_Bulk_Density() + 4.99 * Post_Dosing_Ingredients_Majors() + 10.5 * Post_Dosing_Ingredients_Minors()"
        
        Base_Factor <- seq(from  = 1, to = 100, length.out = 100)
        Filler_Sulphate_Salt_as_Balancing_ingredient <- reactive(input$Filler_Sulphate_Salt_as_Balancing_ingredient)
        Base_Powder_Bulk_Density <- reactive(input$Base_Powder_Bulk_Density)
        Post_Dosing_Ingredients_Majors <- reactive(input$Post_Dosing_Ingredients_Majors)
        Post_Dosing_Ingredients_Minors <- reactive(input$Post_Dosing_Ingredients_Minors)
        
        observeEvent(input$Base_Factor | input$Filler_Sulphate_Salt_as_Balancing_ingredient | 
                     input$Base_Powder_Bulk_Density | input$Post_Dosing_Ingredients_Majors | 
                     input$Post_Dosing_Ingredients_Minors
          ,{
          BD_Prediction_by_Model <- reactive(eval(parse(text = eqn1)))
          
          output$plot2 <- renderPlot({
            BD_Prediction_by_Model <- BD_Prediction_by_Model()
            ggplot(data=data.frame(Base_Factor, BD_Prediction_by_Model), aes(x=Base_Factor, y= BD_Prediction_by_Model)) +
              geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
              gghighlight(Base_Factor == input$Base_Factor)
          })
        })
        
      })
      
      
      
      #visualization page renderings
      observeEvent(req(input$datacall_uday_sd),
                   {data_uday_sd <- reactive(read_excel(input$datacall_uday_sd$datapath))
                   #data <- reactive(data()[,-(which(colSums(data())==0))])
                   #View(data_uday)
                   updateSelectInput(session, "y_axis_sd", choices = colnames(data_uday_sd()), selected = colnames(data_uday_sd())[2])
                   updateSelectInput(session, "x_axis_sd", choices = colnames(data_uday_sd()))
                   updateSelectInput(session, "x_line", choices = colnames(data_uday_sd()))
                   updateSelectInput(session, "y_line", choices = colnames(data_uday_sd()))
                   updateSelectInput(session, "hist_choice_uday_sd", choices = colnames(data_uday_sd()))
                   
                   output$simulationdata1 <- renderDataTable(data_uday_sd())
                   output$scatterplot_uday_sd <- renderPlot(
                     if(length(input$y_axis_sd) == 1){
                       plot(x= data_uday_sd()[[input$x_axis_sd]], y = data_uday_sd()[[input$y_axis_sd]],
                            xlab = input$x_axis_sd, ylab = input$y_axis_sd, col = "blue")
                       abline(lm(data_uday_sd()[[input$y_axis_sd]]~data_uday_sd()[[input$x_axis_sd]]), col = "blue")
                     }
                     else{if(length(input$y_axis_sd) > 4){
                       showModal(modalDialog("Maximum 4 selections are allowed."))
                     }else{
                       plot(x= data_uday_sd()[[input$x_axis_sd]], y = data_uday_sd()[[input$y_axis_sd[1]]],
                            xlab = input$x_axis_sd, ylab = input$y_axis_sd, col = "blue")
                       abline(lm(data_uday_sd()[[input$y_axis_sd[1]]]~data_uday_sd()[[input$x_axis_sd]]), col = "blue")
                       for(i in 2:length(input$y_axis_sd)){
                         points(x= data_uday_sd()[[input$x_axis_sd]], y = data_uday_sd()[[input$y_axis_sd[i]]], col = colors[i])
                         abline(lm(data_uday_sd()[[input$y_axis_sd[i]]]~data_uday_sd()[[input$x_axis_sd]]), col = colors[i])
                       }
                     }
                     } )
                   output$ggsmooth_uday_sd<- renderPlot({
                     if(input$smooth2_uday_sd){
                       if(length(input$y_axis_sd) == 1){
                         ggplot(data_uday_sd(), aes(x= data_uday_sd()[[input$x_axis_sd]],y= data_uday_sd()[[input$y_axis_sd]])) +
                           geom_point(col="blue") + geom_smooth(method="lm", col="blue") +xlab(as.character(input$x_axis_sd))+
                           ylab(as.character(input$y_axis_sd))
                         #labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis_sd[1]
                         second <- input$y_axis_sd[2]
                         
                         ggplot(data_uday_sd(), aes(x=data_uday_sd()[[input$x_axis_sd]])) +
                           geom_point(aes(y=data_uday_sd()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_sd()[[second]] * 1), col="red", shape = 18)+
                           geom_smooth(aes(y=data_uday_sd()[[first]]), method="lm", col="blue") +
                           geom_smooth(aes(y=data_uday_sd()[[second]] * 1), method="lm", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_sd))
                       }
                     }else{
                       if(length(input$y_axis_sd) == 1){
                         ggplot(data_uday_sd(), aes(x= data_uday_sd()[[input$x_axis_sd]],y= data_uday_sd()[[input$y_axis_sd]])) +
                           geom_point() +xlab(as.character(input$x_axis_sd))+
                           ylab(as.character(input$y_axis_sd))
                         # labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else
                       {
                         first <- input$y_axis_sd[1]
                         second <- input$y_axis_sd[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data_uday_sd(), aes(x=data_uday_sd()[[input$x_axis_sd]])) +
                           geom_point(aes(y=data_uday_sd()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_sd()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_sd))
                       }
                     }
                   })
                   
                   output$ggscatter_uday_sd <- renderPlot({
                     if(input$smooth_uday_sd){
                       if(length(input$y_axis_sd) == 1){
                         ggplot(data_uday_sd(), aes(x= data_uday_sd()[[input$x_axis_sd]],y= data_uday_sd()[[input$y_axis_sd]])) +
                           geom_point(col="blue") + geom_line(col="blue") +labs(x = as.character(input$x_axis_sd))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis_sd[1]
                         second <- input$y_axis_sd[2]
                         
                         ggplot(data_uday_sd(), aes(x=data_uday_sd()[[input$x_axis_sd]])) +
                           geom_point(aes(y=data_uday_sd()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_sd()[[second]] * 1), col="red", shape = 18)+
                           geom_line(aes(y=data_uday_sd()[[first]]), col="blue")+
                           geom_line(aes(y=data_uday_sd()[[second]] * 1), col="red")+
                           # geom_smooth(aes(y=data()[[first]]), method="loess", col="blue") +
                           # geom_smooth(aes(y=data()[[second]] * 20), method="loess", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_sd))
                       }
                     }else{
                       if(length(input$y_axis_sd) == 1){
                         ggplot(data_uday_sd(), aes(x= data_uday_sd()[[input$x_axis_sd]],y= data_uday_sd()[[input$y_axis_sd]])) +
                           geom_point() +labs(x = as.character(input$x_axis_sd))
                       }else
                       {
                         first <- input$y_axis_sd[1]
                         second <- input$y_axis_sd[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data_uday_sd(), aes(x=data_uday_sd()[[input$x_axis_sd]])) +
                           geom_point(aes(y=data_uday_sd()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_sd()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_sd))
                       }
                     }
                   })
                   
                   output$hist_uday_sd <- renderPlot({
                     ggplot(data_uday_sd(), aes(x= data_uday_sd()[[input$hist_choice_uday_sd]])) +
                       geom_histogram(color="black", fill="lightblue")+
                       labs(x = as.character(input$hist_choice_uday_sd))
                   })
                   })
      
      
      
      observeEvent(req(x_uday_sd),{
        x1_uday_sd <- reactiveValues()
        x2_uday_sd <- reactiveValues()
        observe({
          y_uday_sd <- reactive({
            munits <- c("%(w/w)","%(w/w)","kg/m3","%(w/w)","%(w/w)")
            values <- rep(1, length(all_uday_sd))
            sqr <- do.call(rbind,data.frame(cbind(munits,values)))
            #sqr1 <- datatable(sqr, editable = T,colnames = c("Model Predictors","Measurement Units", "Enter Simulation Values"))
            colnames(sqr) <- all_uday_sd
            rownames(sqr) <- c("Measurement Units (fractions)", "Enter Simulation Values")
            sqr
          })
          x1_uday_sd$df <- y_uday_sd()
        })
        
        
        
        output$simulation_input_uday_sd <- renderDataTable({ 
          datatable(x1_uday_sd$df, editable = T)
        })
        
        
        
        proxy_uday_sd <- dataTableProxy("simulation_input_uday_sd")
        
        
        
        observeEvent(input[["simulation_input_uday_sd_cell_edit"]], {
          info <- input[["simulation_input_uday_sd_cell_edit"]]
          
          i <- info$row
          j <- info$col
          v <- info$value
          
          if(!is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            x1_uday_sd$df[i, j] <- DT::coerceValue(v,x1_uday_sd$df[i, j])}
          
          rep <- x1_uday_sd$df
          DT::replaceData(proxy_uday_sd, rep, resetPaging = FALSE)
          #x1$df <<- editData(x1$df, info)
          
        })
        
        
        
        output$modeltable_uday_sd <- renderDataTable(
          datatable(x1_uday_sd$df)
        )
        
        # b_uday_sd <- c("BaseFactor", "FillerSulphateSaltasBalancingingredient", "BasePowderBulkDensity", "PostDosingIngredientsMajors", "PostDosingIngredientsMinors")
        
        observeEvent(input$simulate_uday_sd,{
          eqn1 <- "9.3 + 0.104 * Base_Factor * Filler_Sulphate_Salt_as_Balancing_ingredient + 0.905 * Base_Powder_Bulk_Density + 4.99 * Post_Dosing_Ingredients_Majors + 10.5 * Post_Dosing_Ingredients_Minors"
          for(i in all_uday_sd){
            eqn1 <- gsub(i, x1_uday_sd$df[2,i], eqn1)
          }
          
          
          BD_Prediction_by_Model <- eval(parse(text = eqn1))
          
          output$simulation_result_uday_sd <- renderUI({
            h3("Simulation Results")
          })
          
          
          tbl <- as.data.frame(BD_Prediction_by_Model)
          df <- x1_uday_sd$df
          nrdata <- as.data.frame(tbl)
          nrdata1 <- as.data.frame(df)
          
          manualinput(df)
          manual(tbl)
          output$download1_uday_sd <- downloadHandler(
            filename = function() { "Manual Entry Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
            }
          )
          
          output$result1_uday_sd <- renderDataTable(
            { tbl })
        }) # simulate uday sd ends here
        
        data_uday1_sd <- reactive({
          req(input$datacall_uday_sd)
          inFile <- input$datacall_uday_sd
          if(is.null(inFile)) return(NULL)
          
          read_excel(input$datacall_uday_sd$datapath) 
        })
        
        
        observeEvent(req(input$datacall_uday_sd),{
          df<-read_excel(input$datacall_uday_sd$datapath) 
          names(df) <- tolower(names(df))
          names(df) <- gsub(" ", "",names(df))
          names(df) <- gsub('\\(.*?\\)', '', names(df))
          
          # names(df) <- gsub("\\(", "", names(df))
          # names(df) <- gsub("\\)", "", names(df))
          names(df) <- gsub("\\/", "", names(df))
          names(df) <- gsub("\\-", "", names(df))
          
          #View(df)
          
          
          
          # observeEvent(data_uday1_sd(),
          #              {i <- b_uday_sd[!is.element(b_uday_sd,colnames(df))]
          # 
          #              if(length(i)>=1){
          #                showModal(modalDialog(paste0(i, " is not present in imported data but required in equation.")))
          # 
          #              }
          #              else NULL
          #              })
          
          
          
          observeEvent(req(input$simulate2_uday_sd,data_uday1_sd()),{
            eqn1 <- "9.3 + 0.104 * basefactor * filler + 0.905 * basepowderbulkdensity + 4.99 * postdosingingredientsmajors + 10.5 * postdosingingredientsminors"
            
            
            for(i in b_uday_sd){
              eqn1 <- gsub(i, paste0("df$",i), eqn1)
            }
            
            output$simulation_result_uday_sd2 <- renderUI({
              h3("Simulation Results")
            })
            
            BD_Prediction_by_Model <- eval(parse(text = eqn1))
            
            nrdata <- as.data.frame(BD_Prediction_by_Model)
            importresults(nrdata)
            
           # nrdata1 <- as.data.frame(df)
            output$download2_uday_sd <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
            
            
            output$modeltable2_uday_sd <- renderDataTable({
              DT::datatable(as.data.frame(BD_Prediction_by_Model), rownames =FALSE)
            })
          })
          
        })
      })
      observeEvent(input$downloadresults_uday_sd,{
        
        output$Download_Values_uday_sd <- renderUI({
          ns <- session$ns
          downloadButton(ns("download_all_uday_sd"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        #View(manual())
        # View(manualinput())
        #View(importresults())
        
        nrdata <- as.data.frame(manual())
        nrdata1 <- as.data.frame(manualinput())
        nrdata2 <- as.data.frame(importresults())
        #nrdata3 <- as.data.frame(optimise())
        #nrdata4 <- as.data.frame(optimise1())
       # nrdata5 <- as.data.frame(optimise2())
        
        
        #View(nrdata)
        #View(nrdata1)
        #View(nrdata2)
        
        output$download_all_uday_sd <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2), file)
          }
        )
      }) #download end
    
      # non linear optimisation for Udays SD 
      # observeEvent(req(x_uday_sd),{
      #   predictor_names_sd <- c("Base_Factor","Filler_Sulphate_Salt_as_Balancing_ingredient",
      #                           "Base_Powder_Bulk_Density","Post_Dosing_Ingredients_Majors",
      #                           "Post_Dosing_Ingredients_Minors")
      #   zero_vector<-rep(1,length(predictor_names_sd))
      #   min_vector <- c(0.29,0.13,0.07,0.001,0.17,0,0.17)
      #   max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
      #   coef_data2 <- data.frame(cbind(predictor_names_torque,zero_vector,min_vector,max_vector))
      #   opt$tab_2 <- coef_data2
      #   opt$tab_2[[3]]<- as.numeric(opt$tab_2[[3]])
      #   opt$tab_2[[4]]<- as.numeric(opt$tab_2[[4]])
      #   
      #   #table 1
      #   output$optimiser_table1_uday_non_linear <- renderDataTable({
      #     DT::datatable(opt$tab_2,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
      #   })
      #   
      #   #cell edit
      #   observeEvent(input$optimiser_table1_uday_non_linear_cell_edit,{
      #     info <- input$optimiser_table1_uday_non_linear_cell_edit
      #     i <- info$row
      #     j <- info$col
      #     v <- info$value
      #     if(j >= 2 && !is.na(v) && !is.na(as.numeric(v))){
      #       v <- as.numeric(v)
      #       if(j==2 || ( j==3 && opt$tab_2[i, j+1] > v) || (j==4 && opt$tab_2[i, j-1] < v )){
      #         opt$tab_2[i,j] <<- DT::coerceValue(v,opt$tab_2[i, j])
      #       }
      #     }
      #     rep <- opt$tab_2
      #     DT::replaceData(uday_proxy_non_linear, rep, resetPaging = FALSE)
      #   })
      #   
      # })
      
      
      
      }
  )}