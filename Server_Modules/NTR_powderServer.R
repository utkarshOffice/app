NTR_powderServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      uday_proxy <- DT::dataTableProxy('optimiser_table1_uday')
      manualinput <- reactiveVal(NULL)
      manual <- reactiveVal(NULL)
      importresults <- reactiveVal(NULL)
      optimise <- reactiveVal(NULL)
      optimise1 <- reactiveVal(NULL)
      optimise2 <- reactiveVal(NULL)
      x_uday_ntr <- data.frame(Models <- c("BD_Prediction_by_Model  =132.314+0.00874* Base_Factor *Base_Powder_Bulk_Density +8.812*Filler (Sulphate/ Salt as Balancing ingredient)+8.602*Post_Dosing_Ingredients_Majors_( >1% in FG other than Filler)+9.003*Post_Dosing_Ingredient_Minor_ (<1% in FG other than Filler)"
      ))  
      # function for parsing the equations
      predictors <- function(str){
        y <- strsplit(str,"=")[[1]][2]
        y <- gsub('[[:digit:]]+', '', gsub("[^[:alnum:]]", " ", y))
        y <- unique(strsplit(str_squish(y)," ")[[1]])
        return(y)
      }
      
      b_uday_ntr <- c("basefactor", "filler", "basepowderbulkdensity", "postdosingingredientsmajors", "postdosingingredientsminors")
      
      all_uday_ntr <- c("Base_Factor","Filler_Sulphate_Salt_as_Balancing_ingredient","Base_Powder_Bulk_Density","Post_Dosing_Ingredients_Majors","Post_Dosing_Ingredients_Minors")
      
      
      # model bank table
      output$models_uday_ntr <- renderDataTable({
        datatable(x_uday_ntr, colnames = c("Models"))
      } )
      
      # go to simulation
      observeEvent(input$commit_uday_ntr,{
        updateTabsetPanel(top_session, "tabs_uday_ntr", selected = "Simulation")
      })
      # go to visualization
      observeEvent(input$commit2_uday_ntr,{
        updateTabsetPanel(top_session, "tabs_uday_ntr", selected = "Visualization")
      })
      
      
      # Profiler renderings
      observeEvent(req(input$Base_Factor),{
        eqn1 <- "132.314+0.00874* Base_Factor *Base_Powder_Bulk_Density() +8.812*Filler_Sulphate_Salt_as_Balancing_ingredient() +8.602*Post_Dosing_Ingredients_Majors() +9.003*Post_Dosing_Ingredients_Minors()"
        
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
      
      
      colors <- c("red","black", "green","yellow","violet")
      
      
      #visualization page renderings
      observeEvent(req(input$datacall_uday_ntr),
                   {data_uday_ntr <- reactive(read_excel(input$datacall_uday_ntr$datapath))
                   #data <- reactive(data()[,-(which(colSums(data())==0))])
                   
                   updateSelectInput(session, "y_axis_ntr", choices = colnames(data_uday_ntr()), selected = colnames(data_uday_ntr())[2])
                   updateSelectInput(session, "x_axis_ntr", choices = colnames(data_uday_ntr()))
                   updateSelectInput(session, "x_line", choices = colnames(data_uday_ntr()))
                   updateSelectInput(session, "y_line", choices = colnames(data_uday_ntr()))
                   updateSelectInput(session, "hist_choice_uday_ntr", choices = colnames(data_uday_ntr()))
                   
                   output$simulationdata1 <- renderDataTable(data_uday_ntr())
                   output$scatterplot_uday_ntr <- renderPlot(
                     if(length(input$y_axis_ntr) == 1){
                       plot(x= data_uday_ntr()[[input$x_axis_ntr]], y = data_uday_ntr()[[input$y_axis_ntr]],
                            xlab = input$x_axis_ntr, ylab = input$y_axis_ntr, col = "blue")
                       abline(lm(data_uday_ntr()[[input$y_axis_ntr]]~data_uday_ntr()[[input$x_axis_ntr]]), col = "blue")
                     }
                     else{if(length(input$y_axis_ntr) > 4){
                       showModal(modalDialog("Maximum 4 selections are allowed."))
                     }else{
                       plot(x= data_uday_ntr()[[input$x_axis_ntr]], y = data_uday_ntr()[[input$y_axis_ntr[1]]],
                            xlab = input$x_axis_ntr, ylab = input$y_axis_ntr, col = "blue")
                       abline(lm(data_uday_ntr()[[input$y_axis_ntr[1]]]~data_uday_ntr()[[input$x_axis_ntr]]), col = "blue")
                       for(i in 2:length(input$y_axis_ntr)){
                         points(x= data_uday_ntr()[[input$x_axis_ntr]], y = data_uday_ntr()[[input$y_axis_ntr[i]]], col = colors[i])
                         abline(lm(data_uday_ntr()[[input$y_axis_ntr[i]]]~data_uday_ntr()[[input$x_axis_ntr]]), col = colors[i])
                       }
                     }
                     } )
                   output$ggsmooth_uday_ntr<- renderPlot({
                     if(input$smooth2_uday_ntr){
                       if(length(input$y_axis_ntr) == 1){
                         ggplot(data_uday_ntr(), aes(x= data_uday_ntr()[[input$x_axis_ntr]],y= data_uday_ntr()[[input$y_axis_ntr]])) +
                           geom_point(col="blue") + geom_smooth(method="lm", col="blue") +xlab(as.character(input$x_axis_ntr))+
                           ylab(as.character(input$y_axis_ntr))
                         #labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis_ntr[1]
                         second <- input$y_axis_ntr[2]
                         
                         ggplot(data_uday_ntr(), aes(x=data_uday_ntr()[[input$x_axis_ntr]])) +
                           geom_point(aes(y=data_uday_ntr()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_ntr()[[second]] * 1), col="red", shape = 18)+
                           geom_smooth(aes(y=data_uday_ntr()[[first]]), method="lm", col="blue") +
                           geom_smooth(aes(y=data_uday_ntr()[[second]] * 1), method="lm", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_ntr))
                       }
                     }else{
                       if(length(input$y_axis_ntr) == 1){
                         ggplot(data_uday_ntr(), aes(x= data_uday_ntr()[[input$x_axis_ntr]],y= data_uday_ntr()[[input$y_axis_ntr]])) +
                           geom_point() +xlab(as.character(input$x_axis_ntr))+
                           ylab(as.character(input$y_axis_ntr))
                         # labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else
                       {
                         first <- input$y_axis_ntr[1]
                         second <- input$y_axis_ntr[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data_uday_ntr(), aes(x=data_uday_ntr()[[input$x_axis_ntr]])) +
                           geom_point(aes(y=data_uday_ntr()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_ntr()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_ntr))
                       }
                     }
                   })
                   
                   output$ggscatter_uday_ntr <- renderPlot({
                     if(input$smooth_uday_ntr){
                       if(length(input$y_axis_ntr) == 1){
                         ggplot(data_uday_ntr(), aes(x= data_uday_ntr()[[input$x_axis_ntr]],y= data_uday_ntr()[[input$y_axis_ntr]])) +
                           geom_point(col="blue") + geom_line(col="blue") +labs(x = as.character(input$x_axis_ntr))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis_ntr[1]
                         second <- input$y_axis_ntr[2]
                         
                         ggplot(data_uday_ntr(), aes(x=data_uday_ntr()[[input$x_axis_ntr]])) +
                           geom_point(aes(y=data_uday_ntr()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_ntr()[[second]] * 1), col="red", shape = 18)+
                           geom_line(aes(y=data_uday_ntr()[[first]]), col="blue")+
                           geom_line(aes(y=data_uday_ntr()[[second]] * 1), col="red")+
                           # geom_smooth(aes(y=data()[[first]]), method="loess", col="blue") +
                           # geom_smooth(aes(y=data()[[second]] * 20), method="loess", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_ntr))
                       }
                     }else{
                       if(length(input$y_axis_ntr) == 1){
                         ggplot(data_uday_ntr(), aes(x= data_uday_ntr()[[input$x_axis_ntr]],y= data_uday_ntr()[[input$y_axis_ntr]])) +
                           geom_point() +labs(x = as.character(input$x_axis_ntr))
                       }else
                       {
                         first <- input$y_axis_ntr[1]
                         second <- input$y_axis_ntr[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data_uday_ntr(), aes(x=data_uday_ntr()[[input$x_axis_ntr]])) +
                           geom_point(aes(y=data_uday_ntr()[[first]]), col="blue")+
                           geom_point(aes(y=data_uday_ntr()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_ntr))
                       }
                     }
                   })
                   
                   output$hist_uday_ntr <- renderPlot({
                     ggplot(data_uday_ntr(), aes(x= data_uday_ntr()[[input$hist_choice_uday_ntr]])) +
                       geom_histogram(color="black", fill="lightblue")+
                       labs(x = as.character(input$hist_choice_uday_ntr))
                   })
                   })
      
      
      
      observeEvent(req(x_uday_ntr),{
        x1_uday_ntr <- reactiveValues()
        x2_uday_ntr <- reactiveValues()
        observe({
          y_uday_ntr <- reactive({
            munits <- c("%(w/w)","%(w/w)","kg/m3","%(w/w)","%(w/w)")
            values <- rep(1, length(all_uday_ntr))
            sqr <- do.call(rbind,data.frame(cbind(munits,values)))
            #sqr1 <- datatable(sqr, editable = T,colnames = c("Model Predictors","Measurement Units", "Enter Simulation Values"))
            colnames(sqr) <- all_uday_ntr
            rownames(sqr) <- c("Measurement Units (fractions)", "Enter Simulation Values")
            sqr
          })
          x1_uday_ntr$df <- y_uday_ntr()
        })
        
        
        
        output$simulation_input_uday_ntr <- renderDataTable({ 
          datatable(x1_uday_ntr$df, editable = T)
        })
        
        
        
        proxy_uday_ntr <- dataTableProxy("simulation_input_uday_ntr")
        
        
        
        observeEvent(input[["simulation_input_uday_ntr_cell_edit"]], {
          info <- input[["simulation_input_uday_ntr_cell_edit"]]
          
          i <- info$row
          j <- info$col
          v <- info$value
          
          if(!is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            x1_uday_ntr$df[i, j] <- DT::coerceValue(v,x1_uday_ntr$df[i, j])}
          
          rep <- x1_uday_ntr$df
          DT::replaceData(proxy_uday_ntr, rep, resetPaging = FALSE)
          #x1$df <<- editData(x1$df, info)
          
        })
        
        
        
        output$modeltable_uday_ntr <- renderDataTable(
          datatable(x1_uday_ntr$df)
        )
        
        
        observeEvent(input$simulate_uday_ntr,{
          
          eqn1 <- "132.314+0.00874* Base_Factor *Base_Powder_Bulk_Density +8.812*Filler_Sulphate_Salt_as_Balancing_ingredient+8.602*Post_Dosing_Ingredients_Majors+9.003*Post_Dosing_Ingredients_Minors"
          for(i in all_uday_ntr){
            eqn1 <- gsub(i, x1_uday_ntr$df[2,i], eqn1)
          }
          
          
          BD_Prediction_by_Model <- eval(parse(text = eqn1))
          
          output$simulation_result_uday_ntr <- renderUI({
            h3("Simulation Results")
          })
          
          
          tbl <- as.data.frame(BD_Prediction_by_Model)
          #tbl <- as.data.frame(BD_Prediction_by_Model)
          df <- x1_uday_ntr$df
          nrdata <- as.data.frame(tbl)
          nrdata1 <- as.data.frame(df)
          
          manualinput(df)
          manual(tbl)
          output$download1_uday_ntr <- downloadHandler(
            filename = function() { "Manual Entry Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
            }
          )
          
          output$result1_uday_ntr <- renderDataTable(
            {DT::datatable(tbl, rownames = FALSE) })
        })
        
        data_uday1_ntr <- reactive({
          req(input$datacall_uday_ntr)
          inFile <- input$datacall_uday_ntr
          if(is.null(inFile)) return(NULL)
          
          read_excel(input$datacall_uday_ntr$datapath) 
        })
        
        observeEvent(req(input$datacall_uday_ntr),{
          df<-read_excel(input$datacall_uday_ntr$datapath) 
          names(df) <- tolower(names(df))
          names(df) <- gsub(" ", "",names(df))
          names(df) <- gsub('\\(.*?\\)', '', names(df))
          names(df) <- gsub("\\(", "", names(df))
          names(df) <- gsub("\\)", "", names(df))
          names(df) <- gsub("\\/", "", names(df))
          names(df) <- gsub("\\-", "", names(df))
          
          
          
          
          
          observeEvent(data_uday1_ntr(),
                       {i <- b_uday_ntr[!is.element(b_uday_ntr,colnames(df))]
                       
                       if(length(i)>=1){
                         showModal(modalDialog(paste0(i, " is not present in imported data but required in equation.")))
                         
                       }
                       else NULL
                       })
          
          observeEvent(req(input$simulate2_uday_ntr,data_uday1_ntr()),{
            eqn1 <- "132.314+0.00874* basefactor *basepowderbulkdensity +8.812*filler+8.602*postdosingingredientsmajors+9.003*postdosingingredientsminors"
            
            
            for(i in b_uday_ntr){
              eqn1 <- gsub(i, paste0("df$",i), eqn1)
            }
            
            output$simulation_result_uday_ntr2 <- renderUI({
              h3("Simulation Results")
            })
            
            BD_Prediction_by_Model <- eval(parse(text = eqn1))
            
            nrdata <- as.data.frame(BD_Prediction_by_Model)
            importresults(nrdata)
            
            # nrdata1 <- as.data.frame(df)
            output$download2_uday_ntr <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
            
            output$modeltable2_uday_ntr <- renderDataTable({
              DT::datatable(as.data.frame(BD_Prediction_by_Model), rownames = FALSE)
            })
          })
          
          
        })
      })
      
      observeEvent(input$downloadresults_uday_ntr,{
        
        output$Download_Values_uday_ntr <- renderUI({
          ns <- session$ns
          downloadButton(ns("download_all_uday_ntr"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
        
        output$download_all_uday_ntr <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2), file)
          }
        )
      })
      
      
    }
  )
}


















