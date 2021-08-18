Facial_MoisturizerServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session){
      manualinput <- reactiveVal(NULL)
      manual <- reactiveVal(NULL)
      importresults <- reactiveVal(NULL)
      optimise <- reactiveVal(NULL)
      optimise1 <- reactiveVal(NULL)
      optimise2 <- reactiveVal(NULL)
      kayla_proxy <- DT::dataTableProxy('optimiser_table1_skincare_kayla')
      opt_kayla<-reactiveValues(tab_1=NULL)
      
      x_skincare_kayla <- data.frame(Models <- c("Viscosity 24hrs
  =Initial Emulsification Temperature * -572.345592886129 + Homogenization Speed * 731.123144906604 + First Homogenization Time Length * -3593.54500381997 + Second Homogenization Time Length * 2041.84871782967 +
  First Cooling Time Length * -1239.58504910869 + Second Cooling Time Length * -211.142485744822 + Discharge Temperature * -2217.51827004116 + Discharge Time Length * -1871.61581842917 + 301654.351744948 ",
  "Viscosity 2hrs = Initial Emulsification Temperature * 922.281708815879 + Homogenization Speed * 887.110890789025 + First Homogenization Time Length * 2042.97506366246 + Second Homogenization Time Length * -2234.74100427201 +
  First Cooling Time Length * 53.2437206619529 + Second Cooling Time Length * -350.472374047041 + Discharge Temperature * 1694.80362126778 + Discharge Time Length * -1390.91117995195 - 75916.4225373359 "))
      
      b_skincare_kayla <- c("Initial Emulsification Temperature",
                            "Homogenization Speed",
                            "First Homogenization Time Length",
                            "Second Homogenization Time Length",
                            "First Cooling Time Length",
                            "Second Cooling Time Length",
                            "Discharge Temperature",
                            "Discharge Time Length"
      )                          
      
      all_skincare_kayla <- c("Initial_Emulsification_Temperature"
        , "Homogenization_Speed"              
        , "First_Homogenization_Time_Length"  
        , "Second_Homogenization_Time_Length" 
        , "First_Cooling_Time_Length"         
        , "Second_Cooling_Time_Length"        
        , "Discharge_Temperature"             
        , "Discharge_Time_Length")
      
      
      # model bank table
      output$models_skincare_kayla <- renderDataTable({
        datatable(x_skincare_kayla, colnames = c("Models"))
      } )
      
      # go to simulation
      observeEvent(input$commit_skincare_kayla,{
        updateTabsetPanel(top_session, "tabs_skincare_kayla", selected = "Simulation")
      })
      # go to visualization
      observeEvent(input$commit2_skincare_kayla,{
        updateTabsetPanel(top_session, "tabs_skincare_kayla", selected = "Visualization")
      })

      colors <- c("red","black", "green","yellow","violet")
      
      # Profiler rendering
      observeEvent(req(input$Initial_Emulsification_Temperature),{
        eqn1 <- " Initial_Emulsification_Temperature * -572.345592886129 + Homogenization_Speed() * 731.123144906604 + First_Homogenization_Time_Length() * -3593.54500381997 + Second_Homogenization_Time_Length() * 2041.84871782967 +
  First_Cooling_Time_Length() * -1239.58504910869 + Second_Cooling_Time_Length() * -211.142485744822 + Discharge_Temperature() * -2217.51827004116 + Discharge_Time_Length() * -1871.61581842917 + 301654.351744948 "
        eqn2 <- "Initial_Emulsification_Temperature  * 922.281708815879 + Homogenization_Speed() * 887.110890789025 + First_Homogenization_Time_Length() * 2042.97506366246 + Second_Homogenization_Time_Length() * -2234.74100427201 +
  First_Cooling_Time_Length() * 53.2437206619529 + Second_Cooling_Time_Length() * -350.472374047041 + Discharge_Temperature() * 1694.80362126778 + Discharge_Time_Length() * -1390.91117995195 - 75916.4225373359 "
        
        Initial_Emulsification_Temperature <- 68:72      #round(seq(from  = 68, to = 72, length.out = 100),3)
        Homogenization_Speed <- reactive(input$Homogenization_Speed) 
        First_Homogenization_Time_Length  <- reactive(input$First_Homogenization_Time_Length)
        Second_Homogenization_Time_Length <- reactive(input$Second_Homogenization_Time_Length)
        First_Cooling_Time_Length <- reactive(input$First_Cooling_Time_Length)        
        Second_Cooling_Time_Length  <- reactive(input$Second_Cooling_Time_Length)    
        Discharge_Temperature  <- reactive(input$Discharge_Temperature)           
        Discharge_Time_Length <- reactive(input$Discharge_Time_Length)

        observeEvent(input$Initial_Emulsification_Temperature | input$Homogenization_Speed | input$First_Homogenization_Time_Length |
                       input$Second_Homogenization_Time_Length | input$First_Cooling_Time_Length | input$Second_Cooling_Time_Length |
                       input$Discharge_Temperature | input$Discharge_Time_Length,{
                         
                         result1 <- reactive(eval(parse(text = eqn1)))
                         result2 <- reactive(eval(parse(text = eqn2)))
                         
                         output$plot1 <- renderPlot({
                           result <- result1()
                           ggplot(data=data.frame(Initial_Emulsification_Temperature, result), aes(x=Initial_Emulsification_Temperature, y= result)) +
                             geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 15))+ xlab("Initial Emulsification Temperature")+
                             ylab("Viscosity 24hrs (mPas)")+
                             gghighlight(Initial_Emulsification_Temperature == input$Initial_Emulsification_Temperature)
                         })
                         
                         output$plot2 <- renderPlot({
                           result <- result2()
                           ggplot(data=data.frame(Initial_Emulsification_Temperature, result), aes(x=Initial_Emulsification_Temperature, y= result)) +
                             geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 15))+ xlab("Initial Emulsification Temperature")+
                             ylab("Viscosity 2hrs (mPas)")+
                             gghighlight(Initial_Emulsification_Temperature == input$Initial_Emulsification_Temperature)
                         })
                       })
      })
      
      
      
      #visualization page renderings
      observeEvent(req(input$datacall_skincare_kayla),
                   {data_skincare_kayla <- reactive(read_excel(input$datacall_skincare_kayla$datapath))
                   #data <- reactive(data()[,-(which(colSums(data())==0))])
                   #View(data_uday)
                   updateSelectInput(session, "y_axis_skincare_kayla", choices = colnames(data_skincare_kayla()), selected = colnames(data_skincare_kayla())[2])
                   updateSelectInput(session, "x_axis_skincare_kayla", choices = colnames(data_skincare_kayla()))
                   updateSelectInput(session, "x_line", choices = colnames(data_skincare_kayla()))
                   updateSelectInput(session, "y_line", choices = colnames(data_skincare_kayla()))
                   updateSelectInput(session, "hist_choice_skincare_kayla", choices = colnames(data_skincare_kayla()))
                   
                   output$simulationdata1 <- renderDataTable(data_skincare_kayla())
                   output$scatterplot_skincare_kayla <- renderPlot(
                     if(length(input$y_axis_skincare_kayla) == 1){
                       plot(x= data_skincare_kayla()[[input$x_axis_skincare_kayla]], y = data_skincare_kayla()[[input$y_axis_skincare_kayla]],
                            xlab = input$x_axis_skincare_kayla, ylab = input$y_axis_skincare_kayla, col = "blue")
                       abline(lm(data_skincare_kayla()[[input$y_axis_skincare_kayla]]~data_skincare_kayla()[[input$x_axis_skincare_kayla]]), col = "blue")
                     }
                     else{if(length(input$y_axis_skincare_kayla) > 4){
                       showModal(modalDialog("Maximum 4 selections are allowed."))
                     }else{
                       plot(x= data_skincare_kayla()[[input$x_axis_skincare_kayla]], y = data_skincare_kayla()[[input$y_axis_skincare_kayla[1]]],
                            xlab = input$x_axis_skincare_kayla, ylab = input$y_axis_skincare_kayla, col = "blue")
                       abline(lm(data_skincare_kayla()[[input$y_axis_skincare_kayla[1]]]~data_skincare_kayla()[[input$x_axis_skincare_kayla]]), col = "blue")
                       for(i in 2:length(input$y_axis_skincare_kayla)){
                         points(x= data_skincare_kayla()[[input$x_axis_skincare_kayla]], y = data_skincare_kayla()[[input$y_axis_skincare_kayla[i]]], col = colors[i])
                         abline(lm(data_skincare_kayla()[[input$y_axis_skincare_kayla[i]]]~data_skincare_kayla()[[input$x_axis_skincare_kayla]]), col = colors[i])
                       }
                     }
                     } )
                   output$ggsmooth_skincare_kayla<- renderPlot({
                     if(input$smooth2_skincare_kayla){
                       if(length(input$y_axis_skincare_kayla) == 1){
                         ggplot(data_skincare_kayla(), aes(x= data_skincare_kayla()[[input$x_axis_skincare_kayla]],y= data_skincare_kayla()[[input$y_axis_skincare_kayla]])) +
                           geom_point(col="blue") + geom_smooth(method="lm", col="blue") +xlab(as.character(input$x_axis_skincare_kayla))+
                           ylab(as.character(input$y_axis_skincare_kayla))
                         #labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis_skincare_kayla[1]
                         second <- input$y_axis_skincare_kayla[2]
                         
                         ggplot(data_skincare_kayla(), aes(x=data_skincare_kayla()[[input$x_axis_skincare_kayla]])) +
                           geom_point(aes(y=data_skincare_kayla()[[first]]), col="blue")+
                           geom_point(aes(y=data_skincare_kayla()[[second]] * 1), col="red", shape = 18)+
                           geom_smooth(aes(y=data_skincare_kayla()[[first]]), method="lm", col="blue") +
                           geom_smooth(aes(y=data_skincare_kayla()[[second]] * 1), method="lm", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_skincare_kayla))
                       }
                     }else{
                       if(length(input$y_axis_skincare_kayla) == 1){
                         ggplot(data_skincare_kayla(), aes(x= data_skincare_kayla()[[input$x_axis_skincare_kayla]],y= data_skincare_kayla()[[input$y_axis_skincare_kayla]])) +
                           geom_point() +xlab(as.character(input$x_axis_skincare_kayla))+
                           ylab(as.character(input$y_axis_skincare_kayla))
                         # labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else
                       {
                         first <- input$y_axis_skincare_kayla[1]
                         second <- input$y_axis_skincare_kayla[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data_skincare_kayla(), aes(x=data_skincare_kayla()[[input$x_axis_skincare_kayla]])) +
                           geom_point(aes(y=data_skincare_kayla()[[first]]), col="blue")+
                           geom_point(aes(y=data_skincare_kayla()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_skincare_kayla))
                       }
                     }
                   })
                   
                   output$ggscatter_skincare_kayla <- renderPlot({
                     if(input$smooth_skincare_kayla){
                       if(length(input$y_axis_skincare_kayla) == 1){
                         ggplot(data_skincare_kayla(), aes(x= data_skincare_kayla()[[input$x_axis_skincare_kayla]],y= data_skincare_kayla()[[input$y_axis_skincare_kayla]])) +
                           geom_point(col="blue") + geom_line(col="blue") +labs(x = as.character(input$x_axis_skincare_kayla))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis_skincare_kayla[1]
                         second <- input$y_axis_skincare_kayla[2]
                         
                         ggplot(data_skincare_kayla(), aes(x=data_skincare_kayla()[[input$x_axis_skincare_kayla]])) +
                           geom_point(aes(y=data_skincare_kayla()[[first]]), col="blue")+
                           geom_point(aes(y=data_skincare_kayla()[[second]] * 1), col="red", shape = 18)+
                           geom_line(aes(y=data_skincare_kayla()[[first]]), col="blue")+
                           geom_line(aes(y=data_skincare_kayla()[[second]] * 1), col="red")+
                           # geom_smooth(aes(y=data()[[first]]), method="loess", col="blue") +
                           # geom_smooth(aes(y=data()[[second]] * 20), method="loess", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_skincare_kayla))
                       }
                     }else{
                       if(length(input$y_axis_skincare_kayla) == 1){
                         ggplot(data_skincare_kayla(), aes(x= data_skincare_kayla()[[input$x_axis_skincare_kayla]],y= data_skincare_kayla()[[input$y_axis_skincare_kayla]])) +
                           geom_point() +labs(x = as.character(input$x_axis_skincare_kayla))
                       }else
                       {
                         first <- input$y_axis_skincare_kayla[1]
                         second <- input$y_axis_skincare_kayla[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data_skincare_kayla(), aes(x=data_skincare_kayla()[[input$x_axis_skincare_kayla]])) +
                           geom_point(aes(y=data_skincare_kayla()[[first]]), col="blue")+
                           geom_point(aes(y=data_skincare_kayla()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis_skincare_kayla))
                       }
                     }
                   })
                   
                   output$hist_skincare_kayla <- renderPlot({
                     ggplot(data_skincare_kayla(), aes(x= data_skincare_kayla()[[input$hist_choice_skincare_kayla]])) +
                       geom_histogram(color="black", fill="lightblue")+
                       labs(x = as.character(input$hist_choice_skincare_kayla))
                   })
                   })
      #advisory table
      output$advice_skincare_kayla <- renderDataTable({
        Advisory_table <- data.frame(Ingredients = c("Initial Emulsification Temperature",
                                                     "Homogenization Speed",
                                                     "First Homogenization Time Length",
                                                     "Second Homogenization Time Length",
                                                     "First Cooling Time Length",
                                                     "Second Cooling Time Length",
                                                     "Discharge Temperature",
                                                     "Discharge Time Length"
        ),
                                     Lower_Level = c(68,33,10,3,14,10,36,3),
                                     Upper_Level = c(72,50,20,13,43,52,42,26))
        datatable(Advisory_table)
      })
      
      
      observeEvent(req(x_skincare_kayla),{
        x1_skincare_kayla <- reactiveValues()
        x2_skincare_kayla <- reactiveValues()
        observe({
          y_skincare_kayla <- reactive({
        
            values <- c(69,34,11,4,15,11,37,4)
            sqr <- data.frame(t(values))
            #sqr1 <- datatable(sqr, editable = T,colnames = c("Model Predictors","Measurement Units", "Enter Simulation Values"))
            colnames(sqr) <- b_skincare_kayla
            rownames(sqr) <- c("Enter Simulation Values")
            sqr
            
          })
          x1_skincare_kayla$df <- y_skincare_kayla()
        })
        
      
        output$simulation_input_skincare_kayla <- renderDataTable({ 
          
          #datatable(x1_skincare_kayla$df, editable = T)
          
          datatable(x1_skincare_kayla$df, editable = T) %>%
            formatStyle(
              "Initial Emulsification Temperature",
              color = styleInterval(c(68, 72), c('red', 'black', 'red')))%>%
            formatStyle(
              "Homogenization Speed",
              color = styleInterval(c(33, 50), c('red', 'black', 'red')))%>%
            formatStyle(
              "First Homogenization Time Length",
              color = styleInterval(c(10, 20), c('red', 'black', 'red')))%>%
            formatStyle(
              "Second Homogenization Time Length",
              color = styleInterval(c(3, 13), c('red', 'black', 'red')))%>%
            formatStyle(
              "First Cooling Time Length",
              color = styleInterval(c(14, 43), c('red', 'black', 'red')))%>%
            formatStyle(
              "Second Cooling Time Length",
              color = styleInterval(c(10, 52), c('red', 'black', 'red')))%>%
            formatStyle(
              "Discharge Temperature",
              color = styleInterval(c(36, 42), c('red', 'black', 'red')))%>%
            formatStyle(
              "Discharge Time Length",
              color = styleInterval(c(3,26), c('red', 'black', 'red')))
        })
        
        proxy_skincare_kayla <- dataTableProxy("simulation_input_skincare_kayla")
        
        observeEvent(input[["simulation_input_skincare_kayla_cell_edit"]], {
          info <- input[["simulation_input_skincare_kayla_cell_edit"]]
          
          i <- info$row
          j <- info$col
          v <- info$value
          
          if(!is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            x1_skincare_kayla$df[i, j] <- DT::coerceValue(v,x1_skincare_kayla$df[i, j])}
          
          rep <- x1_skincare_kayla$df
          DT::replaceData(proxy_skincare_kayla, rep, resetPaging = FALSE)
          #x1$df <<- editData(x1$df, info)
          
        })

        output$modeltable_skincare_kayla <- renderDataTable(
          datatable(x1_skincare_kayla$df)
        )
        
        
        observeEvent(input$simulate_skincare_kayla,{
          x2 <- x1_skincare_kayla$df
          # View(x2)
          colnames(x2) <- all_skincare_kayla
          
          eqn1 <- " Initial_Emulsification_Temperature * -572.345592886129 + Homogenization_Speed * 731.123144906604 + First_Homogenization_Time_Length * -3593.54500381997 + Second_Homogenization_Time_Length * 2041.84871782967 +
  First_Cooling_Time_Length * -1239.58504910869 + Second_Cooling_Time_Length * -211.142485744822 + Discharge_Temperature * -2217.51827004116 + Discharge_Time_Length * -1871.61581842917 + 301654.351744948 "
          eqn2 <- "Initial_Emulsification_Temperature  * 922.281708815879 + Homogenization_Speed * 887.110890789025 + First_Homogenization_Time_Length * 2042.97506366246 + Second_Homogenization_Time_Length * -2234.74100427201 +
  First_Cooling_Time_Length * 53.2437206619529 + Second_Cooling_Time_Length * -350.472374047041 + Discharge_Temperature * 1694.80362126778 + Discharge_Time_Length * -1390.91117995195 - 75916.4225373359 "
          
          for(i in all_skincare_kayla){
            eqn1 <- gsub(i, x2[1,i], eqn1)
            eqn2 <- gsub(i, x2[1,i], eqn2)
          }
          
          Viscosity_24hrs <- eval(parse(text = eqn1))
          Viscosity_2hrs <- eval(parse(text = eqn2))
          
          output$kayla_simulation <- renderUI(
            h3("Simulation Result")
          )
          
          View(Viscosity_24hrs)
          View(Viscosity_2hrs)
          tbl <- data.frame(Viscosity_24hrs,Viscosity_2hrs)
          
          df <- x1_skincare_kayla$df
          nrdata <- as.data.frame(tbl)
          nrdata1 <- as.data.frame(df)
          manualinput(df)
          manual(tbl)
          output$download1_skincare_kayla <- downloadHandler(
            filename = function() { "Manual Entry Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
            }
          )
          
          output$result1_skincare_kayla <- renderDataTable(
            {DT::datatable(tbl, rownames = FALSE) })
        })
        
        data_skincare_kayla <- reactive({
          req(input$datacall_skincare_kayla)
          inFile <- input$datacall_skincare_kayla
          if(is.null(inFile)) return(NULL)
          
          read_excel(input$datacall_skincare_kayla$datapath)
        })
        
        
        data_skincare_kayla1 <- reactive({
          req(input$datacall_skincare_kayla)
          inFile <- input$datacall_skincare_kayla
          if(is.null(inFile)) return(NULL)
          
          xlfile <- read_excel(input$datacall_skincare_kayla$datapath)
          colnames(xlfile) <- gsub(" ","_",colnames(xlfile))
          # colnames(xlfile) <- tolower(colnames(xlfile))
          # colnames(xlfile) <- gsub('\\(.*?\\)', '', colnames(xlfile))
          # 
          # colnames(xlfile) <- gsub('[[:digit:]]+', '', colnames(xlfile))
          # colnames(xlfile) <- gsub(" ", "",colnames(xlfile))
          # colnames(xlfile) <- gsub("\\(", "", colnames(xlfile))
          # colnames(xlfile)<- gsub("\\)", "", colnames(xlfile))
          # colnames(xlfile) <- gsub("\\/", "", colnames(xlfile))
          # colnames(xlfile) <- gsub("\\-", "", colnames(xlfile))
          xlfile
          
        })
        
        # observeEvent(req(input$datacall_skincare_kayla),{
        #   View(data_skincare_kayla1()) })
        
        
        
        
        # observeEvent(data_skincare_kayla(),
        #              {i <- b_skincare_kayla[!is.element(b_skincare_kayla,colnames(df))]
        #              
        #              if(length(i)>=1){
        #                showModal(modalDialog(paste0(i, " is not present in imported data but required in equation.")))
        #                
        #              }
        #              else NULL
        #              })
        
        observeEvent(req(input$simulate2_skincare_kayla,data_skincare_kayla1()),{
          
          eqn1 <- " Initial_Emulsification_Temperature * -572.345592886129 + Homogenization_Speed * 731.123144906604 + First_Homogenization_Time_Length * -3593.54500381997 + Second_Homogenization_Time_Length * 2041.84871782967 +
  First_Cooling_Time_Length * -1239.58504910869 + Second_Cooling_Time_Length * -211.142485744822 + Discharge_Temperature * -2217.51827004116 + Discharge_Time_Length * -1871.61581842917 + 301654.351744948 "
          eqn2 <- " Initial_Emulsification_Temperature  * 922.281708815879 + Homogenization_Speed * 887.110890789025 + First_Homogenization_Time_Length * 2042.97506366246 + Second_Homogenization_Time_Length * -2234.74100427201 +
  First_Cooling_Time_Length * 53.2437206619529 + Second_Cooling_Time_Length * -350.472374047041 + Discharge_Temperature * 1694.80362126778 + Discharge_Time_Length * -1390.91117995195 - 75916.4225373359 "

          df<- data_skincare_kayla1()
          
          for(i in all_skincare_kayla){
            eqn1 <- gsub(i, paste0("df$",i), eqn1)
            eqn2 <- gsub(i, paste0("df$",i), eqn2)
          }
          
          output$kayla_simulation2 <- renderUI(
            h3("Simulation Result")
          )
          
          Viscosity_24hrs <- eval(parse(text = eqn1))
          Viscosity_2hrs <- eval(parse(text = eqn2))
          
          nrdata <- data.frame(Viscosity_24hrs,Viscosity_2hrs)
          importresults(nrdata)
          
          output$download2_skincare_kayla <- downloadHandler(
            filename = function() { "Import Data Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Import Results" = nrdata), file)
            }
          )
          output$modeltable2_skincare_kayla <- renderDataTable({
            DT::datatable(data.frame(Viscosity_24hrs,Viscosity_2hrs), rownames = FALSE)
          })
        })
        
        
      })
      
      #optimisation renderings for kayla
      
      # observeEvent(req(x_skincare_kayla),{
      #   
      #   predictors_in_model_skincare_kayla<-c("2 Homo I PT100 (oC) Mean_[67,72]",
      #                                         "3 Homo II Main recycle pipe flowmeter (kg/h) Mean_[486,955]",
      #                                         "3 Homo II Silverson (Hz) Mean_[33,50]","3 Homo II Length_[3,13]",
      #                                         "6 Cooling II Length_[10,50]", "7 Discharge PT100 (oC) Mean_[40,41]")
      #   min_vector<- c(67,486,33,3,10,40)
      #   max_vector<- c(72,955,50,13,50,41)
      #   zero_vector<-rep(1,length(predictors_in_model_skincare_kayla))
      #   coef_data <- data.frame(cbind(predictors_in_model_skincare_kayla,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
      #   opt_kayla$tab_1 <- coef_data
      #   opt_kayla$tab_1[[3]] <- as.numeric(opt_kayla$tab_1[[3]])
      #   opt_kayla$tab_1[[4]] <- as.numeric(opt_kayla$tab_1[[4]])
      #   
      #   
      #   #table 1 - objective function table
      #   output$optimiser_table1_skincare_kayla<-renderDataTable({
      #     DT::datatable(opt_kayla$tab_1,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)") )
      #   })
      #   
      #   observeEvent(input$optimiser_table1_skincare_kayla_cell_edit,{
      #     info <- input$optimiser_table1_skincare_kayla_cell_edit
      #     
      #     i <- info$row
      #     j <- info$col
      #     v <- info$value
      #     # View(j) #correct col number
      #     # View(v)
      #     if(j >= 2 && !is.na(v) && !is.na(as.numeric(v))){
      #       v <- as.numeric(v)
      #       if(j==2 || ( j==3 && opt_kayla$tab_1[i, j+1] > v) || (j==4 && opt_kayla$tab_1[i, j-1] < v )){
      #         opt_kayla$tab_1[i,j] <- DT::coerceValue(v,opt_kayla$tab_1[i, j])
      #       }
      #     }
      #     rep <- opt_kayla$tab_1
      #     DT::replaceData(kayla_proxy, rep, resetPaging = FALSE)
      #     
      #   })
      #   
      #   observeEvent(input$run_optimiser_skincare_kayla,{
      #     
      #     predictors_in_model_skincare_kayla <- c("2 Homo I PT100 (oC) Mean","3 Homo II Main recycle pipe flowmeter (kg/h) Mean","3 Homo II Silverson (Hz) Mean","3 Homo II Length","6 Cooling II Length", "7 Discharge PT100 (oC) Mean")
      #     reg_coeff_in_model_skincare_kayla <- c(-757.860416353529,21.943031882607,313.651192927793,-1148.72296443766,-153.575970145004, 7534.34231013295)
      #     if(input$inequality_selection_skincare_kayla=="less than or equal to"){
      #       constr<-'<='
      #     }
      #     else if(input$inequality_selection_skincare_kayla=="greater than or equal to"){
      #       constr<-'>='
      #     }
      #     else{
      #       constr<-'='
      #     }
      #     # View(constr)#works
      #     
      #     target<-input$numeric_input_skincare_kayla
      #     number.predictors<-length(predictors_in_model_skincare_kayla)
      #     # View(opt$tab_1)
      #     low.lims<-opt_kayla$tab_1[[3]]
      #     upp.lims<-opt_kayla$tab_1[[4]]
      #     objective.in<-opt_kayla$tab_1[[2]]
      #     objective.in<-as.numeric(objective.in)
      #     low.lims<-as.numeric(low.lims)
      #     upp.lims<-as.numeric(upp.lims)
      #     
      #     obj.type<-input$radio_button_skincare_kayla
      #     # View(objective.in)#works
      #     # View(obj.type)
      #     intercept <- as.numeric(-212433.522884953)
      #     
      #     lps.model <- make.lp(0,number.predictors)
      #     add.constraint(lps.model,reg_coeff_in_model_skincare_kayla,constr,target-intercept)
      #     
      #     # Bounds for variables
      #     set.bounds(lps.model,lower=low.lims)
      #     set.bounds(lps.model,upper=upp.lims)
      #     # View(low.lims)#works
      #     # View(target)#works
      #     # View(nrow(low.lims))
      #     # View(obj.type) #works
      #     #View(objective.in)#works
      #     #View(opt_kayla$tab_1)#works
      #     # View(number.predictors)#works gets 6 as output
      #     
      #     # Objective function
      #     lp.control(lps.model,sense=obj.type) # min or max
      #     # View(objective.in) #getting output
      #     
      #     set.objfn(lps.model,objective.in) # coefficients
      #     # View(upp.lims) 
      #     
      #     # Apply solver
      #     solution.status <- solve(lps.model)
      #     # View(solution.status)
      #     if(solution.status!=0){
      #       showModal(modalDialog("Linear Optimisation could not find a solution for the given inputs. Please change the inputs and re-run."))
      #     }
      #     
      #     #unpacking
      #     solution.values <- get.primal.solution(lps.model)
      #     # View(solution.values)
      #     objective.function.value <- as.numeric(solution.values[1])
      #     fitted.response <- as.numeric(solution.values[2])+intercept
      #     solution.values <- as.numeric(solution.values[3:length(solution.values)])
      #     # View(solution.values)
      #     # View(objective.function.value)
      #     #View(fitted.response)
      #     
      #     results<-data.frame(Value=fitted.response)
      #     # colnames(results)<-""
      #     row.names(results)<-"Viscosity Free Sample 4 CPS"
      #     
      #     #downloading
      #     downresults <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Viscosity Free Sample 4 CPS"), Predicted_or_Optimal_Value= fitted.response)
      #     downdf<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("2 Homo I PT100 (oC) Mean","3 Homo II Main recycle pipe flowmeter (kg/h) Mean","3 Homo II Silverson (Hz) Mean","3 Homo II Length","6 Cooling II Length", "7 Discharge PT100 (oC) Mean"),
      #                        Predicted_or_Optimal_Value=solution.values)
      #     downopt <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective func. Value"), Predicted_or_Optimal_Value = objective.function.value)
      #     
      #     final1 <- rbind(downresults,downdf,downopt)
      #     optimise(final1)
      #     output$download3_skincare_kayla <- downloadHandler(
      #       filename = function() { "Optimisation for Viscosity Free Sample 4 CPS .xlsx"},
      #       content = function(file) {
      #         write_xlsx(list("Optimisation Result" = final1), file)
      #       }
      #     )
      #     
      #     # optmiser table2
      #     output$optimiser_table2_skincare_kayla<-renderDataTable({
      #       DT::datatable(results) })
      #     
      #     # optmiser table3
      #     output$optimiser_table3_skincare_kayla<- renderDataTable({
      #       df<-data.frame(Predictors=c("2 Homo I PT100 (oC) Mean","3 Homo II Main recycle pipe flowmeter (kg/h) Mean","3 Homo II Silverson (Hz) Mean","3 Homo II Length","6 Cooling II Length", "7 Discharge PT100 (oC) Mean"),
      #                      Value=solution.values)
      #       DT::datatable(df)
      #       # DT::datatable(df)
      #     })
      #     
      #     #optimiser textoutput
      #     output$value_results_skincare_kayla<- renderUI({
      #       # ns <- session$ns
      #       p(paste0("The objective value resulting from the optimisation is : "),objective.function.value)
      #     })
      #     
      #   })#observeEvent run optimiser ends
      #   
      #   # reset button
      #   observeEvent(input$reset_skincare_kayla,{
      #     updateSelectInput(session,"inequality_selection_skincare_kayla",selected = "less than or equal to")
      #     updateNumericInput(session,"numeric_input_skincare_kayla",value = 56000)
      #     updateRadioButtons(session,"radio_button_skincare_kayla",selected = "min")
      #     predictors_in_model_skincare_kayla<-c("2 Homo I PT100 (oC) Mean_[67,72]",
      #                                           "3 Homo II Main recycle pipe flowmeter (kg/h) Mean_[486,955]",
      #                                           "3 Homo II Silverson (Hz) Mean_[33,50]","3 Homo II Length_[3,13]",
      #                                           "6 Cooling II Length_[10,50]", "7 Discharge PT100 (oC) Mean_[40,41]")
      #     min_vector<- c(67,486,33,3,10,40)
      #     max_vector<- c(72,955,50,13,50,41)
      #     zero_vector<-rep(1,length(predictors_in_model_skincare_kayla))
      #     coef_data <- data.frame(cbind(predictors_in_model_skincare_kayla,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
      #     opt_kayla$tab_1 <- coef_data
      #   })
      #   
      #   
      # })#observeEvent kayla end
      
      observeEvent(input$downloadresults_skincare_kayla,{
        
        output$Download_Values_skincare_kayla <- renderUI({
          ns <- session$ns
          downloadButton(ns("download_all_skincare_kayla"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        
        nrdata <- as.data.frame(manual())
        nrdata1 <- as.data.frame(manualinput())
        nrdata2 <- as.data.frame(importresults())
        nrdata3 <- as.data.frame(optimise())
        
        output$download_all_skincare_kayla <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2, "Optimisation for Viscosity Free Sample 4 CPS" = nrdata3), file)
          }
        )
      })
    }
  )
}