chillrollServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      # initializing some variables which will be used later
      santosh_proxy <- DT::dataTableProxy('optimiser_table1_chill')
      opt<-reactiveValues(tab_1=NULL) 
      manualinput_santosh <- reactiveVal(NULL)
      manual_santosh <- reactiveVal(NULL)
      importresults_santosh <- reactiveVal(NULL)
      optimise_santosh <- reactiveVal(NULL)
      optimise1_santosh <- reactiveVal(NULL)
      optimise2_santosh <- reactiveVal(NULL)
      weight_one <- reactiveVal(NULL)
      weight_two <- reactiveVal(NULL)
      
      # ---------------------------------------------------- MODEL & DATA IMPORT --------------------------------------------------------
      
      # making a dataframe for our two models calculating Flake Final Temp (Model 1 & 2) 
      x_santosh <- data.frame(Models_santosh <- c("Flake Final Temp (Model 1) = (Cooling_seg_fraction)*(-45.5179701273954) + (Film_thickness)*(18.8956339477401) + ((Film_thickness)^2)*(6.67710528547635) + (Roll_Speed)*(7.21746209125386) + ((Roll_Speed)^2)*(1.27090332399397) + 20.3887470216993",
                                            "Flake Final Temp (Model 2) = (Cooling_seg_fraction)*(-48.3757602771176) + (T_ambient)^2*(-0.0000431916784640458) + (T_flake feed)*(0.339086502336415) + (T_chilled water)*0.529509608669907 + (Film thickness)*(18.6709859995675) + (Film thickness)^2*(6.55116697694696) + (DEFI Free roll length)^2*(0.13197883988453) + (Roll Speed)*7.21191328015338 + (Roll Speed)^2*1.26186094993258 - 18.9553266806959"
                                            ))
      
      # lists for variable names
      b_santosh <- c("Film_thickness", "Cooling_seg_fraction", "T_flake_feed" , "T_ambient","T_chilled_water" ,"DEFI_Free_roll_length", "Roll_Speed")
      all_vars_santosh <-c("Film_thickness", "Cooling_seg_fraction", "T_flake_feed" , "T_ambient","T_chilled_water" ,"DEFI_Free_roll_length", "Roll_Speed")
      
      # lists for excel file column names
      c_santosh <- c("filmthick", "coolseg", "tfeed" , "tambient","tsteel" ,"rollldefifree", "rollspeed")
      d_santosh <- c("filmthick", "coolseg", "tfeed" , "tambient","tsteel" ,"rollldefifree", "rollspeed")
     
      
      # model bank table output
      output$models_santosh <- renderDataTable({
        datatable(x_santosh, colnames = c("Models"))
      } )
      
      
      # button action to go to simulation
      observeEvent(input$commit_santosh,{
        updateTabsetPanel(top_session, "tabs_santosh", selected = "Simulation")
      })
      
      # button action to go to visualization
      observeEvent(input$commit2_santosh,{
        updateTabsetPanel(top_session, "tabs_santosh", selected = "Visualization")
      })
      
      # rendering advisory tables - predictors and their limits
      output$advice_santosh <- renderDataTable({
        Advisory_table <- data.frame(Ingredients = c("Film thickness","Cooling_seg_fraction",
                                                     "T_flake feed",
                                                     "T_ambient","T_chilled_water",
                                                     "DEFI Free roll length", "Roll Speed"),
                                     Lower_Level = c(0.5, 0.25, 100, 15, -5, 0.001, 1.0),
                                     Upper_Level = c(2.0, 0.875, 130, 40, 15, 0.1, 4.0))
        datatable(Advisory_table)
      })
      
      
      colors <- c("red","black", "green","yellow","violet")
      
      # ------------------------------------------------ SIMULATION - PROFILER ------------------------------------------------------
      
      # profiler renderings
      observeEvent(req(input$profiler_Film_thickness),{
        
        eqn1 <- "(Cooling_seg_fraction())*(-45.5179701273954) + (Film_thickness)*(18.8956339477401) + ((Film_thickness)^2)*(6.67710528547635) + Roll_Speed()*7.21746209125386 + (Roll_Speed()^2)*(1.27090332399397) + 20.3887470216993"
        eqn2 <- "(Cooling_seg_fraction())*(-48.3757602771176) + (T_ambient()**2)*(-0.0000431916784640458) + T_flake_feed()*0.339086502336415 + T_chilled_water()*0.529509608669907 + (Film_thickness)*(18.6709859995675) + ((Film_thickness)**2)*(6.55116697694696) + (DEFI_Free_roll_length()**2)*(0.13197883988453) + Roll_Speed()*7.21191328015338 + (Roll_Speed()**2)*1.26186094993258 - (18.9553266806959)"
        
        
        Film_thickness <- round(seq(from  = 0.5, to = 2.0, length.out = 25),3)
        Cooling_seg_fraction <- reactive(input$profiler_Cooling_seg_fraction)
        T_ambient <- reactive(input$profiler_T_ambient)
        T_flake_feed <- reactive(input$profiler_T_flake_feed)
        DEFI_Free_roll_length <- reactive(input$profiler_DEFI_Free_roll_length)
        Roll_Speed <- reactive(input$profiler_Roll_Speed)
        T_chilled_water <- reactive(input$profiler_T_chilled_water)
        
        # # making sure all variables are changed to numeric
        # Film_thickness <- as.numeric(Film_thickness)
        # Cooling_seg_fraction <- as.numeric(Cooling_seg_fraction())
        # T_ambient <- as.numeric(T_ambient())
        # T_flake_feed <- as.numeric(T_flake_feed())
        # DEFI_Free_roll_length <- as.numeric(DEFI_Free_roll_length())
        # Roll_Speed <- as.numeric(Roll_Speed())
        # T_chilled_water <- as.numeric(T_chilled_water())
        
        observeEvent(input$profiler_Film_thickness | input$profiler_T_ambient | input$profiler_T_flake_feed | input$profiler_Cooling_seg_fraction |input$profiler_DEFI_Free_roll_length | input$profiler_T_chilled_water | input$profiler_Cooling_seg_fraction,
            {
              
            Final_Flake_Temp_M1 <- reactive(eval(parse(text = eqn1)))
            Final_Flake_Temp_M2 <- reactive(eval(parse(text = eqn2)))
            
            output$plot1 <- renderPlot({
              Final_Flake_Temp_1 <- Final_Flake_Temp_M1()
              ggplot(data=data.frame(Film_thickness, Final_Flake_Temp_1), aes(x=Film_thickness, y= Final_Flake_Temp_1)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(Film_thickness == input$profiler_Film_thickness)
            })
            
            output$plot2 <- renderPlot({
              Final_Flake_Temp_2 <- Final_Flake_Temp_M2()
              ggplot(data=data.frame(Film_thickness, Final_Flake_Temp_2), aes(x=Film_thickness, y= Final_Flake_Temp_2)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(Film_thickness == input$profiler_Film_thickness)
            })
    
            })
      })
      
      
      # --------------------------------------------- VISUALIZATION ------------------------------------------------------
      
      #visualization page renderings
      observeEvent(req(input$datacall_santosh),
                   {
                     santosh_data_slurry1 <- reactive({
                       req(input$datacall_santosh)
                       inFile <- input$datacall_santosh
                       if(is.null(inFile)) return(NULL)

                       xlfile <- read_excel(input$datacall_santosh$datapath , sheet='Sensor')
                       colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
                       colnames(xlfile) <- gsub(" ","",colnames(xlfile))
                       colnames(xlfile) <- tolower(colnames(xlfile))
                       xlfile

                     })
                     df1 <- santosh_data_slurry1()
                     
                     santosh_data_slurry2 <- reactive({
                       req(input$datacall_santosh)
                       inFile <- input$datacall_santosh
                       if(is.null(inFile)) return(NULL)

                       # reading only range in which predictors exist
                       xlfile <- read_excel(input$datacall_santosh$datapath, sheet='Sensor', range='Sensor!D1:J502')

                       xlfile <- round(xlfile,3)

                     })
                     df2 <- santosh_data_slurry2()
                     
                     finaldf <- as.data.frame(df2)

                     output$simulationdata_santosh <- renderDataTable({
                       santosh_data_slurry2()
                     })
                     
                     updateSelectInput(session, "y_axis_bd", choices = colnames(finaldf), selected = colnames(finaldf)[1])
                     updateSelectInput(session, "x_axis_bd", choices = colnames(finaldf), selected = colnames(finaldf)[5])
                     updateSelectInput(session, "x_line", choices = colnames(finaldf))
                     updateSelectInput(session, "y_line", choices = colnames(finaldf))
                     updateSelectInput(session, "hist_choice_santosh", choices = colnames(finaldf))

                     output$simulationdata1_santosh <- renderDataTable(finaldf)
                     output$scatterplot_santosh <- renderPlot(
                       if(length(input$y_axis_bd) == 1){
                         plot(x= finaldf[[input$x_axis_bd]], y = finaldf[[input$y_axis_bd]],
                              xlab = input$x_axis_bd, ylab = input$y_axis_bd, col = "blue")
                         abline(lm(finaldf[[input$y_axis_bd]]~finaldf[[input$x_axis_bd]]), col = "blue")
                       }
                       else{if(length(input$y_axis_bd) > 4){
                         showModal(modalDialog("Maximum 4 selections are allowed."))
                       }else{
                         plot(x= finaldf[[input$x_axis_bd]], y = finaldf[[input$y_axis_bd[1]]],
                              xlab = input$x_axis_bd, ylab = input$y_axis_bd, col = "blue")
                         abline(lm(finaldf[[input$y_axis_bd[1]]]~finaldf[[input$x_axis_bd]]), col = "blue")
                         for(i in 2:length(input$y_axis_bd)){
                           points(x= finaldf[[input$x_axis_bd]], y = finaldf[[input$y_axis_bd[i]]], col = colors[i])
                           abline(lm(finaldf[[input$y_axis_bd[i]]]~finaldf[[input$x_axis_bd]]), col = colors[i])
                         }
                       }
                       } )
                     output$ggsmooth_santosh<- renderPlot({
                       if(input$smooth2_santosh){
                         if(length(input$y_axis_bd) == 1){
                           ggplot(finaldf, aes(x= finaldf[[input$x_axis_bd]],y= finaldf[[input$y_axis_bd]])) +
                             geom_point(col="blue") + geom_smooth(method="lm", col="blue") +xlab(as.character(input$x_axis_bd))+
                             ylab(as.character(input$y_axis_bd))
                         }else{
                           first <- input$y_axis_bd[1]
                           second <- input$y_axis_bd[2]

                           ggplot(finaldf, aes(x=finaldf[[input$x_axis_bd]])) +
                             geom_point(aes(y=finaldf[[first]]), col="blue")+
                             geom_point(aes(y=finaldf[[second]] * 1), col="red", shape = 18)+
                             geom_smooth(aes(y=finaldf[[first]]), method="lm", col="blue") +
                             geom_smooth(aes(y=finaldf[[second]] * 1), method="lm", col="red") +
                             scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                             theme(
                               axis.title.y.left=element_text(color="blue"),
                               axis.text.y.left=element_text(color="blue"),
                               axis.title.y.right=element_text(color="red"),
                               axis.text.y.right=element_text(color="red")
                             )+labs(x = as.character(input$x_axis_bd))
                         }
                       }else{
                         if(length(input$y_axis_bd) == 1){
                           ggplot(data_santosh_bd(), aes(x= finaldf[[input$x_axis_bd]],y= finaldf[[input$y_axis_bd]])) +
                             geom_point() +xlab(as.character(input$x_axis_bd))+
                             ylab(as.character(input$y_axis_bd))
                         }else
                         {
                           first <- input$y_axis_bd[1]
                           second <- input$y_axis_bd[2]

                           ggplot(finaldf, aes(x=finaldf[[input$x_axis_bd]])) +
                             geom_point(aes(y=finaldf[[first]]), col="blue")+
                             geom_point(aes(y=finaldf[[second]] * 1), col="red", shape = 18)+
                             scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                             theme(
                               axis.title.y.left=element_text(color="blue"),
                               axis.text.y.left=element_text(color="blue"),
                               axis.title.y.right=element_text(color="red"),
                               axis.text.y.right=element_text(color="red")
                             )+labs(x = as.character(input$x_axis_bd))
                         }
                       }
                     })

                     output$ggscatter_santosh <- renderPlot({
                       if(input$smooth_santosh){
                         if(length(input$y_axis_bd) == 1){
                           ggplot(finaldf, aes(x= finaldf[[input$x_axis_bd]],y= finaldf[[input$y_axis_bd]])) +
                             geom_point(col="blue") + geom_line(col="blue") +labs(x = as.character(input$x_axis_bd))
                         }else{
                           first <- input$y_axis_bd[1]
                           second <- input$y_axis_bd[2]

                           ggplot(finaldf, aes(x=finaldf[[input$x_axis_bd]])) +
                             geom_point(aes(y=finaldf[[first]]), col="blue")+
                             geom_point(aes(y=finaldf[[second]] * 1), col="red", shape = 18)+
                             geom_line(aes(y=finaldf[[first]]), col="blue")+
                             geom_line(aes(y=finaldf[[second]] * 1), col="red")+
                           
                             scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                             theme(
                               axis.title.y.left=element_text(color="blue"),
                               axis.text.y.left=element_text(color="blue"),
                               axis.title.y.right=element_text(color="red"),
                               axis.text.y.right=element_text(color="red")
                             )+labs(x = as.character(input$x_axis_bd))
                         }
                       }else{
                         if(length(input$y_axis_bd) == 1){
                           ggplot(finaldf, aes(x= finaldf[[input$x_axis_bd]],y= data_santosh_bd()[[input$y_axis_bd]])) +
                             geom_point() +labs(x = as.character(input$x_axis_bd))
                         }else
                         {
                           first <- input$y_axis_bd[1]
                           second <- input$y_axis_bd[2]

                           ggplot(finaldf, aes(x=finaldf[[input$x_axis_bd]])) +
                             geom_point(aes(y=finaldf[[first]]), col="blue")+
                             geom_point(aes(y=finaldf[[second]] * 1), col="red", shape = 18)+
                             scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                             theme(
                               axis.title.y.left=element_text(color="blue"),
                               axis.text.y.left=element_text(color="blue"),
                               axis.title.y.right=element_text(color="red"),
                               axis.text.y.right=element_text(color="red")
                             )+labs(x = as.character(input$x_axis_bd))
                         }
                       }
                     })

                     output$multi_lines_graph_santosh <- renderPlotly({
                       if(length(input$y_axis_bd) == 1){
                         fig <- plot_ly( x = ~data_santosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd]],mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = as.character(input$y_axis)) )
                         fig
                       }else if(length(input$y_axis_bd) == 2){
                         fig <- plot_ly(data_santosh_bd(), x = ~data_santosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       }
                       else if(length(input$y_axis_bd) == 3){
                         fig <- plot_ly(data_santosh_bd(), x = ~data_santosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 4){
                         fig <- plot_ly(data_santosh_bd(), x = ~data_santosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 5){
                         fig <- plot_ly(data_santosh_bd(), x = ~data_santosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[5]]], name = input$y_axis_bd[5], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 6){
                         fig <- plot_ly(data_santosh_bd(), x = ~data_santosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[5]]], name = input$y_axis_bd[5], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_santosh_bd()[[input$y_axis_bd[6]]], name = input$y_axis_bd[6], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       }
                     })
                     output$hist_santosh <- renderPlot({
                       ggplot(finaldf, aes(x= finaldf[[input$hist_choice_santosh]])) +
                         geom_histogram(color="black", fill="lightblue")+
                         labs(x = as.character(input$hist_choice_santosh))
                     })
                   })

      
      # --------------------------------------------- SIMULATION - MANUAL ENTRY --------------------------------------------------
      
      observeEvent(req(x_santosh),{
        x1_santosh <- reactiveValues()
        observe({
          y_santosh <- reactive({
            values <- c(1.0, 0.75, 110, 30, 7.5, 0.01, 2.0)
            sqr <- data.frame(t(values))
            colnames(sqr) <- all_vars_santosh
            rownames(sqr) <- c("Enter Simulation Values")
            sqr
          })
          x1_santosh$df <- y_santosh()
        })
        
        
        
        output$simulation_input_santosh <- renderDataTable({ 
          
          datatable(x1_santosh$df, editable = T) %>%
            formatStyle(
              "Film_thickness",
              color = styleInterval(c(0.5, 2.0), c('red', 'black', 'red')))%>%
            formatStyle(
              "Cooling_seg_fraction",
              color = styleInterval(c(0.5, 0.875), c('red', 'black', 'red')))%>%
            formatStyle(
              "T_flake_feed",
              color = styleInterval(c(100, 130), c('red', 'black', 'red')))%>%
            formatStyle(
              "T_ambient",
              color = styleInterval(c(15, 40), c('red', 'black', 'red')))%>%
            formatStyle(
              "T_chilled_water",
              color = styleInterval(c(-5, 15), c('red', 'black', 'red')))%>%
            formatStyle(
              "DEFI_Free_roll_length",
              color = styleInterval(c(0.001, 0.1), c('red', 'black', 'red')))%>%
            formatStyle(
              "Roll_Speed",
              color = styleInterval(c(1.0, 4.0), c('red', 'black', 'red')))
        })
        
        
        proxy_santosh <- dataTableProxy("simulation_input_santosh")
        
        
        # to capture edited values in our saved table - x1_santosh
        
        observeEvent(input[["simulation_input_santosh_cell_edit"]], {
          info <- input[["simulation_input_santosh_cell_edit"]]
          
          i <- info$row
          j <- info$col
          v <- info$value
          
          if(!is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            x1_santosh$df[i, j] <- DT::coerceValue(v,x1_santosh$df[i, j])}
          
          rep <- x1_santosh$df
          DT::replaceData(proxy_santosh, rep, resetPaging = FALSE)
          
        })
        
        
        output$modeltable_santosh <- renderDataTable(
          datatable(x1_santosh$df)
        )
        
        
        observeEvent(input$simulate_santosh,{
          
          eqn1 <- "(Cooling_seg_fraction)*(-45.5179701273954) + (Film_thickness)*(18.8956339477401) + ((Film_thickness)^2)*(6.67710528547635) + (Roll_Speed)*(7.21746209125386) + ((Roll_Speed)^2)*(1.27090332399397) + 20.3887470216993"
          eqn2 <- "(Cooling_seg_fraction)*(-48.3757602771176) + ((T_ambient)**2)*(-0.0000431916784640458) + (T_flake_feed)*(0.339086502336415) + (T_chilled_water)*(0.529509608669907) + (Film_thickness)*(18.6709859995675) + ((Film_thickness)**2)*(6.55116697694696) + ((DEFI_Free_roll_length)**2)*(0.13197883988453) + (Roll_Speed)*(7.21191328015338) + ((Roll_Speed)**2)*(1.26186094993258) - (18.9553266806959)"
          
          df <- as.data.frame(x1_santosh$df)
  
          # prefixing 'df' to column names
          for(i in b_santosh){
            eqn1 <- gsub(i, df[1,i], eqn1)
            eqn2 <- gsub(i, df[1,i], eqn2)
          }
          

          Final_Flake_Temp_M1 <- round(eval(parse(text = eqn1)),3)
          Final_Flake_Temp_M2 <- round(eval(parse(text = eqn2)),3)

          # result table
          tbl <- cbind(Final_Flake_Temp_M1, Final_Flake_Temp_M2)
          
          # input table
          df1 <- x1_santosh$df
          
          manualinput_santosh(df1)
          manual_santosh(tbl)
          
          nrdata <- as.data.frame(tbl)
          nrdata1 <- as.data.frame(df)
          
          output$download1_santosh <- downloadHandler(
            filename = function() { "Manual Entry Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
            }
          )
          output$heading1_santosh <- renderUI({
            h3("Simulation Results")
            
          })
          
          output$result1_santosh <- renderDataTable(
            { tbl })
        })
        
        
        # ---------------------------------------- SIMULATION - IMPORTED DATA -------------------------------------------------
        
        santosh_data_slurry <- reactive({
          req(input$datacall_santosh)
          inFile <- input$datacall_santosh
          if(is.null(inFile)) return(NULL)
        
          xlfile <- read_excel(input$datacall_santosh$datapath, sheet='Sensor')
          colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
          colnames(xlfile) <- gsub(" ","",colnames(xlfile))
          colnames(xlfile) <- tolower(colnames(xlfile))
          xlfile
          
        })
        
        # checking whether data contains all required columns
        observeEvent(santosh_data_slurry(),
                     {i <- d_santosh[!is.element(d_santosh,colnames(santosh_data_slurry()))]
                     if(length(i)>=1){
                       showModal(modalDialog(paste0(i, " is not present in imported data but required in equation. 0 is placed in the place of missing variables to render the respective coefficients ineffective.")))
                     }
                     })

        observeEvent(req(input$datacall_santosh), {
          

          df1 <- santosh_data_slurry()
      
          observeEvent(req(input$simulate2_santosh),{
            eqn1 <- "(coolseg)*(-45.5179701273954) + (filmthick)*(18.8956339477401) + ((filmthick)^2)*(6.67710528547635) + (rollspeed)*(7.21746209125386) + ((rollspeed)^2)*(1.27090332399397) + 20.3887470216993"
            eqn2 <- "(coolseg)*(-48.3757602771176) + ((tambient)**2)*(-0.0000431916784640458) + (tfeed)*(0.339086502336415) + (tsteel)*(0.529509608669907) + (filmthick)*(18.6709859995675) + ((filmthick)**2)*(6.55116697694696) + ((rollldefifree)**2)*(0.13197883988453) + (rollspeed)*(7.21191328015338) + ((rollspeed)**2)*(1.26186094993258) - (18.9553266806959)"
            

            # prefixing 'df1' to column_names in equations
            for(i in c_santosh){
              eqn1 <- gsub(i, paste0("df1$",i), eqn1)
              eqn2 <- gsub(i, paste0("df1$",i), eqn2)
            }

            
            Final_Flake_Temp_M1 <- round(eval(parse(text = eqn1)),3)
            Final_Flake_Temp_M2 <- round(eval(parse(text = eqn2)),3)

            tbl <- cbind(Final_Flake_Temp_M1, Final_Flake_Temp_M2)
            
            nrdata <- as.data.frame(tbl)
            importresults_santosh(tbl)

            output$download2_santosh <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
            
            output$heading_santosh <- renderUI({
              h3("Simulation Results")
            })
            
            output$modeltable2_santosh <- renderDataTable({
              DT::datatable(as.data.frame(cbind(Final_Flake_Temp_M1, Final_Flake_Temp_M2)), rownames = FALSE)
            })
          })
        })
        
        
        
        # --------------------------------------------- OPTIMIZATION ------------------------------------------------------
        
        #optimisation renderings for santosh - chillpowder
        observeEvent(req(x_santosh),
                     {
                       predictors_in_model_chill <-c("Film_thickness_[0.5,2]", "Cooling_seg_fraction_[0.25,0.875]", "T_flake_feed_[100,130]" , 
                                               "T_ambient_[15,40]","T_chilled_water_[-5,15]" ,"DEFI_Free_roll_length_[0.001,0.1]",
                                               "Roll_Speed_[1,4]")
                       
                       zero_vector<-rep(1,length(predictors_in_model_chill))
                       min_vector <- c(.5,.25,100,15,-5,.001,1)
                       max_vector <- c(2,.875,130,140,15,.1,4)
                       coef_data <- data.frame(cbind(predictors_in_model_chill,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                       opt$tab_1 <- coef_data
                       opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                       opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                       
                       #table 1
                       output$optimiser_table1_chill<-renderDataTable({
                         DT::datatable(opt$tab_1,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
                       })
                       
                       observeEvent(input$optimiser_table1_chill_cell_edit,{
                         info <- input$optimiser_table1_chill_cell_edit
                         i <- info$row
                         j <- info$col
                         v <- info$value
                         if(j >= 2 && !is.na(v) && !is.na(as.numeric(v))){
                           v <- as.numeric(v)
                           if(j==2 || ( j==3 && opt$tab_1[i, j+1] > v) || (j==4 && opt$tab_1[i, j-1] < v )){
                             opt$tab_1[i,j] <<- DT::coerceValue(v,opt$tab_1[i, j])
                           }
                         }
                         rep <- opt$tab_1
                         DT::replaceData(santosh_proxy, rep, resetPaging = FALSE)
                       })
                       
                       observeEvent(input$run_optimiser_chill,{
                         
                         target_one <- input$numeric_input_one
                         inequality_selection_one <- input$inequality_selection_one
                         weight_one <- input$weight_one
                         
                         target_two <- input$numeric_input_two
                         inequality_selection_two <- input$inequality_selection_two
                         weight_two <- input$weight_two
                         
                         opt$tab_1[[2]] <- as.numeric(opt$tab_1[[2]])
                         
                         constraint <- function(x){
                         
                           equation_one <- (x[2])*(-45.5179701273954) + (x[1])*(18.8956339477401) +
                             (x[1]*x[1])*(6.67710528547635) + x[7]*(7.21746209125386) + 
                             (x[7]*x[7])*(1.27090332399397) + 20.3887470216993 - target_one
                           
                           equation_two <-  (x[2])*(-48.3757602771176) + (x[4]*x[4])*(-0.0000431916784640458) + 
                             (x[3])*(0.339086502336415) + (x[5])*0.529509608669907 +
                             (x[1])*(18.6709859995675) + (x[1]*x[1])*(6.55116697694696) + 
                             (x[6]*x[6])*(0.13197883988453) + (x[7])*7.21191328015338 +
                             (x[7]*x[7])*1.26186094993258 - 18.9553266806959 - target_two
                           
                           #lesser than
                           if(inequality_selection_one== "less than or equal to" & inequality_selection_two=="less than or equal to"){
                             return(c(equation_one,equation_two))
                           }
                           
                           else if(inequality_selection_one== "less than or equal to" & inequality_selection_two=="greater than or equal to"){
                             return(c(equation_one,-1*equation_two))
                           }
                           
                           else if(inequality_selection_one== "less than or equal to" & inequality_selection_two=="equal to"){
                             return(c(equation_one,equation_two-0.0001,-1*equation_two-0.0001))
                           }
                           
                           #greater than
                           else if(inequality_selection_one == "greater than or equal to" & inequality_selection_two=="less than or equal to"){
                             return(c(-1*equation_one,equation_two))
                           }
                           
                           else if(inequality_selection_one == "greater than or equal to" & inequality_selection_two=="greater than or equal to"){
                             return(c(-1*equation_one,-1*equation_two))
                           }
                           
                           else if(inequality_selection_one == "greater than or equal to" & inequality_selection_two=="equal to"){
                             return(c(-1*equation_one,equation_two-0.0001,-1*equation_two-0.0001))
                           }
                           
                           #equal to 
                           else if(inequality_selection_one == "equal to" & inequality_selection_two=="less than or equal to"){
                             return(c(equation_one-0.0001,-1*equation_one-0.0001,equation_two))
                           }
                           
                           else if(inequality_selection_one == "equal to" & inequality_selection_two=="greater than or equal to"){
                             return(c(equation_one-0.0001,-1*equation_one-0.0001,-1*equation_two))
                           }
                           
                           else{
                             return(c(equation_one-0.0001,-1*equation_one-0.0001,equation_two-0.0001,-1*equation_two-0.0001))
                           }
                           

                           }#constraint end
                           
                         obj<-function(x){
                           eq_one <- weight_one*(opt$tab_1[2,2]*x[2]+ opt$tab_1[1,2]*x[1] + opt$tab_1[1,2]*x[1]*opt$tab_1[1,2]*x[1] +
                                                   opt$tab_1[7,2]*x[7] + opt$tab_1[7,2]*x[7]*opt$tab_1[7,2]*x[7] )
                           
                           eq_two <- weight_two*(opt$tab_1[2,2]*x[2] + opt$tab_1[4,2]*x[4]*opt$tab_1[4,2]*x[4] +
                                                   opt$tab_1[3,2]*x[3] + opt$tab_1[5,2]*x[5] + opt$tab_1[1,2]*x[1] + 
                                                   opt$tab_1[1,2]*x[1]*opt$tab_1[1,2]*x[1] +
                                                   opt$tab_1[6,2]*x[6]*opt$tab_1[6,2]*x[6] + opt$tab_1[7,2]*x[7] + 
                                                   opt$tab_1[7,2]*x[7]*opt$tab_1[7,2]*x[7] 
                                                   )
                           
                           
                           if(input$radio_button_chill == 'min'){
                             return(eq_one+eq_two)
                           }
                           
                           else{
                             return(-eq_one-eq_two)
                           }
                           
                         } #objective function end
                         
                         x0 <- opt$tab_1[[3]]
                         lb <- opt$tab_1[[3]]
                         ub <- opt$tab_1[[4]]
                         
                         opts <- list("algorithm"="NLOPT_LN_COBYLA",
                                      "xtol_rel"=1.0e-8)
                         res<- nloptr(x0=x0,eval_f =  obj,
                                      eval_g_ineq = constraint,
                                      opts = opts,
                                      lb=lb, ub=ub)
                         
                         # optimiser output table 1
                         output$optimiser_table32_chill <- renderDataTable({
                           df<-data.frame(Predictors = c("Film_thickness", "Cooling_seg_fraction", "T_flake_feed" , 
                                                         "T_ambient","T_chilled_water" ,"DEFI_Free_roll_length",
                                                         "Roll_Speed"),
                                          Value = round(res$solution,3)
                           )
                           DT::datatable(df,selection ="none",rownames = FALSE )
                         })
                         
                         constraint_value <- function(x){
                            
                           e <- (x[2])*(-45.5179701273954) + (x[1])*(18.8956339477401) +
                             (x[1]*x[1])*(6.67710528547635) + x[7]*(7.21746209125386) + 
                             (x[7]*x[7])*(1.27090332399397) + 20.3887470216993
                           return(e)}
                           
                         constraint_value2 <- function(x){
                           eq <-  (x[2])*(-48.3757602771176) + (x[4]*x[4])*(-0.0000431916784640458) + 
                             (x[3])*(0.339086502336415) + (x[5])*0.529509608669907 +
                             (x[1])*(18.6709859995675) + (x[1]*x[1])*(6.55116697694696) + 
                             (x[6]*x[6])*(0.13197883988453) + (x[7])*7.21191328015338 +
                             (x[7]*x[7])*1.26186094993258 - 18.9553266806959
                           return(eq)
                           }
                         
                         # optimiser output table 2
                         output$optimiser_table22_chill <- renderDataTable({
                           value1 <- round(constraint_value(res$solution),3)
                           value2 <- round(constraint_value2(res$solution),3)
                           # val <- data.frame(Predictors = c("	Flake Final Temp (Model 1)","	Flake Final Temp (Model 2)"),
                           #                   Value = as.data.frame(rbind(value1,value2)))
                           
                           DT::datatable(as.data.frame(rbind(round(constraint_value(res$solution),3),round(constraint_value2(res$solution),3))) 
                                         ,rownames = c("Flake Final Temp (Model 1)","	Flake Final Temp (Model 2)"), colnames =c("Target variable", "Value"))
                         })
                         
                         
                         # optimiser output table 3
                         if(input$radio_button_chill=='min'){
                           output$value_results_chill<- renderUI({
                             ns <- session$ns
                             p(paste0("The objective function value resulting from the optimisation is : "),round(res$objective,3))
                           })
                         }
                         else{
                           output$value_results_chill<- renderUI({
                             ns <- session$ns
                             p(paste0("The objective function value resulting from the optimisation is : "),round(-1*res$objective,3))
                           })
                           
                         }
                         
                         
                       })#observeEvent run optimiser ends
                       
                       # reset button
                       observeEvent(input$reset_chill,{
                         
                         updateSelectInput(session,"inequality_selection_one",selected = "less than or equal to")
                         updateNumericInput(session,"numeric_input_one",value = 45)
                         updateNumericInput(session,"weight_one",value = 1)
                         updateRadioButtons(session,"radio_button_chill",selected = "min")
                         
                         updateSelectInput(session,"inequality_selection_two",selected = "less than or equal to")
                         updateNumericInput(session,"numeric_input_two",value = 45)
                         updateNumericInput(session,"weight_two",value = 1)
                         predictors_in_model3_santosh<-c("Film_thickness_[0.5,2]", "Cooling_seg_fraction_[0.25,0.875]", "T_flake_feed_[100,130]" , 
                                                         "T_ambient_[15,40]","T_chilled_water_[-5,15]" ,"DEFI_Free_roll_length_[0.001,0.1]",
                                                         "Roll_Speed_[1,4]")
                         zero_vector<-rep(1,length(predictors_in_model3_santosh))
                         min_vector <- c(.5,.25,100,15,-5,.001,1)
                         max_vector <- c(2,.875,130,140,15,.1,4)
                         coef_data <- data.frame(cbind(predictors_in_model_chill,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                         opt$tab_1 <- coef_data
                         opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                         opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]]) 
                       })
                       
                     })#observeevent datacall close
        
        observeEvent(input$downloadresults_santosh,{
          
          output$Download_Values_santosh <- renderUI({
            ns <- session$ns
            downloadButton(ns("download_all_santosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
          nrdata <- as.data.frame(manual_santosh())
          nrdata1 <- as.data.frame(manualinput_santosh())
          nrdata2 <- as.data.frame(importresults_santosh())
          nrdata3 <- as.data.frame(optimise_santosh())
          
          output$download_all_santosh <- downloadHandler(
            filename = function() { "All Results.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2, "Drying Prediction Optimisation" = nrdata3), file)
            }
          )
        })
      })
      
      
      
    }
  )
}