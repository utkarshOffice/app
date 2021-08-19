chillrollServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      santosh_proxy <- DT::dataTableProxy('optimiser_table1_santosh')
      opt<-reactiveValues(tab_1=NULL) 
      
      
      manualinput_santosh <- reactiveVal(NULL)
      manual_santosh <- reactiveVal(NULL)
      importresults_santosh <- reactiveVal(NULL)
      optimise_santosh <- reactiveVal(NULL)
      optimise1_santosh <- reactiveVal(NULL)
      optimise2_santosh <- reactiveVal(NULL)
      
      x_santosh <- data.frame(Models_santosh <- c("Flake Final Temp (Model 1) = (Cooling_seg_fraction)*(-45.5179701273954) + (Film_thickness)*(18.8956339477401) + ((Film_thickness)^2)*(6.67710528547635) + (Roll_Speed)*(7.21746209125386) + ((Roll_Speed)^2)*(1.27090332399397) + 20.3887470216993",
                                            "Flake Final Temp (Model 2) = (Cooling_seg_fraction)*(-48.3757602771176) + (T_ambient)^2*(-0.0000431916784640458) + (T_flake feed)*(0.339086502336415) + (T_chilled water)*0.529509608669907 + (Film thickness)*(18.6709859995675) + (Film thickness)^2*(6.55116697694696) + (DEFI Free roll length)^2*(0.13197883988453) + (Roll Speed)*7.21191328015338 + (Roll Speed)^2*1.26186094993258 - 18.9553266806959"
                                            ))
      
      
      b_santosh <- c("Film_thickness", "Cooling_seg_fraction", "T_flake_feed" , "T_ambient","T_chilled_water" ,"DEFI_Free_roll_length", "Roll_Speed")
      c_santosh <- c("filmthick", "coolseg", "tfeed" , "tambient","tsteel" ,"rollldefifree", "rollspeed")
      d_santosh <- c("filmthick", "coolseg", "tfeed" , "tambient","tsteel" ,"rollldefifree", "rollspeed")
      # c_santosh <- c("targetsmc", "NaLASinSlurry", "LSAinSlurry" , "SulphateinSlurry","AlkSilicateinSlurry" ,"CP5inSlurry", "SCMCinSlurry")
      # d_santosh <- c("targetsmc", "nalas", "lsa" , "sulphate","alksilicate" ,"cp5", "scmc")
      all_vars_santosh <-c("Film_thickness", "Cooling_seg_fraction", "T_flake_feed" , "T_ambient","T_chilled_water" ,"DEFI_Free_roll_length", "Roll_Speed")
      # model bank table 
      
      output$models_santosh <- renderDataTable({
        datatable(x_santosh, colnames = c("Models"))
      } )
      
      
      
      # go to simulation
      observeEvent(input$commit_santosh,{
        updateTabsetPanel(top_session, "tabs_santosh", selected = "Simulation")
      })
      # go to visualization
      observeEvent(input$commit2_santosh,{
        updateTabsetPanel(top_session, "tabs_santosh", selected = "Visualization")
      })
      
      #rendering advisory tables
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
      
      
      # profiler renderings
      observeEvent(req(input$profiler_Film_thickness),{
        
        eqn1 <- "(Cooling_seg_fraction)*(-45.5179701273954) + (Film_thickness)*(18.8956339477401) + ((Film_thickness)^2)*(6.67710528547635) + (Roll_Speed)*(7.21746209125386) + ((Roll_Speed)^2)*(1.27090332399397) + 20.3887470216993"
        eqn2 <- "(Cooling_seg_fraction)*(-48.3757602771176) + ((T_ambient)**2)*(-0.0000431916784640458) + (T_flake_feed)*(0.339086502336415) + (T_chilled_water)*(0.529509608669907) + (Film_thickness)*(18.6709859995675) + ((Film_thickness)**2)*(6.55116697694696) + ((DEFI_Free_roll_length)**2)*(0.13197883988453) + (Roll_Speed)*(7.21191328015338) + ((Roll_Speed)**2)*(1.26186094993258) - (18.9553266806959)"
        
        
        Film_thickness <- round(seq(from  = 0.5, to = 2.0, length.out = 25),3)
        Cooling_seg_fraction <- reactive(input$profiler_Cooling_seg_fraction)
        T_ambient <- reactive(input$profiler_T_ambient)
        T_flake_feed <- reactive(input$profiler_T_flake_feed)
        DEFI_Free_roll_length <- reactive(input$profiler_DEFI_Free_roll_length)
        Roll_Speed <- reactive(input$profiler_Roll_Speed)
        T_chilled_water <- reactive(input$profiler_T_chilled_water)
        
        # making sure all variables are changed to numeric
        Film_thickness <- as.numeric(Film_thickness)
        Cooling_seg_fraction <- as.numeric(Cooling_seg_fraction())
        T_ambient <- as.numeric(T_ambient())
        T_flake_feed <- as.numeric(T_flake_feed())
        DEFI_Free_roll_length <- as.numeric(DEFI_Free_roll_length())
        Roll_Speed <- as.numeric(Roll_Speed())
        T_chilled_water <- as.numeric(T_chilled_water())
        
        observeEvent(input$profiler_Film_thickness | input$profiler_T_ambient | input$profiler_T_flake_feed | input$profiler_Cooling_seg_fraction |input$profiler_DEFI_Free_roll_length | input$profiler_T_chilled_water | input$profiler_Cooling_seg_fraction,
            {
            Final_Flake_Temp_M1 <- reactive(eval(parse(text = eqn1)))
            Final_Flake_Temp_M2 <- reactive(eval(parse(text = eqn2)))
            
            output$plot1 <- renderPlot({
              FFT1 <- Final_Flake_Temp_M1()
              ggplot(data=data.frame(Film_thickness, FFT1), aes(x=Film_thickness, y= FFT1)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(Film_thickness == input$profiler_Film_thickness)
            })
            
            output$plot2 <- renderPlot({
              FFT2 <- Final_Flake_Temp_M2()
              ggplot(data=data.frame(Film_thickness, FFT2), aes(x=Film_thickness, y= FFT2)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(Film_thickness == input$profiler_Film_thickness)
            })
    
            })
      })
      
      
      # --------------------------------------------- VISUALIZATION ------------------------------------------------------
      
      #visualization page renderings
      observeEvent(req(input$datacall_santosh),
                   {#data_santosh <- reactive(read_excel(input$datacall_santosh$datapath))
                     #data <- reactive(data()[,-(which(colSums(data())==0))])
                     santosh_data_slurry1 <- reactive({
                       req(input$datacall_santosh)
                       inFile <- input$datacall_santosh
                       if(is.null(inFile)) return(NULL)

                       #xlfile <- read_excel(paste0(inFile$datapath, ".xlsx"))
                       xlfile <- read_excel(input$datacall_santosh$datapath , sheet='Sensor')
                       colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
                       colnames(xlfile) <- gsub(" ","",colnames(xlfile))
                       colnames(xlfile) <- tolower(colnames(xlfile))
                       xlfile

                     })
                     df1 <- santosh_data_slurry1()
                     # df1$sum <- df1$nalas + df1$scmc + df1$alksilicate + df1$cp5 + df1$sulphate + df1$lsa
                     # df1$NaLASinSlurry <- df1$nalas/df1$sum*(1-df1$targetsmc)
                     # df1$LSAinSlurry <- df1$lsa/df1$sum*(1-df1$targetsmc)
                     # df1$SCMCinSlurry <- df1$scmc/df1$sum*(1-df1$targetsmc)
                     # df1$AlkSilicateinSlurry <- df1$alksilicate/df1$sum*(1-df1$targetsmc)
                     # df1$CP5inSlurry <- df1$cp5/df1$sum*(1-df1$targetsmc)
                     # df1$SulphateinSlurry <- df1$sulphate/df1$sum*(1-df1$targetsmc)
                     #View(df1)
                     santosh_data_slurry2 <- reactive({
                       req(input$datacall_santosh)
                       inFile <- input$datacall_santosh
                       if(is.null(inFile)) return(NULL)

                       #file.rename(inFile$datapath, paste0(inFile$datapath, ".xlsx"))
                       #xlfile <- read_excel(paste0(inFile$datapath, ".xlsx"))
                       xlfile <- read_excel(input$datacall_santosh$datapath, sheet='Sensor', range='Sensor!D1:J502')

                       xlfile

                     })
                     df2 <- santosh_data_slurry2()
                     #df3 <- df1[tail(seq_along(df1),7)]
                     
                     #colnames(df3) <- c("Cool seg",	"T ambient","T feed","T steel",	"film thick"	,"roll L DEFI Free", "roll Speed")
                     #View(df3)
                     #finaldf <- as.data.frame(cbind(df2, df3))
                     finaldf <- as.data.frame(df2)

                     output$simulationdata_santosh <- renderDataTable({
                       santosh_data_slurry2()
                     })
                     # View(finaldf)
                     #View(data_santosh)
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
                           #labs(x = as.character(input$x_axis_bd),y = as.character(input$y_axis))
                         }else{
                           #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
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
                           # labs(x = as.character(input$x_axis_bd),y = as.character(input$y_axis))
                         }else
                         {
                           first <- input$y_axis_bd[1]
                           second <- input$y_axis_bd[2]
                           #scaleFactor <- max(data()[[first]]) / max(data()[[second]])

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
                           #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                           first <- input$y_axis_bd[1]
                           second <- input$y_axis_bd[2]

                           ggplot(finaldf, aes(x=finaldf[[input$x_axis_bd]])) +
                             geom_point(aes(y=finaldf[[first]]), col="blue")+
                             geom_point(aes(y=finaldf[[second]] * 1), col="red", shape = 18)+
                             geom_line(aes(y=finaldf[[first]]), col="blue")+
                             geom_line(aes(y=finaldf[[second]] * 1), col="red")+
                             # geom_smooth(aes(y=data()[[first]]), method="loess", col="blue") +
                             # geom_smooth(aes(y=data()[[second]] * 20), method="loess", col="red") +
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
                           #scaleFactor <- max(data()[[first]]) / max(data()[[second]])

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

      
      # --------------------------------------------- SIMULATION ------------------------------------------------------
      
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
  
          
          for(i in b_santosh){
            eqn1 <- gsub(i, df[1,i], eqn1)
            eqn2 <- gsub(i, df[1,i], eqn2)
          }
          

          Final_Flake_Temp_M1 <- eval(parse(text = eqn1))
          Final_Flake_Temp_M2 <- eval(parse(text = eqn2))

          
          tbl <- cbind(Final_Flake_Temp_M1, Final_Flake_Temp_M2)
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
            

            
            for(i in c_santosh){
              eqn1 <- gsub(i, paste0("df1$",i), eqn1)
              eqn2 <- gsub(i, paste0("df1$",i), eqn2)
            }

            
            Final_Flake_Temp_M1 <- eval(parse(text = eqn1))
            Final_Flake_Temp_M2 <- eval(parse(text = eqn2))

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
        
        
        
        
        
        #})
        # --------------------------------------------- OPTIMIZATION ------------------------------------------------------
        #optimisation renderings for santosh(SD slurry props- drying prediction)
        observeEvent(req(x_santosh),
                     {
                       predictors_in_model3_santosh<-c("TargetSMC_[0.287,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                                    "CP5_[0,0.03]", "LSA_[0.17,0.45]",
                                                    "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
                       zero_vector<-rep(1,length(predictors_in_model3_santosh))
                       min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                       max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                       coef_data <- data.frame(cbind(predictors_in_model3_santosh,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                       opt$tab_1 <- coef_data
                       opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                       opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                       
                       #table 1
                       output$optimiser_table1_santosh<-renderDataTable({
                         DT::datatable(opt$tab_1,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
                       })
                       
                       observeEvent(input$optimiser_table1_santosh_cell_edit,{
                         info <- input$optimiser_table1_santosh_cell_edit
                         # View(opt$tab_1)
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
                       
                       observeEvent(input$run_optimiser_santosh,{
                         
                         predictors_in_model3_santosh<-c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate")
                         reg_coeff_in_model3_santosh<-c(1.64652727504537,-0.340054974118285,0.0349876142645199,-0.26064073764549,-0.0575389664392278,-1.17237663840093,-0.298363251134605)
                         if(input$inequality_selection_santosh=="less than or equal to"){
                           constr_santosh<-'<='
                         }
                         else if(input$inequality_selection_santosh=="greater than or equal to"){
                           constr_santosh<-'>='
                         }
                         else{
                           constr_santosh<-'='
                         }
                         # View(constr)#works
                         
                         target_santosh<-input$numeric_input_santosh
                         number.predictors_santosh<-length(predictors_in_model3_santosh)
                         # View(opt$tab_1)
                         low.lims_santosh<-opt$tab_1[[3]]
                         upp.lims_santosh<-opt$tab_1[[4]]
                         objective.in_santosh<-opt$tab_1[[2]]
                         objective.in_santosh<-as.numeric(objective.in_santosh)
                         obj.type_santosh<-input$radio_button_santosh
                         # View(objective.in)#works
                         
                         lps.model_santosh <- make.lp(0,number.predictors_santosh)
                         add.constraint(lps.model_santosh,reg_coeff_in_model3_santosh,constr_santosh,target_santosh)
                         
                         # Bounds for variables
                         set.bounds(lps.model_santosh,lower=low.lims_santosh)
                         set.bounds(lps.model_santosh,upper=upp.lims_santosh)
                         # View(low.lims)
                         # View(nrow(low.lims))
                         # View(obj.type_santosh)
                         # View(objective.in)
                         
                         # Objective function
                         lp.control(lps.model_santosh,sense=obj.type_santosh) # min or max
                         # View(objective.in) #getting output
                         
                         set.objfn(lps.model_santosh,objective.in_santosh) # coefficients
                         # View(upp.lims)
                         
                         # Apply solver
                         solution.status <- solve(lps.model_santosh)
                         # View(solution.status)
                         if(solution.status!=0){
                           showModal(modalDialog("Linear Optimisation could not find a solution for the given inputs. Please change the inputs and re-run."))
                         }
                         #unpacking
                         solution.values_santosh <- get.primal.solution(lps.model_santosh)
                         # View(solution.values_santosh)
                         objective.function.value_santosh <- solution.values_santosh[1]
                         fitted.response_santosh <- solution.values_santosh[2]
                         solution.values_santosh <- solution.values_santosh[3:length(solution.values_santosh)]
                         
                         results_santosh<-data.frame(Value = fitted.response_santosh)
                         # colnames(results)<-""
                         row.names(results_santosh)<-"Drying Prediction"
                         
                         #dopwnload
                         downresults <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Drying Prediction"), Predicted_or_Optimal_Value= fitted.response_santosh)
                         downdf<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate"),
                                            Predicted_or_Optimal_Value=solution.values_santosh)
                         downopt <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective func. Value"), Predicted_or_Optimal_Value = objective.function.value_santosh)
                         
                         
                         final1 <- rbind(downresults,downdf,downopt)
                         #View(final1)
                         optimise_santosh(final1)
                         output$download3_santosh <- downloadHandler(
                           filename = function() { "Optimisation for Drying Prediction .xlsx"},
                           content = function(file) {
                             write_xlsx(list("Optimisation Result" = final1), file)
                           }
                         )
                         
                         # optmiser table2
                         output$optimiser_table2_santosh<-renderDataTable({
                           DT::datatable(results_santosh) })
                         
                         # optmiser table3
                         output$optimiser_table3_santosh<- renderDataTable({
                           df_santosh<-data.frame(Predictors=c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate"),
                                               Value=solution.values_santosh)
                           DT::datatable(df_santosh,rownames=FALSE)
                           # DT::datatable(df)
                         })
                         
                         #optimiser textoutput
                         output$value_results_santosh<- renderUI({
                           p(paste0("The objective value resulting from the optimisation is : "),round(objective.function.value_santosh),4)
                         })
                         
                         
                         
                       })#observeEvent run optimiser ends
                       
                       # reset button
                       observeEvent(input$reset_santosh,{
                         updateSelectInput(session,"inequality_selection_santosh",selected = "less than or equal to")
                         updateNumericInput(session,"numeric_input_santosh",value = .32)
                         updateRadioButtons(session,"radio_button_santosh",selected = "min")
                         predictors_in_model3_santosh<-c("TargetSMC_[0.287,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                                      "CP5_[0,0.03]", "LSA_[0.17,0.45]",
                                                      "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
                         zero_vector<-rep(1,length(predictors_in_model3_santosh))
                         min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                         max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                         coef_data <- data.frame(cbind(predictors_in_model3_santosh,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
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