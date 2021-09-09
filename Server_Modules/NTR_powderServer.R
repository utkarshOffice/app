NTR_powderServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      uday_proxy_ntr <- DT::dataTableProxy('optimiser_table1_uday_ntr')
      manualinput <- reactiveVal(NULL)
      manual <- reactiveVal(NULL)
      importresults <- reactiveVal(NULL)
      optimise <- reactiveVal(NULL)
      optimise1 <- reactiveVal(NULL)
      optimise2_uday_ntr <- reactiveVal(NULL)
      opt <- reactiveValues(tab_1=NULL)
      
      x_uday_ntr <- data.frame(Models <- c("BD Prediction by Model  = 132.314 + 0.00874 * Base Factor * Base Powder Bulk Density + 8.812 * Filler (Sulphate/ Salt as Balancing ingredient)+ 8.602 * Post Dosing Ingredients Majors ( >1% in FG other than Filler) + 9.003 * Post Dosing Ingredient Minor (<1% in FG other than Filler)"
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
      
      output$advice_uday_ntr <- renderDataTable({
        Advisory_table <- data.frame(Ingredients = c("Base Factor","Filler Sulphate Salt as Balancing ingredient",
                                                     "Base Powder Bulk Density","Post Dosing Ingredients Majors(>1% in FG other than Filler)",
                                                     "Post Dosing Ingredients Minors (<1% in FG other than Filler)"),
                                     Lower_Level = c(66.5, 0, 547, 0, 0.5),
                                     Upper_Level = c(99.5, 23.34, 992, 18.87, 4.52))
        
    
        datatable(Advisory_table)
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
                           gghighlight(Base_Factor == input$Base_Factor, label_key = BD_Prediction_by_Model)
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
                   
                   output$multi_lines_graph_uday_ntr <- renderPlotly({
                     if(length(input$y_axis) == 1){
                       fig <- plot_ly( x = ~data_uday_ntr()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis]],mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = as.character(input$y_axis)) )
                       fig
                     }else if(length(input$y_axis) == 2){
                       fig <- plot_ly(data_uday_ntr(), x = ~data_uday_ntr()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     }
                     else if(length(input$y_axis) == 3){
                       fig <- plot_ly(data_uday_ntr(), x = ~data_uday_ntr()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 4){
                       fig <- plot_ly(data_uday_ntr(), x = ~data_uday_ntr()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 5){
                       fig <- plot_ly(data_uday_ntr(), x = ~data_uday_ntr()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[5]]], name = input$y_axis[5], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 6){
                       fig <- plot_ly(data_uday_ntr(), x = ~data_uday_ntr()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[5]]], name = input$y_axis[5], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_ntr()[[input$y_axis[6]]], name = input$y_axis[6], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
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
            values <- c("65.5","0","547","0","0.5")
            sqr <- do.call(rbind,data.frame(cbind(munits,values)))
            #sqr1 <- datatable(sqr, editable = T,colnames = c("Model Predictors","Measurement Units", "Enter Simulation Values"))
            colnames(sqr) <- c("Base Factor","Filler Sulphate Salt as Balancing ingredient",
                               "Base Powder Bulk Density","Post Dosing Ingredients Majors",
                               "Post Dosing Ingredients Minors")
            rownames(sqr) <- c("Measurement Units (fractions)", "Enter Simulation Values")
            sqr
          })
          x1_uday_ntr$df <- y_uday_ntr()
        })
        
        
        
        output$simulation_input_uday_ntr <- renderDataTable({ 
          datatable(x1_uday_ntr$df, editable = T) %>%
            
            formatStyle(
              "Base Factor",
              color = styleInterval(c(65.4, 99.6), c('red', 'black', 'red')))%>%
            formatStyle(
              "Filler Sulphate Salt as Balancing ingredient",
              color = styleInterval(c(-0.01, 23.35), c('red', 'black', 'red')))%>%
            formatStyle(
              "Base Powder Bulk Density",
              color = styleInterval(c(546, 993), c('red', 'black', 'red')))%>%
            formatStyle(
              "Post Dosing Ingredients Majors",
              color = styleInterval(c(-0.01, 18.88), c('red', 'black', 'red')))%>%
            formatStyle(
              "Post Dosing Ingredients Minors",
              color = styleInterval(c(0.4, 4.53), c('red', 'black', 'red')))
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
          datatable(x1_uday_ntr$df, 
                    colnames = c("Base Factor","Filler Sulphate Salt as Balancing ingredient",
                                 "Base Powder Bulk Density","Post Dosing Ingredients Majors",
                                 "Post Dosing Ingredients Minors"))
        )
        
        
        observeEvent(input$simulate_uday_ntr,{
          
          eqn1 <- "132.314+0.00874* basefactor * basepowderbulkdensity +8.812*fillersulphatesaltasbalancingingredient+8.602*postdosingingredientsmajors+9.003*postdosingingredientsminors"
          colnames(x1_uday_ntr$df) <- gsub(" ","",tolower(colnames(x1_uday_ntr$df)))
          
          for(i in colnames(x1_uday_ntr$df)){
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
            {DT::datatable(tbl, rownames = FALSE, colnames = "BD Prediction by Model") })
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
        nrdata4 <- as.data.frame(optimise2_uday_ntr())
        
        
        #View(nrdata)
        #View(nrdata1)
        #View(nrdata2)
        
        output$download_all_uday_ntr <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2, "BD Prediction by Model Optimisation" = nrdata4), file)
          }
        )
      })#download end
      
      # optimisation for uday ntr
      observeEvent(req(x_uday_ntr),{
        predictor_names_ntr <- c("Base Factor [65.5,99.5]","Base Powder Bulk Density [547,992]",
                                 "Filler Sulphate Salt as Balancing ingredient [0,23.34]",
                                "Post Dosing Ingredients Majors(>1% in FG other than Filler) [0,18.87]",
                                "Post Dosing Ingredients Minors (<1% in FG other than Filler) [0.5,4.52]")
        zero_vector<-rep(1,length(predictor_names_ntr))
        min_vector <- c(65.5,547,0,0,0.5)
        max_vector <- c(99.5,992,23.34,18.87,4.52)
        coef_data2 <- data.frame(cbind(predictor_names_ntr,zero_vector,min_vector,max_vector))
        opt$tab_1 <- coef_data2
        opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
        opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])

        #table 1
        output$optimiser_table1_uday_ntr <- renderDataTable({
          DT::datatable(opt$tab_1,selection="none",editable=TRUE,
                        colnames = c("Predictors [Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
        })

        #cell edit
        observeEvent(input$optimiser_table1_uday_ntr_cell_edit,{
          info <- input$optimiser_table1_uday_ntr_cell_edit
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
          DT::replaceData(uday_proxy_ntr, rep, resetPaging = FALSE)
        })


        observeEvent(input$run_optimiser_uday_ntr,{

          target_ntr <- input$numeric_input_uday_ntr
          inequality_selection_ntr <- input$inequality_selection_uday_ntr

          opt$tab_1[[2]] <- as.numeric(opt$tab_1[[2]])

          constraint <- function(x){
            equation <-  132.314+0.00874*x[1]*x[2]+8.812*x[3]+8.602*x[4]+9.003*x[5]-target_ntr

            if(inequality_selection_ntr=="less than or equal to"){
              return(equation)
            }

            else if(inequality_selection_ntr=="greater than or equal to"){
              return(-1*equation)
            }

            else{
              return(c(equation-0.001,-1*equation-0.001))
            }

          }# constraint ends

          obj <- function(x){

            eq <- opt$tab_1[1,2]*x[1]*opt$tab_1[2,2]*x[2] + opt$tab_1[3,2]*x[3] + opt$tab_1[4,2]*x[4]+ opt$tab_1[5,2]*x[5]

            if(input$radio_button_uday_ntr=='min'){
              return(eq)
            }

            else{
              return(-1*eq)
            }

          }#obj end

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
          output$optimiser_table32_uday_ntr <- renderDataTable({
            df<-data.frame(Predictors = c("Base Factor","Base Powder Bulk Density",
                                          "Filler Sulphate Salt as Balancing ingredient",
                                          "Post Dosing Ingredients Majors(>1% in FG other than Filler)",
                                          "Post Dosing Ingredients Minors(<1% in FG other than Filler)"),
                           Value = round(res$solution,3)
            )
            DT::datatable(df,selection ="none",rownames = FALSE)
          })

          constraint_value <- function(x){
            return(132.314+0.00874*x[1]*x[2]+8.812*x[3]+8.602*x[4]+9.003*x[5])
          }
          
          # optimiser output table 2
          output$optimiser_table22_uday_ntr <- renderDataTable({


            DT::datatable(as.data.frame(round(constraint_value(res$solution),3))
                          ,rownames = c("BD Prediction by Model"), colnames =c("Target variable", "Value"))
          })
  
          
          # optimiser output table 3
          if(input$radio_button_uday_ntr=='min'){
            output$value_results_uday_ntr<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(res$objective,3))
            })
          }
          else{
            output$value_results_uday_ntr<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(-1*res$objective,3))
            })
            
          }
          
#<<<<<<< subinoffice2
          if(inequality_selection_ntr=="equal to" && abs(constraint_value(res$solution)-target_ntr)>.2 ){
            showModal(modalDialog("Non Linear optimisation will give unexpected results for the given inputs.
                                  Please alter the inputs and re-run."))
          }
          
          if(inequality_selection_ntr=="less than or equal to" && constraint_value(res$solution)>target_ntr ){
            showModal(modalDialog("Non Linear optimisation will give unexpected results for the given inputs.
                                  Please alter the inputs and re-run."))
          }
          
          if(inequality_selection_ntr=="greater than or equal to" && constraint_value(res$solution)<target_ntr ){
            showModal(modalDialog("Non Linear optimisation will give unexpected results for the given inputs.
                                  Please alter the inputs and re-run."))
          }
          
          # if(res$status!=0){
          #   showModal(modalDialog("Non Linear optimisation will give unexpected results for the given inputs.
          #                         Please alter the inputs and re-run."))
          #   
          # }
        # View(res$status)
#=======
          downresults12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("BD Prediction by Model"), Predicted_or_Optimal_Value= constraint_value(res$solution))
          downdf12<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("Base Factor","Filler Sulphate Salt as Balancing ingredient","Base Powder Bulk Density","Post Dosing Ingredients Majors(>1% in FG other than Filler)","Post Dosing Ingredients Minors (<1% in FG other than Filler)"),
                               Predicted_or_Optimal_Value=res$solution)
          
          if(input$radio_button_uday_ntr=='min'){
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = res$objective)
          }
          else{
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = -1*res$objective)
          }
          
          final123 <- rbind(downresults12,downdf12,downopt12)
          #View(final123)
          
          optimise2_uday_ntr(final123)
          output$download5_uday_ntr <- downloadHandler(
            filename = function() { "BD Prediction by Model Optimisation .xlsx"},
            content = function(file) {
              write_xlsx(list("Optimisation Result" = final123), file)
            }
          )

#>>>>>>> master
        })#observeevent run optimiser ends

      })#observeevent opt end
      
      observeEvent(input$reset_uday_ntr,{
        updateSelectInput(session,"inequality_selection_uday_ntr",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_uday_ntr",value = 900)
        updateRadioButtons(session,"radio_button_uday_ntr",selected = "min")
        predictors_in_model2<-c("Base Factor [65.5,99.5]","Base Powder Bulk Density [547,992]",
                                "Filler Sulphate Salt as Balancing ingredient [0,23.34]",
                                "Post Dosing Ingredients Majors(>1% in FG other than Filler) [0,18.87]",
                                "Post Dosing Ingredients Minors (<1% in FG other than Filler) [0.5,4.52]")
        zero_vector<-rep(1,length(predictors_in_model2))
        min_vector <- c(65.5,547,0,0,0.5)
        max_vector <- c(99.5,992,23.34,18.87,4.52)
        coef_data <- data.frame(cbind(predictors_in_model2,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
        opt$tab_1 <- coef_data
        opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
        opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
      })
      
      
      
    }
  )}


















