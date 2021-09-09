Soap_hardnessServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      manualinput <- reactiveVal(NULL)
      manual <- reactiveVal(NULL)
      importresults <- reactiveVal(NULL)
      optimise <- reactiveVal(NULL)
      optimise1 <- reactiveVal(NULL)
      optimise2_soap <- reactiveVal(NULL)
      opt <- reactiveValues(tab_1=NULL)
      proxy_hardness <- DT::dataTableProxy('optimiser_table1_hardness')
      
      x_uday_sd <- data.frame(Models <- c("Soap Bar Hardness = 76.4293896016496 + -3.68749131760183 * PKO Content + 0.0146038397207457 *
  Glycerine + 0.129132110568962 * Sodium Chloride + -0.16205511182105 *
  Sodium Sulfate + 7.04084445503392 * Sodium Silicate + 0.222746095064746 *
  Petrolatum Jelly + -4.00166431759117 * Amazon Polymer + -35.5964551844198 *
  Carbopole SC200  ( 100%) + -0.0109043309619179 * Talc + 0.0796945875198386 *
  Moisture + -0.181260159211409 * IV (Iodine Value) + 0.250970645173301 *
  Soap Mass Temperature + -0.0000661956994276951 * Flow Rate (30%)
+0.0985345478859178 * Plodder Back Pressure (30%) @ 40 degC + -0.241325571790654
* Billet Moisture + (Glycerine - 3.27272727272727) * ((Glycerine
   -3.27272727272727) * 0.0608438311745331) + (Glycerine - 3.27272727272727) * ((
    Flow Rate (30%) - 5719.86363636364) * 0.0000098445456748929) + (Sodium Chloride
    - 0.831818181818182) * ((Talc - 2.90909090909091) * -0.00446869632914622) + (
     Sodium Chloride - 0.831818181818182) * ((IV (Iodine Value) - 38.6136363636364)
     * -0.027000138479312) + (Sodium Silicate - 1.16686363636364) * ((Sodium Silicate
     - 1.16686363636364) * 0.291936080502523) + (Sodium Silicate - 1.16686363636364) *
  ((Billet Moisture - 20.4986363636364) * -0.162669884515511) + (Petrolatum Jelly
     -0.236363636363636) * ((Flow Rate (30%) - 5719.86363636364) *
       -0.000263176092084823) + (Petrolatum Jelly - 0.236363636363636) * ((
Plodder Back Pressure (30%) @ 40 degC - 11.7522727272727) * 0.145362088204255)
+ (Petrolatum Jelly - 0.236363636363636) * ((Billet Moisture - 20.4986363636364)
   * 0.255700486025522) + (Amazon Polymer - 0.0838636363636363) * ((
  Soap Mass Temperature - 41.5577272727273) * 0.326798858596123) + (Amazon Polymer
     - 0.0838636363636363) * ((Plodder Back Pressure (30%) @ 40 degC
    -11.7522727272727) * 0.0446677847454846) + (Amazon Polymer - 0.0838636363636363) *
  ((Billet Moisture - 20.4986363636364) * -1.04095149545583) + (Talc
    -2.90909090909091) * ((Flow Rate (30%) - 5719.86363636364) *
    -0.0000085461667984637) + (Talc - 2.90909090909091) * ((Billet Moisture
     -20.4986363636364) * 0.00485018468726219) + (Moisture - 20.5656818181818) * ((
    Plodder Back Pressure (30%) @ 40 degC - 11.7522727272727) * -0.0237189322663761)
+ (IV (Iodine Value) - 38.6136363636364) * ((IV (Iodine Value)
   -38.6136363636364) * 0.0442814320739906) + (IV (Iodine Value) - 38.6136363636364
   ) * ((Flow Rate (30%) - 5719.86363636364) * 0.0000197853904718027) + (
    Soap Mass Temperature - 41.5577272727273) * ((Soap Mass Temperature
   -41.5577272727273) * -0.0490857657173331) + (
   Plodder Back Pressure (30%) @ 40 degC - 11.7522727272727) * ((
  Plodder Back Pressure (30%) @ 40 degC - 11.7522727272727) * -0.00438487333986625 )"
      ))  
      
      b_uday_sd <- c("Billet Moisture","Plodder Back Pressure (30%) @ 40 degC","Flow Rate (30%)","Soap Mass Temperature",
                     "IV (Iodine Value)","Moisture","PKO Content","Glycerine","Sodium Chloride","Sodium Sulfate",
                     "Sodium Silicate","Petrolatum Jelly","Amazon Polymer","Carbopole SC200 (100%)","Talc")
      
      all_uday_sd <- c("billetmoistur","plodderbackpressuredegc","flowrate","soapmasstemperature",
                       "iv","moisture","pkocontent","glycerine","sodiumchloride","sodiumsulfate",
                       "sodiumsilicate","petrolatumjelly","amazonpolymer","carbopolesc","talc")
      
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
      observeEvent(req(input$plodderbackpressuredegc),{
        eqn1 <- "76.4293896016496+-3.68749131760183*pkocontent()+0.0146038397207457*glycerine()+0.129132110568962*sodiumchloride()+-0.16205511182105*sodiumsulfate()+7.04084445503392*sodiumsilicate()+0.222746095064746*petrolatumjelly()+-4.00166431759117*amazonpolymer()+-35.5964551844198*carbopolesc()+-0.0109043309619179*talc()+0.0796945875198386*moisture()+-0.181260159211409*iv()+0.250970645173301*soapmasstemperature()+-0.0000661956994276951*flowrate()+0.0985345478859178*plodderbackpressuredegc()+-0.241325571790654*billetmoistur+(glycerine()-3.27272727272727)*((glycerine()-3.27272727272727)*0.0608438311745331)+(glycerine()-3.27272727272727)*((flowrate()-5719.86363636364)*0.0000098445456748929)+(sodiumchloride()-0.831818181818182)*((talc()-2.90909090909091)*-0.00446869632914622)+(sodiumchloride()-0.831818181818182)*((iv()-38.6136363636364)*-0.027000138479312)+(sodiumsilicate()-1.16686363636364)*((sodiumsilicate()-1.16686363636364)*0.291936080502523)+(sodiumsilicate()-1.16686363636364)*((billetmoistur-20.4986363636364)*-0.162669884515511)+(petrolatumjelly()-0.236363636363636)*((flowrate()-5719.86363636364)*-0.000263176092084823)+(petrolatumjelly()-0.236363636363636)*((plodderbackpressuredegc()-11.7522727272727)*0.145362088204255)+(petrolatumjelly()-0.236363636363636)*((billetmoistur-20.4986363636364)*0.255700486025522)+(amazonpolymer()-0.0838636363636363)*((soapmasstemperature()-41.5577272727273)*0.326798858596123)+(amazonpolymer()-0.0838636363636363)*((plodderbackpressuredegc()-11.7522727272727)*0.0446677847454846)+(amazonpolymer()-0.0838636363636363)*((billetmoistur-20.4986363636364)*-1.04095149545583)+(talc()-2.90909090909091)*((flowrate()-5719.86363636364)*-0.0000085461667984637)+(talc()-2.90909090909091)*((billetmoistur-20.4986363636364)*0.00485018468726219)+(moisture()-20.5656818181818)*((plodderbackpressuredegc()-11.7522727272727)*-0.0237189322663761)+(iv()-38.6136363636364)*((iv()-38.6136363636364)*0.0442814320739906)+(iv()-38.6136363636364)*((flowrate()-5719.86363636364)*0.0000197853904718027)+(soapmasstemperature()-41.5577272727273)*((soapmasstemperature()-41.5577272727273)*-0.0490857657173331)+(plodderbackpressuredegc()-11.7522727272727)*((plodderbackpressuredegc()-11.7522727272727)*-0.00438487333986625)"
        
        billetmoistur <- seq(from  = 14, to = 29, length.out = 16)
        plodderbackpressuredegc <- reactive(input$plodderbackpressuredegc)
        flowrate <- reactive(input$flowrate)
        soapmasstemperature <- reactive(input$soapmasstemperature)
        iv <- reactive(input$iv)
        moisture <- reactive(input$moisture)
        pkocontent <- reactive(input$pkocontent)
        glycerine <- reactive(input$glycerine)
        sodiumchloride <- reactive(input$sodiumchloride)
        sodiumsulfate <- reactive(input$sodiumsulfate)
        sodiumsilicate <- reactive(input$sodiumsilicate)
        petrolatumjelly <- reactive(input$petrolatumjelly)
        amazonpolymer <- reactive(input$amazonpolymer)
        carbopolesc <- reactive(input$carbopolesc)
        talc <- reactive(input$talc)
        

        observeEvent(input$billetmoistur | input$plodderbackpressuredegc |input$flowrate |input$soapmasstemperature |input$iv |
                       input$moisture | input$pkocontent |input$glycerine |input$sodiumchloride |input$sodiumsulfate |
                       input$sodiumsilicate | input$petrolatumjelly |input$amazonpolymer |input$carbopolesc |input$talc
                     ,{
                       soap_bar_hardness <- reactive(eval(parse(text = eqn1)))

                       output$plot1 <- renderPlot({
                         soap_bar_hardness <- round(soap_bar_hardness(),3)
                         ggplot(data=data.frame(billetmoistur, soap_bar_hardness), aes(x=billetmoistur, y= soap_bar_hardness)) +
                           geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+ xlab("Billet Moisture")+
                           ylab("Soap Bar Hardness")+
                           gghighlight(billetmoistur == input$billetmoistur, label_key = soap_bar_hardness)
                       })
                     })

      })
      
      
      
      #visualization page renderings
      observeEvent(req(input$datacall_uday_sd),
                   {data_uday_sd <- reactive(read_excel(input$datacall_uday_sd$datapath))

                   
                   updateSelectInput(session, "y_axis_sd", choices = colnames(data_uday_sd()), selected = colnames(data_uday_sd())[2])
                   updateSelectInput(session, "x_axis_sd", choices = colnames(data_uday_sd()))
                   updateSelectInput(session, "hist_choice_uday_sd", choices = colnames(data_uday_sd()))
                   
                   output$simulationdata_uday_sd <- renderDataTable(data_uday_sd())
                   
                   # output$scatterplot_uday_sd <- renderPlot(
                   #   if(length(input$y_axis_sd) == 1){
                   #     plot(x= data_uday_sd()[[input$x_axis_sd]], y = data_uday_sd()[[input$y_axis_sd]],
                   #          xlab = input$x_axis_sd, ylab = input$y_axis_sd, col = "blue")
                   #     abline(lm(data_uday_sd()[[input$y_axis_sd]]~data_uday_sd()[[input$x_axis_sd]]), col = "blue")
                   #   }
                   #   else{if(length(input$y_axis_sd) > 4){
                   #     showModal(modalDialog("Maximum 4 selections are allowed."))
                   #   }else{
                   #     plot(x= data_uday_sd()[[input$x_axis_sd]], y = data_uday_sd()[[input$y_axis_sd[1]]],
                   #          xlab = input$x_axis_sd, ylab = input$y_axis_sd, col = "blue")
                   #     abline(lm(data_uday_sd()[[input$y_axis_sd[1]]]~data_uday_sd()[[input$x_axis_sd]]), col = "blue")
                   #     for(i in 2:length(input$y_axis_sd)){
                   #       points(x= data_uday_sd()[[input$x_axis_sd]], y = data_uday_sd()[[input$y_axis_sd[i]]], col = colors[i])
                   #       abline(lm(data_uday_sd()[[input$y_axis_sd[i]]]~data_uday_sd()[[input$x_axis_sd]]), col = colors[i])
                   #     }
                   #   }
                   #   } )
                   
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
                   output$multi_lines_graph_uday <- renderPlotly({
                     if(length(input$y_axis) == 1){
                       fig <- plot_ly( x = ~data_uday_sd()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis]],mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = as.character(input$y_axis)) )
                       fig
                     }else if(length(input$y_axis) == 2){
                       fig <- plot_ly(data_uday_sd(), x = ~data_uday_sd()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     }
                     else if(length(input$y_axis) == 3){
                       fig <- plot_ly(data_uday_sd(), x = ~data_uday_sd()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 4){
                       fig <- plot_ly(data_uday_sd(), x = ~data_uday_sd()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 5){
                       fig <- plot_ly(data_uday_sd(), x = ~data_uday_sd()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[5]]], name = input$y_axis[5], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 6){
                       fig <- plot_ly(data_uday_sd(), x = ~data_uday_sd()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[5]]], name = input$y_axis[5], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data_uday_sd()[[input$y_axis[6]]], name = input$y_axis[6], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
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
            munits <- rep("-", length(b_uday_sd))
            values <- rep(1, length(b_uday_sd))
            sqr <- do.call(rbind,data.frame(cbind(munits,values)))
            # sqr <- data.frame(t(values))
            colnames(sqr) <- b_uday_sd
            rownames(sqr) <- c("Measurement Units","Enter Simulation Values")
            sqr
          })
          
          x1_uday_sd$df <- y_uday_sd()
        })

        output$simulation_input_uday_sd <- renderDataTable({ datatable(x1_uday_sd$df, editable = T) })
        
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
        
        observeEvent(input$simulate_uday_sd,{
          x2 <- x1_uday_sd$df
          colnames(x2) <- all_uday_sd
          
          eqn1 <- "76.4293896016496+-3.68749131760183*pkocontent+0.0146038397207457*glycerine+0.129132110568962*sodiumchloride+-0.16205511182105*sodiumsulfate+7.04084445503392*sodiumsilicate+0.222746095064746*petrolatumjelly+-4.00166431759117*amazonpolymer+-35.5964551844198*carbopolesc+-0.0109043309619179*talc+0.0796945875198386*moisture+-0.181260159211409*iv+0.250970645173301*soapmasstemperature+-0.0000661956994276951*flowrate+0.0985345478859178*plodderbackpressuredegc+-0.241325571790654*billetmoistur+(glycerine-3.27272727272727)*((glycerine-3.27272727272727)*0.0608438311745331)+(glycerine-3.27272727272727)*((flowrate-5719.86363636364)*0.0000098445456748929)+(sodiumchloride-0.831818181818182)*((talc-2.90909090909091)*-0.00446869632914622)+(sodiumchloride-0.831818181818182)*((iv-38.6136363636364)*-0.027000138479312)+(sodiumsilicate-1.16686363636364)*((sodiumsilicate-1.16686363636364)*0.291936080502523)+(sodiumsilicate-1.16686363636364)*((billetmoistur-20.4986363636364)*-0.162669884515511)+(petrolatumjelly-0.236363636363636)*((flowrate-5719.86363636364)*-0.000263176092084823)+(petrolatumjelly-0.236363636363636)*((plodderbackpressuredegc-11.7522727272727)*0.145362088204255)+(petrolatumjelly-0.236363636363636)*((billetmoistur-20.4986363636364)*0.255700486025522)+(amazonpolymer-0.0838636363636363)*((soapmasstemperature-41.5577272727273)*0.326798858596123)+(amazonpolymer-0.0838636363636363)*((plodderbackpressuredegc-11.7522727272727)*0.0446677847454846)+(amazonpolymer-0.0838636363636363)*((billetmoistur-20.4986363636364)*-1.04095149545583)+(talc-2.90909090909091)*((flowrate-5719.86363636364)*-0.0000085461667984637)+(talc-2.90909090909091)*((billetmoistur-20.4986363636364)*0.00485018468726219)+(moisture-20.5656818181818)*((plodderbackpressuredegc-11.7522727272727)*-0.0237189322663761)+(iv-38.6136363636364)*((iv-38.6136363636364)*0.0442814320739906)+(iv-38.6136363636364)*((flowrate-5719.86363636364)*0.0000197853904718027)+(soapmasstemperature-41.5577272727273)*((soapmasstemperature-41.5577272727273)*-0.0490857657173331)+(plodderbackpressuredegc-11.7522727272727)*((plodderbackpressuredegc-11.7522727272727)*-0.00438487333986625)"
          
          for(i in all_uday_sd){
            eqn1 <- gsub(i, x2[2,i], eqn1)
          }
          
          
          soap_bar_hardness <- eval(parse(text = eqn1))
          
          output$simulation_result_uday_sd <- renderUI({
            h3("Simulation Results")
          })
          
          
          tbl <- as.data.frame(soap_bar_hardness)
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
          colnames(df) <- gsub(" ","",tolower(colnames(df)))
          colnames(df) <- gsub("\\s*\\([^\\)]+\\)","",colnames(df))
          colnames(df) <- gsub("[[:digit:]]+","",colnames(df))
          colnames(df) <- gsub("[^[:alnum:] ]","",colnames(df))
          colnames(df) <- gsub("billetmoisture","billetmoistur",colnames(df))
          

          
          
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
            eqn1 <- "76.4293896016496+-3.68749131760183*df$ pkocontent+0.0146038397207457*df$ glycerine+0.129132110568962*df$ sodiumchloride+-0.16205511182105*df$ sodiumsulfate+7.04084445503392*df$ sodiumsilicate+0.222746095064746*df$ petrolatumjelly+-4.00166431759117*df$ amazonpolymer+-35.5964551844198*df$ carbopolesc+-0.0109043309619179*df$ talc+0.0796945875198386*df$ moisture+-0.181260159211409*df$ iv+0.250970645173301*df$ soapmasstemperature+-0.0000661956994276951*df$ flowrate+0.0985345478859178*df$ plodderbackpressuredegc+-0.241325571790654*df$ billetmoistur+(df$ glycerine-3.27272727272727)*((df$ glycerine-3.27272727272727)*0.0608438311745331)+(df$ glycerine-3.27272727272727)*((df$ flowrate-5719.86363636364)*0.0000098445456748929)+(df$ sodiumchloride-0.831818181818182)*((df$ talc-2.90909090909091)*-0.00446869632914622)+(df$ sodiumchloride-0.831818181818182)*((df$ iv-38.6136363636364)*-0.027000138479312)+(df$ sodiumsilicate-1.16686363636364)*((df$ sodiumsilicate-1.16686363636364)*0.291936080502523)+(df$ sodiumsilicate-1.16686363636364)*((df$ billetmoistur-20.4986363636364)*-0.162669884515511)+(df$ petrolatumjelly-0.236363636363636)*((df$ flowrate-5719.86363636364)*-0.000263176092084823)+(df$ petrolatumjelly-0.236363636363636)*((df$ plodderbackpressuredegc-11.7522727272727)*0.145362088204255)+(df$ petrolatumjelly-0.236363636363636)*((df$ billetmoistur-20.4986363636364)*0.255700486025522)+(df$ amazonpolymer-0.0838636363636363)*((df$ soapmasstemperature-41.5577272727273)*0.326798858596123)+(df$ amazonpolymer-0.0838636363636363)*((df$ plodderbackpressuredegc-11.7522727272727)*0.0446677847454846)+(df$ amazonpolymer-0.0838636363636363)*((df$ billetmoistur-20.4986363636364)*-1.04095149545583)+(df$ talc-2.90909090909091)*((df$ flowrate-5719.86363636364)*-0.0000085461667984637)+(df$ talc-2.90909090909091)*((df$ billetmoistur-20.4986363636364)*0.00485018468726219)+(df$ moisture-20.5656818181818)*((df$ plodderbackpressuredegc-11.7522727272727)*-0.0237189322663761)+(df$ iv-38.6136363636364)*((df$ iv-38.6136363636364)*0.0442814320739906)+(df$ iv-38.6136363636364)*((df$ flowrate-5719.86363636364)*0.0000197853904718027)+(df$ soapmasstemperature-41.5577272727273)*((df$ soapmasstemperature-41.5577272727273)*-0.0490857657173331)+(df$ plodderbackpressuredegc-11.7522727272727)*((df$ plodderbackpressuredegc-11.7522727272727)*-0.00438487333986625)"
            
            output$simulation_result_uday_sd2 <- renderUI({
              h3("Simulation Results")
            })
            
            soap_bar_hardness <- eval(parse(text = eqn1))
            
            nrdata <- as.data.frame(soap_bar_hardness)
            importresults(nrdata)
            
            # nrdata1 <- as.data.frame(df)
            output$download2_uday_sd <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
            
            
            output$modeltable2_uday_sd <- renderDataTable({
              DT::datatable(as.data.frame(soap_bar_hardness), rownames =FALSE)
            })
          })
          
        })
      })
      
      observeEvent(input$downloadresults_soap,{
        
        output$Download_Values_soap <- renderUI({
          ns <- session$ns
          downloadButton(ns("download_all_soap"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        
        nrdata <- as.data.frame(manual())
        nrdata1 <- as.data.frame(manualinput())
        nrdata2 <- as.data.frame(importresults())
        nrdata4 <- as.data.frame(optimise2_soap())

        
        output$download_all_soap <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2, "Optimisation Soap Hardness" = nrdata4), file)
          }
        )
      })
      
      #optimisation for hardness
      observeEvent(req(x_uday_sd),{
        predictor_names_sd <- c("Billet Moisture_[14.4,28.1]","Plodder Back Pressure (30%) @ 40 degC_[7.7,16.4]",
                                "Flow Rate (30%)_[3550,7770]","Soap Mass Temperature_[40,46.8]",
                                "IV (Iodine Value)_[36,42]","Moisture_[15.3,28]","PKO Content_[20,23.5]",
                                "Glycerine_[0,6]","Sodium Chloride_[0.5,1.2]","Sodium Sulfate_[0,1.5]",
                                "Sodium Silicate_[0,1.7]","Petrolatum Jelly_[0,0.8]","Amazon Polymer_[0,0.7]",
                                "Carbopole SC200 (100%)_[0,0.3]","Talc_[0,6]")
        zero_vector<-rep(1,length(predictor_names_sd))
        min_vector <- c(14.4,7.7,3550,40,36,15.3,20,0,.5,0,0,0,0,0,0)
        max_vector <- c(28.1,16.4,7770,46.8,42,28,23.5,6,1.2,1.5,1.7,.8,.7,.3,6)
        coef_data2 <- data.frame(cbind(predictor_names_sd,zero_vector,min_vector,max_vector))
        opt$tab_1 <- coef_data2
        opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
        opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
        
        #table 1
        output$optimiser_table1_hardness <- renderDataTable({
          DT::datatable(opt$tab_1,selection="none",editable=TRUE,
                        colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
        })
        
        #cell edit
        observeEvent(input$optimiser_table1_hardness_cell_edit,{
          info <- input$optimiser_table1_hardness_cell_edit
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
          DT::replaceData(proxy_hardness, rep, resetPaging = FALSE)
        })
        
        
        observeEvent(input$run_optimiser_hardness,{
          
          target_sd <- input$numeric_input_hardness
          inequality_selection_sd <- input$inequality_selection_hardness
          
          opt$tab_1[[2]] <- as.numeric(opt$tab_1[[2]])
          
          constraint <- function(x){
            
            equation <-  76.4293896016496 + -3.68749131760183 * x[7] +
              0.0146038397207457 * x[8] + 0.129132110568962 * x[9] + 
              -0.16205511182105 * x[10] + 7.04084445503392 * x[11] +
              0.222746095064746 * x[12] + -4.00166431759117 * x[13] + 
              -35.5964551844198 * x[14] + -0.0109043309619179 * x[15] + 0.0796945875198386 * x[6] + 
              -0.181260159211409 * x[5] + 0.250970645173301 * x[4] +
              -0.0000661956994276951 * x[3] +0.0985345478859178 * x[2] + 
              -0.241325571790654 * x[1] + (x[8] - 3.27272727272727) * ((x[8] -3.27272727272727) * 0.0608438311745331) +
              (x[8] - 3.27272727272727) * (( x[3] - 5719.86363636364) * 0.0000098445456748929) +
              (x[9] - 0.831818181818182) * ((x[15] - 2.90909090909091) * -0.00446869632914622) + 
              (x[9] - 0.831818181818182) * ((x[5] - 38.6136363636364) * -0.027000138479312) +
              (x[11] - 1.16686363636364) * ((x[11] - 1.16686363636364) * 0.291936080502523) + 
              (x[11] - 1.16686363636364) * ((x[1] - 20.4986363636364) * -0.162669884515511) + 
              (x[12] -0.236363636363636) * ((x[3] - 5719.86363636364) * -0.000263176092084823) +
              (x[12] - 0.236363636363636) * (( x[2] - 11.7522727272727) * 0.145362088204255) +
              (x[12] - 0.236363636363636) * ((x[1] - 20.4986363636364) * 0.255700486025522) + 
              (x[13] - 0.0838636363636363) * (( x[4] - 41.5577272727273) * 0.326798858596123) +
              (x[13] - 0.0838636363636363) * ((x[2] -11.7522727272727) * 0.0446677847454846) + 
              (x[13] - 0.0838636363636363) * ((x[1] - 20.4986363636364) * -1.04095149545583) +
              (x[15] -2.90909090909091) * ((x[3] - 5719.86363636364) * -0.0000085461667984637) + 
              (x[15] - 2.90909090909091) * ((x[1] -20.4986363636364) * 0.00485018468726219) + 
              (x[6] - 20.5656818181818) * (( x[2] - 11.7522727272727) * -0.0237189322663761) + 
              (x[5] - 38.6136363636364) * ((x[5] -38.6136363636364) * 0.0442814320739906) +
              (x[5] - 38.6136363636364) * ((x[3] - 5719.86363636364) * 0.0000197853904718027) + 
              (x[4] - 41.5577272727273) * ((x[4] -41.5577272727273) * -0.0490857657173331) + 
              (x[2] - 11.7522727272727) * (( x[2] - 11.7522727272727) * -0.00438487333986625 ) -target_sd  
            
            if(inequality_selection_sd=="less than or equal to"){
              return(equation)
            }
            
            else if(inequality_selection_sd=="greater than or equal to"){
              return(-1*equation)
            }
            
            else{
              return(c(equation-0.001,-1*equation-0.001))
            }
            
          }# constraint ends
          
          obj <- function(x){
            
            eq <- opt$tab_1[7,2]*x[7] + opt$tab_1[8,2]*x[8] + opt$tab_1[9,2]*x[9] +  opt$tab_1[10,2]*x[10] +
              opt$tab_1[11,2]*x[11] + opt$tab_1[12,2]*x[12] +  opt$tab_1[13,2]*x[13] +  opt$tab_1[14,2]*x[14] +
              opt$tab_1[15,2]*x[15] +  opt$tab_1[6,2]*x[6] + opt$tab_1[5,2]*x[5] +  opt$tab_1[4,2]*x[4] + 
              opt$tab_1[3,2]*x[3] +  opt$tab_1[2,2]*x[2] +  opt$tab_1[1,2]*x[1] +
              opt$tab_1[8,2]*x[8]*opt$tab_1[8,2]*x[8] + opt$tab_1[8,2]*x[8]*opt$tab_1[3,2]*x[3] +
              opt$tab_1[9,2]*x[9]*opt$tab_1[15,2]*x[15] + opt$tab_1[9,2]*x[9]*opt$tab_1[5,2]*x[5] + 
              opt$tab_1[11,2]*x[11]*opt$tab_1[11,2]*x[11] + opt$tab_1[11,2]*x[11]*opt$tab_1[1,2]*x[1] +
              opt$tab_1[12,2]*x[12]*opt$tab_1[3,2]*x[3] +  opt$tab_1[12,2]*x[12]*opt$tab_1[2,2]*x[2] + 
              opt$tab_1[12,2]*x[12]*opt$tab_1[1,2]*x[1] + opt$tab_1[13,2]*x[13]*opt$tab_1[4,2]*x[4] +
              opt$tab_1[13,2]*x[13]*opt$tab_1[2,2]*x[2] + opt$tab_1[13,2]*x[13]*opt$tab_1[1,2]*x[1] +     
              opt$tab_1[15,2]*x[15]*opt$tab_1[3,2]*x[3] +  opt$tab_1[15,2]*x[15]*opt$tab_1[1,2]*x[1] +
              opt$tab_1[6,2]*x[6]*opt$tab_1[2,2]*x[2] +  opt$tab_1[5,2]*x[5]*opt$tab_1[5,2]*x[5] + 
              opt$tab_1[5,2]*x[5]*opt$tab_1[3,2]*x[3] +  opt$tab_1[4,2]*x[4]*opt$tab_1[4,2]*x[4] +  
              opt$tab_1[2,2]*x[2]*opt$tab_1[2,2]*x[2]    
              
            
            if(input$radio_button_hardness=='min'){
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
          output$optimiser_table32_hardness <- renderDataTable({
            df<-data.frame(Predictors = c("Billet Moisture","Plodder Back Pressure (30%) @ 40 degC",
                                          "Flow Rate (30%)","Soap Mass Temperature",
                                          "IV (Iodine Value)","Moisture","PKO Content",
                                          "Glycerine","Sodium Chloride","Sodium Sulfate",
                                          "Sodium Silicate","Petrolatum Jelly","Amazon Polymer",
                                          "Carbopole SC200 (100%)","Talc"),
                           Value = round(res$solution,3)
            )
            DT::datatable(df,selection ="none",rownames = FALSE )
          })
          
          constraint_value <- function(x){
            
            return(76.4293896016496 + -3.68749131760183 * x[7] +
                     0.0146038397207457 * x[8] + 0.129132110568962 * x[9] + 
                     -0.16205511182105 * x[10] + 7.04084445503392 * x[11] +
                     0.222746095064746 * x[12] + -4.00166431759117 * x[13] + 
                     -35.5964551844198 * x[14] + -0.0109043309619179 * x[15] + 0.0796945875198386 * x[6] + 
                     -0.181260159211409 * x[5] + 0.250970645173301 * x[4] +
                     -0.0000661956994276951 * x[3] +0.0985345478859178 * x[2] + 
                     -0.241325571790654 * x[1] + (x[8] - 3.27272727272727) * ((x[8] -3.27272727272727) * 0.0608438311745331) +
                     (x[8] - 3.27272727272727) * (( x[3] - 5719.86363636364) * 0.0000098445456748929) +
                     (x[9] - 0.831818181818182) * ((x[15] - 2.90909090909091) * -0.00446869632914622) + 
                     (x[9] - 0.831818181818182) * ((x[5] - 38.6136363636364) * -0.027000138479312) +
                     (x[11] - 1.16686363636364) * ((x[11] - 1.16686363636364) * 0.291936080502523) + 
                     (x[11] - 1.16686363636364) * ((x[1] - 20.4986363636364) * -0.162669884515511) + 
                     (x[12] -0.236363636363636) * ((x[3] - 5719.86363636364) * -0.000263176092084823) +
                     (x[12] - 0.236363636363636) * (( x[2] - 11.7522727272727) * 0.145362088204255) +
                     (x[12] - 0.236363636363636) * ((x[1] - 20.4986363636364) * 0.255700486025522) + 
                     (x[13] - 0.0838636363636363) * (( x[4] - 41.5577272727273) * 0.326798858596123) +
                     (x[13] - 0.0838636363636363) * ((x[2] -11.7522727272727) * 0.0446677847454846) + 
                     (x[13] - 0.0838636363636363) * ((x[1] - 20.4986363636364) * -1.04095149545583) +
                     (x[15] -2.90909090909091) * ((x[3] - 5719.86363636364) * -0.0000085461667984637) + 
                     (x[15] - 2.90909090909091) * ((x[1] -20.4986363636364) * 0.00485018468726219) + 
                     (x[6] - 20.5656818181818) * (( x[2] - 11.7522727272727) * -0.0237189322663761) + 
                     (x[5] - 38.6136363636364) * ((x[5] -38.6136363636364) * 0.0442814320739906) +
                     (x[5] - 38.6136363636364) * ((x[3] - 5719.86363636364) * 0.0000197853904718027) + 
                     (x[4] - 41.5577272727273) * ((x[4] -41.5577272727273) * -0.0490857657173331) + 
                     (x[2] - 11.7522727272727) * (( x[2] - 11.7522727272727) * -0.00438487333986625 ))
          }
          # View(res$solution)
          # optimiser output table 2
          output$optimiser_table22_hardness <- renderDataTable({
            value1 <- round(constraint_value(res$solution),3)
            val <- data.frame(Predictors = c("Soap Bar Hardness"),
                              Value = as.data.frame(value1))
            
            DT::datatable(as.data.frame(round(constraint_value(res$solution),3)) 
                          ,rownames = c("Soap Bar Hardness"), colnames =c("Target variable", "Value"))
          })
          
          # optimiser output table 3
          if(input$radio_button_hardness=='min'){
            output$value_results_hardness<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(res$objective,3))
            })
          }
          else{
            output$value_results_hardness<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(-1*res$objective,3))
            })
            
          }
          
          downresults12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Soap Bar Hardness"), Predicted_or_Optimal_Value= constraint_value(res$solution))
          downdf12<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("Billet Moisture_[14.4,28.1]","Plodder Back Pressure (30%) @ 40 degC_[7.7,16.4]",
                                                                                    "Flow Rate (30%)_[3550,7770]","Soap Mass Temperature_[40,46.8]",
                                                                                    "IV (Iodine Value)_[36,42]","Moisture_[15.3,28]","PKO Content_[20,23.5]",
                                                                                    "Glycerine_[0,6]","Sodium Chloride_[0.5,1.2]","Sodium Sulfate_[0,1.5]",
                                                                                    "Sodium Silicate_[0,1.7]","Petrolatum Jelly_[0,0.8]","Amazon Polymer_[0,0.7]",
                                                                                    "Carbopole SC200 (100%)_[0,0.3]","Talc_[0,6]"),
                               Predicted_or_Optimal_Value=res$solution)
          
          if(input$radio_button_hardness=='min'){
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = res$objective)
          }
          else{
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = -1*res$objective)
          }
          
          final123 <- rbind(downresults12,downdf12,downopt12)
          #View(final123)
          
          optimise2_soap(final123)
          output$download5_soap <- downloadHandler(
            filename = function() { "Optimisation Soap Hardness.xlsx"},
            content = function(file) {
              write_xlsx(list("Optimisation Result" = final123), file)
            }
          )
        })#observeevent run optimiser ends
        
      })#observeevent opt end
      
      observeEvent(input$reset_hardness,{
        updateSelectInput(session,"inequality_selection_hardness",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_hardness",value = 3)
        updateRadioButtons(session,"radio_button_hardness",selected = "min")
        predictor_names_sd <- c("Billet Moisture_[14.4,28.1]","Plodder Back Pressure (30%) @ 40 degC_[7.7,16.4]",
                                "Flow Rate (30%)_[3550,7770]","Soap Mass Temperature_[40,46.8]",
                                "IV (Iodine Value)_[36,42]","Moisture_[15.3,28]","PKO Content_[20,23.5]",
                                "Glycerine_[0,6]","Sodium Chloride_[0.5,1.2]","Sodium Sulfate_[0,1.5]",
                                "Sodium Silicate_[0,1.7]","Petrolatum Jelly_[0,0.8]","Amazon Polymer_[0,0.7]",
                                "Carbopole SC200 (100%)_[0,0.3]","Talc_[0,6]")
        zero_vector<-rep(1,length(predictor_names_sd))
        min_vector <- c(14.4,7.7,3550,40,36,15.3,20,0,.5,0,0,0,0,0,0)
        max_vector <- c(28.1,16.4,7770,46.8,42,28,23.5,6,1.2,1.5,1.7,.8,.7,.3,6)
        coef_data2 <- data.frame(cbind(predictor_names_sd,zero_vector,min_vector,max_vector))
        opt$tab_1 <- coef_data2
        opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
        opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
      })
      
    }
  )}