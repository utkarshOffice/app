Soap_hardnessServer <- function(id, top_session){
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
        
        billetmoistur <- seq(from  = 1, to = 100, length.out = 100)
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
                         soap_bar_hardness <- soap_bar_hardness()
                         ggplot(data=data.frame(billetmoistur, soap_bar_hardness), aes(x=billetmoistur, y= soap_bar_hardness)) +
                           geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+ xlab("Billet Moisture")+
                           ylab("Soap Bar Hardness")+
                           gghighlight(billetmoistur == input$billetmoistur)
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
            
            values <- rep(1, length(b_uday_sd))
            sqr <- data.frame(t(values))
            colnames(sqr) <- b_uday_sd
            rownames(sqr) <- c("Enter Simulation Values")
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
            eqn1 <- gsub(i, x2[1,i], eqn1)
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
      
      observeEvent(input$downloadresults_uday_sd,{
        
        output$Download_Values_uday_sd <- renderUI({
          ns <- session$ns
          downloadButton(ns("download_all_uday_sd"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        
        nrdata <- as.data.frame(manual())
        nrdata1 <- as.data.frame(manualinput())
        nrdata2 <- as.data.frame(importresults())

        
        output$download_all_uday_sd <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2), file)
          }
        )
      })
    }
  )
}