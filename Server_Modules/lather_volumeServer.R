lather_volumeServer <- function(id, top_session){
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
      
      x_uday_sd <- data.frame(Models <- c("Lather volume = 147.116305492888 + 151.501186255172 *
      Sodium Chloride + -55.3215735730235 *Sodium Sulfate + -49.4762432407727 * Sodium Citrate + -41.4352614156827 *
Sodium Carbonate + -29.2685282290144 * Sodium Silicate + -11.1271782344154 *
 + -60.5999588575755 * Oxiflow-Oxiteno + 22.6390321970717 *
FFA_Combined + -623.653030175084 * Titanium Dioxide + -1010.43308194092 *
Tinopal CBS + 57.9687283373548 * Perfume + 7.23116429366333 *
IV (Iodine Value) + (Sodium Chloride - 1.17222222222222) * ((Sodium Chloride
 - 1.17222222222222) * 424.75315555422) + (Sodium Chloride - 1.17222222222222) * ((
Sodium Citrate - 1.70777777777778) * -134.087470076663) + (Sodium Chloride
-1.17222222222222) * ((FFA_Combined - 0.144444444444444) * 92.3627549149173) + (
Sodium Sulfate - 0.276666666666667) * ((Tinopal CBS - 0.0223333333333333) *
4746.07002288773) + (Sodium Citrate - 1.70777777777778) * ((Sodium Citrate
-1.70777777777778) * 21.1314669748662) + (Sodium Citrate - 1.70777777777778) * ((
Titanium Dioxide - 0.360677777777777) * -93.7758844249979) + (Sodium Citrate
-1.70777777777778) * ((Tinopal CBS - 0.0223333333333333) * 2947.53075692333) + (
Sodium Citrate - 1.70777777777778) * ((IV (Iodine Value) - 39.4888888888889) *
2.6018160032659) + (Sodium Silicate - 0.3508) * ((FFA_Combined - 0.144444444444444
) * 51.0250540480239) + (Sodium Silicate - 0.3508) * ((Perfume - 1.00844444444444)
 * 69.3978683182652) + (PKO Content - 4.18300653594772) * ((IV (Iodine Value)
 - 39.4888888888889) * -0.135942835190774) + (Perfume - 1.00844444444444) * ((
Perfume - 1.00844444444444) * 43.6427684250721)"
      ))  
      
      b_uday_sd <- c("Sodium Chloride","Sodium Sulfate","Sodium Silicate","FFA_Combined",
                     "Oxiflow-Oxiteno","Sodium Carbonate","Sodium Citrate","PKO Content",
                     "Titanium Dioxide","Tinopal CBS","Perfume","IV (Iodine Value)")
    
      all_uday_sd <- c("sodiumchloride","sodiumsulfate","sodiumsilicate","ffacombined",
                       "oxiflowoxiteno","sodiumcarbonate","sodiumcitrate","pkocontent",
                       "titaniumdioxide","tinopalcbs","perfume","iv")
      
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
      observeEvent(req(input$perfume),{
        eqn <- "147.116305492888+151.501186255172*sodiumchloride()+-55.3215735730235*sodiumsulfate()+-49.4762432407727*sodiumcitrate()+-41.4352614156827*sodiumcarbonate()+-29.2685282290144*sodiumsilicate()+-11.1271782344154*pkocontent()+-60.5999588575755*oxiflowoxiteno()+22.6390321970717*ffacombined()+-623.653030175084*titaniumdioxide()+-1010.43308194092*tinopalcbs()+57.9687283373548*perfume+7.23116429366333*iv()+(sodiumchloride()-1.17222222222222)*((sodiumchloride()-1.17222222222222)*424.75315555422)+(sodiumchloride()-1.17222222222222)*((sodiumcitrate()-1.70777777777778)*-134.087470076663)+(sodiumchloride()-1.17222222222222)*((ffacombined()-0.144444444444444)*92.3627549149173)+(sodiumsulfate()-0.276666666666667)*((tinopalcbs()-0.0223333333333333)*4746.07002288773)+(sodiumcitrate()-1.70777777777778)*((sodiumcitrate()-1.70777777777778)*21.1314669748662)+(sodiumcitrate()-1.70777777777778)*((titaniumdioxide()-0.360677777777777)*-93.7758844249979)+(sodiumcitrate()-1.70777777777778)*((tinopalcbs()-0.0223333333333333)*2947.53075692333)+(sodiumcitrate()-1.70777777777778)*((iv()-39.4888888888889)*2.6018160032659)+(sodiumsilicate()-0.3508)*((ffacombined()-0.144444444444444)*51.0250540480239)+(sodiumsilicate()-0.3508)*((perfume-1.00844444444444)*69.3978683182652)+(pkocontent()-4.18300653594772)*((iv()-39.4888888888889)*-0.135942835190774)+(perfume-1.00844444444444)*((perfume-1.00844444444444)*43.6427684250721)"
        
        perfume <- seq(from  = 0, to = 99, length.out = 100)
        sodiumchloride <- reactive(input$sodiumchloride)
        sodiumsulfate <- reactive(input$sodiumsulfate)
        sodiumsilicate <- reactive(input$sodiumsilicate)
        ffacombined <- reactive(input$ffacombined)
        oxiflowoxiteno <- reactive(input$oxiflowoxiteno)
        sodiumcarbonate <- reactive(input$sodiumcarbonate)
        sodiumcitrate <- reactive(input$sodiumcitrate)
        pkocontent <- reactive(input$pkocontent)
        titaniumdioxide <- reactive(input$titaniumdioxide)
        tinopalcbs <- reactive(input$tinopalcbs)
        iv <- reactive(input$iv)
        
        observeEvent(input$perfume | input$sodiumchloride | input$oxiflowoxiteno | input$sodiumcarbonate |
                       input$sodiumsulfate | input$sodiumsilicate | input$sodiumcitrate | input$iv |
                       input$ffacombined | input$pkocontent | input$titaniumdioxide | input$tinopalcbs
                     ,{
                       Lather_volume <- reactive(eval(parse(text = eqn)))
                       
                       output$plot <- renderPlot({
                         Lather_volume <- Lather_volume()
                         ggplot(data=data.frame(perfume, Lather_volume), aes(x=perfume, y= Lather_volume)) +
                           geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 15))+
                           gghighlight(perfume == input$perfume)
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

        })
        
        
        
        output$modeltable_uday_sd <- renderDataTable(
          datatable(x1_uday_sd$df)
        )
        
        observeEvent(input$simulate_uday_sd,{
          x2 <- x1_uday_sd$df
          colnames(x2) <- all_uday_sd
          
          eqn <- "147.116305492888+151.501186255172*sodiumchloride+-55.3215735730235*sodiumsulfate+-49.4762432407727*sodiumcitrate+-41.4352614156827*sodiumcarbonate+-29.2685282290144*sodiumsilicate+-11.1271782344154*pkocontent+-60.5999588575755*oxiflowoxiteno+22.6390321970717*ffacombined+-623.653030175084*titaniumdioxide+-1010.43308194092*tinopalcbs+57.9687283373548*perfume+7.23116429366333*iv+(sodiumchloride-1.17222222222222)*((sodiumchloride-1.17222222222222)*424.75315555422)+(sodiumchloride-1.17222222222222)*((sodiumcitrate-1.70777777777778)*-134.087470076663)+(sodiumchloride-1.17222222222222)*((ffacombined-0.144444444444444)*92.3627549149173)+(sodiumsulfate-0.276666666666667)*((tinopalcbs-0.0223333333333333)*4746.07002288773)+(sodiumcitrate-1.70777777777778)*((sodiumcitrate-1.70777777777778)*21.1314669748662)+(sodiumcitrate-1.70777777777778)*((titaniumdioxide-0.360677777777777)*-93.7758844249979)+(sodiumcitrate-1.70777777777778)*((tinopalcbs-0.0223333333333333)*2947.53075692333)+(sodiumcitrate-1.70777777777778)*((iv-39.4888888888889)*2.6018160032659)+(sodiumsilicate-0.3508)*((ffacombined-0.144444444444444)*51.0250540480239)+(sodiumsilicate-0.3508)*((perfume-1.00844444444444)*69.3978683182652)+(pkocontent-4.18300653594772)*((iv-39.4888888888889)*-0.135942835190774)+(perfume-1.00844444444444)*((perfume-1.00844444444444)*43.6427684250721)"
          for(i in all_uday_sd){
            eqn <- gsub(i, x2[1,i], eqn)
          }
          
          
          Lather_volume <- eval(parse(text = eqn))
          
          output$simulation_result_uday_sd <- renderUI({
            h3("Simulation Results")
          })
          
          
          tbl <- as.data.frame(Lather_volume)
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
            eqn1 <- "147.116305492888+151.501186255172*sodiumchloride+-55.3215735730235*sodiumsulfate+-49.4762432407727*sodiumcitrate+-41.4352614156827*sodiumcarbonate+-29.2685282290144*sodiumsilicate+-11.1271782344154*pkocontent+-60.5999588575755*oxiflowoxiteno+22.6390321970717*ffacombined+-623.653030175084*titaniumdioxide+-1010.43308194092*tinopalcbs+57.9687283373548*perfume+7.23116429366333*iv+(sodiumchloride-1.17222222222222)*((sodiumchloride-1.17222222222222)*424.75315555422)+(sodiumchloride-1.17222222222222)*((sodiumcitrate-1.70777777777778)*-134.087470076663)+(sodiumchloride-1.17222222222222)*((ffacombined-0.144444444444444)*92.3627549149173)+(sodiumsulfate-0.276666666666667)*((tinopalcbs-0.0223333333333333)*4746.07002288773)+(sodiumcitrate-1.70777777777778)*((sodiumcitrate-1.70777777777778)*21.1314669748662)+(sodiumcitrate-1.70777777777778)*((titaniumdioxide-0.360677777777777)*-93.7758844249979)+(sodiumcitrate-1.70777777777778)*((tinopalcbs-0.0223333333333333)*2947.53075692333)+(sodiumcitrate-1.70777777777778)*((iv-39.4888888888889)*2.6018160032659)+(sodiumsilicate-0.3508)*((ffacombined-0.144444444444444)*51.0250540480239)+(sodiumsilicate-0.3508)*((perfume-1.00844444444444)*69.3978683182652)+(pkocontent-4.18300653594772)*((iv-39.4888888888889)*-0.135942835190774)+(perfume-1.00844444444444)*((perfume-1.00844444444444)*43.6427684250721)"
            
            
            for(i in all_uday_sd){
              eqn1 <- gsub(i, paste0("df$",i), eqn1)
            }
            
            output$simulation_result_uday_sd2 <- renderUI({
              h3("Simulation Results")
            })
            
            Lather_volume <- eval(parse(text = eqn1))
            
            nrdata <- as.data.frame(Lather_volume)
            importresults(nrdata)
            
            # nrdata1 <- as.data.frame(df)
            output$download2_uday_sd <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
            
            
            output$modeltable2_uday_sd <- renderDataTable({
              DT::datatable(as.data.frame(Lather_volume), rownames =FALSE)
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


