conditionerServer <- function(id,top_session){
  moduleServer(
    id,
    function(input, output, session) {
      manualinput <- reactiveVal(NULL)
      manual <- reactiveVal(NULL)
      importresults <- reactiveVal(NULL)
      optimise <- reactiveVal(NULL)
      optimise1 <- reactiveVal(NULL)
      optimise2 <- reactiveVal(NULL)
      weight_one <-reactiveVal(NULL)
      weight_two <-reactiveVal(NULL)
      weight_three <-reactiveVal(NULL)
      weight_four <-reactiveVal(NULL)
      target_one <- reactiveVal(NULL)
      target_two <- reactiveVal(NULL)
      target_three <- reactiveVal(NULL)
      target_four <- reactiveVal(NULL)
      opt <- reactiveValues(tab_2=NULL)
      erin_proxy <- DT::dataTableProxy('optimiser_table1_erin')

      # go to simulation
      observeEvent(input$commit,{
        updateTabsetPanel(top_session,"tab_id", selected = "Simulation")
      })
      # go to visualization
      observeEvent(input$commit2,{
        updateTabsetPanel(top_session, "tab_id", selected = "Visualization")
      })
      
      
       # dataframe with conditioner model equations
      models_df <- {data.frame(Models <- c("Fresh Viscosity [mPas] = (-1.90929008775433) + 293.747340146854 * Solids Content + 0.0185866446926401 *
    ( Quench Injection Rate [%kg/hr] ) + -0.221460924788289 *
      ( Final Mixing Time [mins] ) + 1.64713604618159 *
      ( Silverson Tip Speed on Discharge [m/s] ) + -2.25165790211731 *
      ( Temp of Samples on Discharge [C] ) + -0.0301432004945295 *
      ( Fats Injection Rate [%kg/hr] ) + 0.911630864596123 *
      ( Updated Fats Temp [C] ) + ( ( Final Mixing Time [mins] )
                                    -24.0511031746786) * ( ( Temp of Samples on Discharge [C] ) - 40.3160952380953
                                    ) * -0.350152923119871 + (Solids Content - 0.889285714285715) * (
                                      ( Fats Injection Rate [%kg/hr] ) - 58.5077556918568) * 5.69749029830664",
                                                       
                                                       
                                                       "24 Hour Viscosity [mPas] = (-63.1284043740373) + 360.506718384543 * Solids Content + 0.0521562257371331 *
      ( Silverson Tip Speed During Quench [m/s] ) + 0.0353296119308415 *
      ( Silverson Tip Speed During End Mixing [m/s] ) + 3.27376326998214 *
      ( Silverson Tip Speed on Discharge [m/s] ) + 2.07763992027492 *
      ( Updated Fats Temp [C] ) + ( (
        Silverson Tip Speed During End Mixing [m/s]
      ) - 11.690086036606) * ( ( Silverson Tip Speed During End Mixing [m/s] )
                               -11.690086036606) * -0.313650248085591 + ( (
                                 Silverson Tip Speed During Quench [m/s]
                               ) - 10.5377695693782) * ( ( Silverson Tip Speed on Discharge [m/s] )
                                                         -11.6316630241121) * 0.0468925549996396",
                                                       
                                                       
                                                       "24 Hour Yield Stress [mPas] = 8.93621729663531 + 196.247634160075 * Solids Content + 0.39712728468703 *
      ( Silverson Tip Speed During End Mixing [m/s] ) + 0.709966849477569 *
      ( Silverson Tip Speed on Discharge [m/s] ) + 0.169393285431812 *
      ( Fats Injection Rate [%kg/hr] ) + -1.60593890352765 *
      ( Temp at Point of Quench Injection [C] ) + (Solids Content
                                                   -0.889285714285715) * ( ( Fats Injection Rate [%kg/hr] ) - 58.5077556918568)
    * 6.46419044816413 + ( ( Temp at Point of Quench Injection [C] )
                           -56.6174285714286) * ( ( Temp at Point of Quench Injection [C] )
                                                  -56.6174285714286) * -1.86801925904818",
                                                       
                                                       
                                                       "1 Week Viscosity [mPas] = 632.871957397762 + 353.212348019952 * Solids Content + 1.85300874016657 *
      ( Silverson Tip Speed During Emulsion [m/s] ) + -0.0492383896885823 *
      ( Silverson Tip Speed During Quench [m/s] ) + 0.15817538864863 *
      ( Silverson Tip Speed During End Mixing [m/s] ) + 2.66081552162148 *
      ( Silverson Tip Speed on Discharge [m/s] ) + -0.233420366808025 *
      ( Fats Injection Rate [%kg/hr] ) + -8.57117901092962 *
      ( Temp at Point of Quench Injection [C] ) + (
        ( Silverson Tip Speed During Quench [m/s] ) - 10.5377695693782) * (
          ( Silverson Tip Speed During Quench [m/s] ) - 10.5377695693782) *
      -0.443509372083955 + (Solids Content - 0.889285714285715) * (
        ( Silverson Tip Speed During End Mixing [m/s] ) - 11.690086036606) *
      -14.3823974090446 + (Solids Content - 0.889285714285715) * (
        ( Fats Injection Rate [%kg/hr] ) - 58.5077556918568) * 15.7785498295843 + (
          ( Fats Injection Rate [%kg/hr] ) - 58.5077556918568) * (
            ( Temp at Point of Quench Injection [C] ) - 56.6174285714286) *
      -0.621838628323516"))}
      
      
      output$models <- renderDataTable({
        datatable(models_df, colnames = c("Conditioner Models"))
      })
      
      # profiler renderings
      observeEvent(req(input$solidscontent),
                   {  
                     fresh_vis <- "(-1.90929008775433)+293.747340146854*solidscontent+0.0185866446926401*(quenchinjectionrate())+-0.221460924788289*(finalmixingtime())+1.64713604618159*(silversontipspeedondischarge())+-2.25165790211731*(tempofsamplesondischarge())+-0.0301432004945295*(fatsinjectionrate())+0.911630864596123*(updatedfatstemp())+((finalmixingtime())-24.0511031746786)*((tempofsamplesondischarge())-40.3160952380953)*-0.350152923119871+(solidscontent-0.889285714285715)*((fatsinjectionrate())-58.5077556918568)*5.69749029830664" 
                     day_vis <- "(-63.1284043740373)+360.506718384543*solidscontent+0.0521562257371331*(silversontipspeedduringquench())+0.0353296119308415*(silversontipspeedduringendmixing())+3.27376326998214*(silversontipspeedondischarge())+2.07763992027492*(updatedfatstemp())+((silversontipspeedduringendmixing())-11.690086036606)*((silversontipspeedduringendmixing())-11.690086036606)*-0.313650248085591+((silversontipspeedduringquench())-10.5377695693782)*((silversontipspeedondischarge())-11.6316630241121)*0.0468925549996396" 
                     day_yield_press <-  "8.93621729663531+196.247634160075*solidscontent+0.39712728468703*(silversontipspeedduringendmixing())+0.709966849477569*(silversontipspeedondischarge())+0.169393285431812*(fatsinjectionrate())+-1.60593890352765*(tempatpointofquenchinjection())+(solidscontent-0.889285714285715)*((fatsinjectionrate())-58.5077556918568)*6.46419044816413+((tempatpointofquenchinjection())-56.6174285714286)*((tempatpointofquenchinjection())-56.6174285714286)*-1.86801925904818" 
                     week_vis <-   "632.871957397762+353.212348019952*solidscontent+1.85300874016657*(silversontipspeedduringemulsion())+-0.0492383896885823*(silversontipspeedduringquench())+0.15817538864863*(silversontipspeedduringendmixing())+2.66081552162148*(silversontipspeedondischarge())+-0.233420366808025*(fatsinjectionrate())+-8.57117901092962*(tempatpointofquenchinjection())+((silversontipspeedduringquench())-10.5377695693782)*((silversontipspeedduringquench())-10.5377695693782)*-0.443509372083955+(solidscontent-0.889285714285715)*((silversontipspeedduringendmixing())-11.690086036606)*-14.3823974090446+(solidscontent-0.889285714285715)*((fatsinjectionrate())-58.5077556918568)*15.7785498295843+((fatsinjectionrate())-58.5077556918568)*((tempatpointofquenchinjection())-56.6174285714286)*-0.621838628323516"
                     
                     solidscontent <- round(seq(from  = 0.85, to = 1, length.out = 100),3)
                     silversontipspeedduringemulsion <- reactive(input$silversontipspeedduringemulsion)
                     silversontipspeedduringquench <- reactive(input$silversontipspeedduringquench)
                     silversontipspeedduringendmixing <- reactive(input$silversontipspeedduringendmixing)
                     silversontipspeedondischarge <- reactive(input$silversontipspeedondischarge)
                     updatedfatstemp <- reactive(input$updatedfatstemp)
                     fatsinjectionrate <- reactive(input$fatsinjectionrate)
                     tempatpointofquenchinjection <- reactive(input$tempatpointofquenchinjection)
                     quenchinjectionrate <- reactive(input$quenchinjectionrate)
                     finalmixingtime <- reactive(input$finalmixingtime)
                     tempofsamplesondischarge <- reactive(input$tempofsamplesondischarge)
                     
                     observeEvent(input$silversontipspeedduringemulsion | input$silversontipspeedduringquench | 
                                    input$silversontipspeedduringendmixing | input$silversontipspeedondischarge | 
                                    input$updatedfatstemp | input$fatsinjectionrate | input$tempatpointofquenchinjection | 
                                    input$quenchinjectionrate | input$finalmixingtime | input$tempofsamplesondischarge | input$solidscontent,{
                                      
                                      freshvis_result <- reactive(eval(parse(text = fresh_vis)))
                                      dayvis_result <- reactive(eval(parse(text = day_vis)))
                                      dayield_result <- reactive(eval(parse(text = day_yield_press)))
                                      weekvis_result <- reactive(eval(parse(text = week_vis)))
                                      
                                      output$plot1 <- renderPlot({
                                        freshvis_result <- freshvis_result()
                                        ggplot(data=data.frame(solidscontent, freshvis_result), aes(x=solidscontent, y= freshvis_result)) +
                                          geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                                          xlab("Solids content") + ylab("Fresh Viscosity [mPas]")+
                                          gghighlight(solidscontent == input$solidscontent)
                                      })
                                      
                                      output$plot2 <- renderPlot({
                                        dayvis_result <- dayvis_result()
                                        ggplot(data=data.frame(solidscontent, dayvis_result), aes(x=solidscontent, y= dayvis_result)) +
                                          geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                                          xlab("Solids content") + ylab("24 Hour Viscosity [mPas]")+
                                        gghighlight(solidscontent == input$solidscontent)
                                      })

                                      output$plot3 <- renderPlot({
                                        dayield_result <- dayield_result()
                                        ggplot(data=data.frame(solidscontent, dayield_result), aes(x=solidscontent, y= dayield_result)) +
                                          geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                                          xlab("Solids content") + ylab("24 Hour Yield Stress [mPas]")+
                                        gghighlight(solidscontent == input$solidscontent)
                                      })

                                      output$plot4 <- renderPlot({
                                        weekvis_result <- weekvis_result()
                                        ggplot(data=data.frame(solidscontent, weekvis_result), aes(x=solidscontent, y= weekvis_result)) +
                                          geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                                          xlab("Solids content") + ylab("1 Week Viscosity [mPas]")+
                                        gghighlight(solidscontent == input$solidscontent)
                                      })
                                      })
 
                   })
      
      
      
      # dataframe with user input limits suggestion
      output$advice <- renderDataTable({
        datatable(data.frame(Materials <- c("Solids Content","Silverson Tip Speed during Emulsion [m/s]","Silverson Tip Speed during Quench  [m/s]",
                                            "Silverson Tip Speed during End Mixing [m/s]","Silverson Tip Speed on Discharge [m/s]",
                                            "Updated Fats Temperature [C]","Fats Injection Rate [%kg/hr]","Temp at point of Quench [C]",
                                            "Quench Injection Rate [%kg/hr]","Final Mixing Time [mins]","Temperature of samples on discharge [C]"),
                             Lower_Limit <- c("0.85","7.76","0.00","0.00","0.00","59.7","33.54","53.94","164.06","8.08","38.34"),
                             Upper_Limit <- c("1.00", "23.26", "23.26", "23.26", "23.26","77.70", "117.51", "59.77", "837.99", "46.05", "42.65")),
                  colnames = c("Materials","Lower Limit","Upper Limit"))
      })
      
      observeEvent(req(models_df),{
        reactive_conditioner <- reactiveValues()
        
        
        observe({
          temp_conditioner <- reactive({
            con_munits <- c("-","[m/s]","[m/s]","[m/s]","[m/s]","[C]","[%kg/hr]","[C]","[%kg/hr]","[mins]","[C]")
            con_values <- rep(0, length(con_munits))
            con <- do.call(rbind,data.frame(cbind(con_munits,con_values)))
            colnames(con) <- c("Solids Content","Silverson Tip Speed during Emulsion","Silverson Tip Speed During Quench",
                               "Silverson Tip Speed during End Mixing","Silverson Tip Speed on Discharge",
                               "Updated Fats Temp","Fats Injection Rate","Temp at point of Quench Injection",
                               "Quench Injection Rate","Final Mixing Time","Temp of Samples on discharge")
            rownames(con) <- c("Measurement Units", "Enter Simulation Values")
            con
          })
          reactive_conditioner$df <- temp_conditioner()
        })
        
        
        
        
        output$simulation_input <- renderDataTable({ 
          datatable(reactive_conditioner$df, editable = T,
                    colnames = c("Solids Content","Silverson Tip Speed during Emulsion","Silverson Tip Speed During Quench",
                                 "Silverson Tip Speed during End Mixing","Silverson Tip Speed on Discharge",
                                 "Updated Fats Temp","Fats Injection Rate","Temp at point of Quench Injection",
                                 "Quench Injection Rate","Final Mixing Time","Temp of Samples on discharge"))
        })
        
        proxy <- dataTableProxy("simulation_input")
        
        
        
        
        observeEvent(input[["simulation_input_cell_edit"]], {
          con_info <- input[["simulation_input_cell_edit"]]
          
          con_i <- con_info$row
          con_j <- con_info$col
          con_v <- con_info$value
          
          if(!is.na(con_v) && !is.na(as.numeric(con_v))){
            v <- as.numeric(con_v)
            reactive_conditioner$df[con_i, con_j] <- DT::coerceValue(con_v,reactive_conditioner$df[con_i, con_j])}
          
          con_rep <- reactive_conditioner$df
          DT::replaceData(proxy, con_rep, resetPaging = FALSE)
        })
        
        
        
        
        output$modeltable <- renderDataTable(
          {
            datatable(reactive_conditioner$df,
                      colnames = c("Solids Content","Silverson Tip Speed during Emulsion","Silverson Tip Speed During Quench",
                                   "Silverson Tip Speed during End Mixing","Silverson Tip Speed on Discharge",
                                   "Updated Fats Temp","Fats Injection Rate","Temp at point of Quench Injection",
                                   "Quench Injection Rate","Final Mixing Time","Temp of Samples on discharge"))
          }
        )
        
        
        
        
        observeEvent(input$simulate,{
          fresh_vis <- "(-1.90929008775433)+293.747340146854*solidscontent+0.0185866446926401*(quenchinjectionrate)+-0.221460924788289*(finalmixingtime)+1.64713604618159*(silversontipspeedondischarge)+-2.25165790211731*(tempofsamplesondischarge)+-0.0301432004945295*(fatsinjectionrate)+0.911630864596123*(updatedfatstemp)+((finalmixingtime)-24.0511031746786)*((tempofsamplesondischarge)-40.3160952380953)*-0.350152923119871+(solidscontent-0.889285714285715)*((fatsinjectionrate)-58.5077556918568)*5.69749029830664" 
          day_vis <- "(-63.1284043740373)+360.506718384543*solidscontent+0.0521562257371331*(silversontipspeedduringquench)+0.0353296119308415*(silversontipspeedduringendmixing)+3.27376326998214*(silversontipspeedondischarge)+2.07763992027492*(updatedfatstemp)+((silversontipspeedduringendmixing)-11.690086036606)*((silversontipspeedduringendmixing)-11.690086036606)*-0.313650248085591+((silversontipspeedduringquench)-10.5377695693782)*((silversontipspeedondischarge)-11.6316630241121)*0.0468925549996396" 
          day_yield_press <-  "8.93621729663531+196.247634160075*solidscontent+0.39712728468703*(silversontipspeedduringendmixing)+0.709966849477569*(silversontipspeedondischarge)+0.169393285431812*(fatsinjectionrate)+-1.60593890352765*(tempatpointofquenchinjection)+(solidscontent-0.889285714285715)*((fatsinjectionrate)-58.5077556918568)*6.46419044816413+((tempatpointofquenchinjection)-56.6174285714286)*((tempatpointofquenchinjection)-56.6174285714286)*-1.86801925904818" 
          week_vis <-   "632.871957397762+353.212348019952*solidscontent+1.85300874016657*(silversontipspeedduringemulsion)+-0.0492383896885823*(silversontipspeedduringquench)+0.15817538864863*(silversontipspeedduringendmixing)+2.66081552162148*(silversontipspeedondischarge)+-0.233420366808025*(fatsinjectionrate)+-8.57117901092962*(tempatpointofquenchinjection)+((silversontipspeedduringquench)-10.5377695693782)*((silversontipspeedduringquench)-10.5377695693782)*-0.443509372083955+(solidscontent-0.889285714285715)*((silversontipspeedduringendmixing)-11.690086036606)*-14.3823974090446+(solidscontent-0.889285714285715)*((fatsinjectionrate)-58.5077556918568)*15.7785498295843+((fatsinjectionrate)-58.5077556918568)*((tempatpointofquenchinjection)-56.6174285714286)*-0.621838628323516"
          
          colnames(reactive_conditioner$df) <- gsub(" ","",tolower(colnames(reactive_conditioner$df)))
          
          for(i in colnames(reactive_conditioner$df)){
            fresh_vis <- gsub(i,reactive_conditioner$df[2,i],fresh_vis)
            day_vis <- gsub(i,reactive_conditioner$df[2,i],day_vis)
            day_yield_press <- gsub(i,reactive_conditioner$df[2,i],day_yield_press)
            week_vis <- gsub(i,reactive_conditioner$df[2,i],week_vis)
          }
          
          freshvis_result <- eval(parse(text = fresh_vis))
          dayvis_result <- eval(parse(text = day_vis))
          dayield_result <- eval(parse(text = day_yield_press))
          weekvis_result <- eval(parse(text = week_vis))
          
          tbl <- as.data.frame(cbind(freshvis_result,dayvis_result,dayield_result,weekvis_result))
          df <-  reactive_conditioner$df
          nrdata <- as.data.frame(tbl)
          nrdata1 <- as.data.frame(df)
          
          manualinput(df)
          manual(tbl)
          output$download1 <- downloadHandler(
            filename = function() { "Manual Entry Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
            }
          )
          output$result1 <- renderDataTable({
            datatable(data.frame(freshvis_result,dayvis_result,dayield_result,weekvis_result),
                      colnames = c("Fresh Viscosity [Pa s]","24 hr Viscosity [Pa s]","24 hr Viscosity [Pa s]","1 Week Viscosity [Pa s]"))
          })
          
          output$Simulation_Result <- renderUI(
            h3("Simulation Result")
          )
          
          
          output$download <- downloadHandler(
            filename = function() {
              paste0("Simulation_Result", ".csv")
            },
            content = function(file) {
              write.csv( data.frame(freshvis_result,dayvis_result,dayield_result,weekvis_result)
                        , file)
            }
          )
          })

        inputdata <- reactive({
          req(input$input)
          inFile <- input$input
          if(is.null(inFile)) return(NULL)
          
          xlfile <- read_excel(input$input$datapath)
          colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
          colnames(xlfile) <- gsub(" ","",colnames(xlfile))
          colnames(xlfile) <- tolower(colnames(xlfile))
          xlfile
        })
        
        col_vars <- c("solidscontent","silversontipspeedduringemulsion" ,"silversontipspeedduringquench","silversontipspeedduringendmixing","silversontipspeedondischarge","updatedfatstemp", "fatsinjectionrate", "tempatpointofquenchinjection", "quenchinjectionrate", "finalmixingtime", "tempofsamplesondischarge" )
        
        observeEvent(inputdata(),
                     { i <- col_vars[!is.element(col_vars,colnames(inputdata()))]
                     if(length(i)>=1){
                       showModal(modalDialog(paste0(as.character(str_c(i,collapse = ",")), " is not present in imported data but required in equation. 0 is placed in the place of missing variables to render the respective coefficients ineffective.")))
                     }
                     })
        
        
        observeEvent(req(input$simulate2,inputdata()),{
          
          fresh_vis <- "(-1.90929008775433)+293.747340146854*solidscontent+0.0185866446926401*(quenchinjectionrate)+-0.221460924788289*(finalmixingtime)+1.64713604618159*(silversontipspeedondischarge)+-2.25165790211731*(tempofsamplesondischarge)+-0.0301432004945295*(fatsinjectionrate)+0.911630864596123*(updatedfatstemp)+((finalmixingtime)-24.0511031746786)*((tempofsamplesondischarge)-40.3160952380953)*-0.350152923119871+(solidscontent-0.889285714285715)*((fatsinjectionrate)-58.5077556918568)*5.69749029830664" 
          day_vis <- "(-63.1284043740373)+360.506718384543*solidscontent+0.0521562257371331*(silversontipspeedduringquench)+0.0353296119308415*(silversontipspeedduringendmixing)+3.27376326998214*(silversontipspeedondischarge)+2.07763992027492*(updatedfatstemp)+((silversontipspeedduringendmixing)-11.690086036606)*((silversontipspeedduringendmixing)-11.690086036606)*-0.313650248085591+((silversontipspeedduringquench)-10.5377695693782)*((silversontipspeedondischarge)-11.6316630241121)*0.0468925549996396" 
          day_yield_press <-  "8.93621729663531+196.247634160075*solidscontent+0.39712728468703*(silversontipspeedduringendmixing)+0.709966849477569*(silversontipspeedondischarge)+0.169393285431812*(fatsinjectionrate)+-1.60593890352765*(tempatpointofquenchinjection)+(solidscontent-0.889285714285715)*((fatsinjectionrate)-58.5077556918568)*6.46419044816413+((tempatpointofquenchinjection)-56.6174285714286)*((tempatpointofquenchinjection)-56.6174285714286)*-1.86801925904818" 
          week_vis <-   "632.871957397762+353.212348019952*solidscontent+1.85300874016657*(silversontipspeedduringemulsion)+-0.0492383896885823*(silversontipspeedduringquench)+0.15817538864863*(silversontipspeedduringendmixing)+2.66081552162148*(silversontipspeedondischarge)+-0.233420366808025*(fatsinjectionrate)+-8.57117901092962*(tempatpointofquenchinjection)+((silversontipspeedduringquench)-10.5377695693782)*((silversontipspeedduringquench)-10.5377695693782)*-0.443509372083955+(solidscontent-0.889285714285715)*((silversontipspeedduringendmixing)-11.690086036606)*-14.3823974090446+(solidscontent-0.889285714285715)*((fatsinjectionrate)-58.5077556918568)*15.7785498295843+((fatsinjectionrate)-58.5077556918568)*((tempatpointofquenchinjection)-56.6174285714286)*-0.621838628323516"
          
          
          for(i in col_vars){
            fresh_vis <- gsub(i,paste0("inputdata()$",`i`),fresh_vis)
            day_vis <- gsub(i,paste0("inputdata()$",`i`),day_vis)
            day_yield_press <- gsub(i,paste0("inputdata()$",`i`),day_yield_press)
            week_vis <- gsub(i,paste0("inputdata()$",`i`),week_vis)
          }
          
          output$Simulation_Result2 <- renderUI(
            h3("Simulation results")
          )
          
          freshvis_result <- eval(parse(text = fresh_vis))
          dayvis_result <- eval(parse(text = day_vis))
          dayield_result <- eval(parse(text = day_yield_press))
          weekvis_result <- eval(parse(text = week_vis))
          
          
          tbl <- as.data.frame(cbind(freshvis_result,dayvis_result,dayield_result,weekvis_result))
          nrdata <- as.data.frame(tbl)
          importresults(nrdata)
          
          # nrdata1 <- as.data.frame(df)
          output$download2 <- downloadHandler(
            filename = function() { "Import Data Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Import Results" = nrdata), file)
            }
          )
          
          output$modeltable2 <- renderDataTable({
            datatable(data.frame(freshvis_result,dayvis_result,dayield_result,weekvis_result),
                      colnames = c("Fresh Viscosity [Pa s]","24 hr Viscosity [Pa s]","24 hr Viscosity [Pa s]","1 Week Viscosity [Pa s]"))          })
        })
        
      })
      
      observeEvent(req(input$input),
                   {data <- reactive(read_excel(input$input$datapath))
                   data <- data()[, c("Solids Content","Silverson Tip Speed During Emulsion [m/s]","Quench Injection Rate [%kg/hr]","Silverson Tip Speed During Quench [m/s]",
                                                              "Silverson Tip Speed During End Mixing [m/s]","Final Mixing Time [mins]","Silverson Tip Speed on Discharge [m/s]","Temp of Samples on Discharge [C]",
                                                              "Fats Injection Rate [%kg/hr]","Temp at Point of Quench Injection [C]","Updated Fats Temp [C]")]
                   
                   updateSelectInput(session, "y_axis", choices = colnames(data), selected = colnames(data)[2])
                   updateSelectInput(session, "x_axis", choices = colnames(data), selected = colnames(data)[4])
                   updateSelectInput(session, "x_line", choices = colnames(data))
                   updateSelectInput(session, "y_line", choices = colnames(data))
                   updateSelectInput(session, "hist_choice", choices = colnames(data))
                   
                   output$simulationdata <- renderDataTable(data)
                   
                   # output$scatterplot <- renderPlot(
                   #   if(length(input$y_axis) == 1){
                   #     plot(x= data[[input$x_axis]], y = data[[input$y_axis]],
                   #          xlab = input$x_axis, ylab = input$y_axis, col = "blue")
                   #     abline(lm(data[[input$y_axis]]~data[[input$x_axis]]), col = "blue")
                   #   }
                   #   else{if(length(input$y_axis) > 4){
                   #     showModal(modalDialog("Maximum 4 selections are allowed."))
                   #   }else{
                   #     plot(x= data[[input$x_axis]], y = data[[input$y_axis[1]]],
                   #          xlab = input$x_axis, ylab = input$y_axis, col = "blue")
                   #     abline(lm(data[[input$y_axis[1]]]~data[[input$x_axis]]), col = "blue")
                   #     for(i in 2:length(input$y_axis)){
                   #       points(x= data[[input$x_axis]], y = data[[input$y_axis[i]]], col = colors[i])
                   #       abline(lm(data[[input$y_axis[i]]]~data[[input$x_axis]]), col = colors[i])
                   #     }
                   #   }
                   #   } )
                   
                   output$ggsmooth <- renderPlot({
                     if(input$smooth2){
                       if(length(input$y_axis) == 1){
                         ggplot(data, aes(x= data[[input$x_axis]],y= data[[input$y_axis]])) +
                           geom_point(col="blue") + geom_smooth(method="lm", col="blue") +xlab(as.character(input$x_axis))+
                           ylab(as.character(input$y_axis))
                       }else{
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         
                         ggplot(data, aes(x=data[[input$x_axis]])) +
                           geom_point(aes(y=data[[first]]), col="blue")+
                           geom_point(aes(y=data[[second]] * 1), col="red", shape = 18)+
                           geom_smooth(aes(y=data[[first]]), method="lm", col="blue") +
                           geom_smooth(aes(y=data[[second]] * 1), method="lm", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }else{
                       if(length(input$y_axis) == 1){
                         ggplot(data, aes(x= data[[input$x_axis]],y= data[[input$y_axis]])) +
                           geom_point +xlab(as.character(input$x_axis))+
                           ylab(as.character(input$y_axis))
                       }else
                       {
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         
                         ggplot(data, aes(x=data[[input$x_axis]])) +
                           geom_point(aes(y=data[[first]]), col="blue")+
                           geom_point(aes(y=data[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }
                   })
                   
                   output$ggscatter <- renderPlot({
                     if(input$smooth){
                       if(length(input$y_axis) == 1){
                         ggplot(data, aes(x= data[[input$x_axis]],y= data[[input$y_axis]])) +
                           geom_point(col="blue") + geom_line(col="blue") +labs(x = as.character(input$x_axis))
                       }else{
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         
                         ggplot(data, aes(x=data[[input$x_axis]])) +
                           geom_point(aes(y=data[[first]]), col="blue")+
                           geom_point(aes(y=data[[second]] * 1), col="red", shape = 18)+
                           geom_line(aes(y=data[[first]]), col="blue")+
                           geom_line(aes(y=data[[second]] * 1), col="red")+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }else{
                       if(length(input$y_axis) == 1){
                         ggplot(data, aes(x= data[[input$x_axis]],y= data[[input$y_axis]])) +
                           geom_point() +labs(x = as.character(input$x_axis))
                       }else
                       {
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data, aes(x=data[[input$x_axis]])) +
                           geom_point(aes(y=data[[first]]), col="blue")+
                           geom_point(aes(y=data[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }
                   })
                   
                   output$hist <- renderPlot({
                     ggplot(data, aes(x= data[[input$hist_choice]])) +
                       geom_histogram(color="black", fill="lightblue")+
                       labs(x = as.character(input$hist_choice))
                   })
                   })
      
      observeEvent(input$downloadresults,{
        
        output$Download_Values <- renderUI({
          ns <- session$ns
          downloadButton(ns("download_all"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
        
        output$download_all <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2), file)
          }
        )
      })#download end
      
      #non linear optimisation for erins 
      
      observeEvent(req(models_df),{

        predictor_names_erin <- c("Solids Content_[0.85,1]","Silverson Tip Speed during Emulsion [m/s]_[7.76,23.26]","Silverson Tip Speed during Quench  [m/s]_[0,23.26]",
                                  "Silverson Tip Speed during End Mixing [m/s]_[0,23.26]","Silverson Tip Speed on Discharge [m/s]_[0,23.26]",
                                  "Updated Fats Temperature [C]_[59.7,77.7]","Fats Injection Rate [%kg/hr]_[33.54,117.51]","Temp at point of Quench [C]_[53.94,59.77]",
                                  "Quench Injection Rate [%kg/hr]_[164.06,837.99]","Final Mixing Time [mins]_[8.08,46.05]","Temperature of samples on discharge [C]_[38.34,42.65]")
        
        zero_vector<-rep(1,length(predictor_names_erin))
        min_vector <- c(0.85,7.76,0.00,0.00,0.00,59.7,33.54,53.94,164.06,8.08,38.34)
        max_vector <- c(1.00,23.26,23.26,23.26,23.26,77.70,117.51,59.77,837.99,46.05,42.65)
        coef_data2 <- data.frame(cbind(predictor_names_erin,zero_vector,min_vector,max_vector))
        opt$tab_2 <- coef_data2
        opt$tab_2[[3]]<- as.numeric(opt$tab_2[[3]])
        opt$tab_2[[4]]<- as.numeric(opt$tab_2[[4]])

        #table 1
        output$optimiser_table1_erin <- renderDataTable({
          DT::datatable(opt$tab_2,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
        })

        #cell edit
        observeEvent(input$optimiser_table1_erin_cell_edit,{
          info <- input$optimiser_table1_erin_cell_edit
          i <- info$row
          j <- info$col
          v <- info$value
          if(j >= 2 && !is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            if(j==2 || ( j==3 && opt$tab_2[i, j+1] > v) || (j==4 && opt$tab_2[i, j-1] < v )){
              opt$tab_2[i,j] <<- DT::coerceValue(v,opt$tab_2[i, j])
            }
          }
          rep <- opt$tab_2
          DT::replaceData(erin_proxy, rep, resetPaging = FALSE)
        })
        
        observeEvent(input$run_optimiser_erin,{
          
          target_one <- input$numeric_input_erin_one
          # inequality_selection_one <- input$inequality_selection_erin_one
          weight_one <- input$weight_erin_one
          
          target_two <- input$numeric_input_erin_two
          # inequality_selection_two <- input$inequality_selection_erin_two
          weight_two <- input$weight_erin_two
          
          target_three <- input$numeric_input_erin_three
          # inequality_selection_three <- input$inequality_selection_erin_three
          weight_three <- input$weight_erin_three
          
          target_four <- input$numeric_input_erin_four
          # inequality_selection_four <- input$inequality_selection_erin_four
          weight_four <- input$weight_erin_four
          
          opt$tab_2[[2]] <- as.numeric(opt$tab_2[[2]])
          
          constraint <- function(x){

            #equations
            equation_one <- -1.90929008775433 + 293.747340146854 * x[1] + 0.0185866446926401 *
                            (x[9]) + -0.221460924788289 * (x[10]) + 1.64713604618159 * (x[5]) +
                            -2.25165790211731 * (x[11]) + -0.0301432004945295 *
                            (x[7]) + 0.911630864596123 *(x[6]) + ((x[10]) -24.0511031746786) *
                            ((x[11]) - 40.3160952380953) * -0.350152923119871 +
                            (x[1] - 0.889285714285715) * ((x[7]) - 58.5077556918568) * 5.69749029830664 - target_one


            equation_two <- -63.1284043740373 + 360.506718384543 * x[1] + 0.0521562257371331 *
                            (x[3]) + 0.0353296119308415 *(x[4]) + 3.27376326998214 *(x[5]) + 2.07763992027492 *
                            (x[6]) + ( (x[4] ) - 11.690086036606) * ((x[4]) -11.690086036606) * -0.313650248085591 +
                            ((x[3]) - 10.5377695693782) * ((x[5]) -11.6316630241121) * 0.0468925549996396 - target_two


            equation_three <-  8.93621729663531 + 196.247634160075 * x[1] + 0.39712728468703 *
                              (x[4]) + 0.709966849477569 *(x[5]) + 0.169393285431812 * (x[7]) + -1.60593890352765 *
                              (x[8]) + (x[1]-0.889285714285715) * ((x[7]) - 58.5077556918568)* 6.46419044816413 +
                              ((x[8])-56.6174285714286) * ((x[8])-56.6174285714286) * -1.86801925904818 - target_three


            equation_four <- 632.871957397762 + 353.212348019952 * x[1]
                              + 1.85300874016657 *(x[2]) + -0.0492383896885823 * (x[3]) + 0.15817538864863 *(x[4]) + 2.66081552162148 *
                              (x[5]) + -0.233420366808025 * (x[7]) + -8.57117901092962 *(x[8]) + ((x[3]) - 10.5377695693782) *
                            ((x[3]) - 10.5377695693782) * -0.443509372083955 + (x[1] - 0.889285714285715) * ((x[4]) - 11.690086036606) *
                            -14.3823974090446 + (x[1] - 0.889285714285715) * ((x[7]) - 58.5077556918568) * 15.7785498295843 +
                            ((x[7]) - 58.5077556918568) * ((x[8]) - 56.6174285714286) * -0.621838628323516 - target_four
            
            #eq1
            if(input$inequality_selection_erin_one =="less than or equal to"){
              equation1 <- c(equation_one)
            }
            # View(inequality_selection_erin_one)
            else if(input$inequality_selection_erin_one =="greater than or equal to"){
              equation1 <- c(-1*equation_one)
            }
            
            else{
              equation1 <- c(equation_one-0.0001,-1*equation_one-0.0001)
            }
            
            #eq2
            if(input$inequality_selection_erin_two=="less than or equal to"){
              equation2 <- c(equation_two)
            }
            
            else if(input$inequality_selection_erin_two =="greater than or equal to"){
              equation2 <- c(-1*equation_two)
            }
            
            else{
              equation2 <- c(equation_two-0.0001,-1*equation_two-0.0001)
            }
            
            #eq3
            if(input$inequality_selection_erin_three =="less than or equal to"){
              equation3 <- c(equation_three)
            }
            
            else if(input$inequality_selection_erin_three =="greater than or equal to"){
              equation3 <- c(-1*equation_three)
            }
            else{
              equation3 <- c(equation_three-0.0001,-1*equation_three-0.0001)
            }
            
            #eq4
            if(input$inequality_selection_erin_four =="less than or equal to"){
              equation4 <- c(equation_four)
            }
            
            else if(input$inequality_selection_erin_four =="greater than or equal to"){
              equation4 <- c(-1*equation_four)
            }
            else{
              equation4 <- c(equation_four-0.0001,-1*equation_four-0.0001)
            }
            
            return(c(equation1,equation2,equation3,equation4))

          }#end of constraint function
          
          obj <-function(x){
            
            if(input$radio_button_erin == 'min'){
              
              return(as.numeric(weight_one(opt$tab_2[1,2]*x[1] + opt$tab_2[9,2]*x[9] + opt$tab_2[10,2]*x[10] + opt$tab_2[5,2]*x[5] +
                                  opt$tab_2[11,2]*x[11] + opt$tab_2[7,2]*x[7] +opt$tab_2[6,2]*x[6] + 
                                  opt$tab_2[10,2]*x[10]*opt$tab_2[11,2]*x[11]+ opt$tab_2[1,2]*x[1]*opt$tab_2[7,2]*x[7])
                                + weight_two(opt$tab_2[1,2]*x[1] + opt$tab_2[3,2]*x[3] + opt$tab_2[4,2]*x[4] + opt$tab_2[5,2]*x[5] +
                                  opt$tab_2[6,2]*x[6] + opt$tab_2[4,2]*x[4]*opt$tab_2[4,2]*x[4] + opt$tab_2[3,2]*x[3]*opt$tab_2[5,2]*x[5])
                                + weight_three(opt$tab_2[1,2]*x[1] + opt$tab_2[4,2]*x[4] + opt$tab_2[5,2]*x[5] + opt$tab_2[7,2]*x[7] +
                                    opt$tab_2[8,2]*x[8] + opt$tab_2[1,2]*x[1]*opt$tab_2[7,2]*x[7] + opt$tab_2[8,2]*x[8]*opt$tab_2[8,2]*x[8])
                                + weight_four(opt$tab_2[1,2]*x[1] + opt$tab_2[2,2]*x[2] + opt$tab_2[3,2]*x[3] + opt$tab_2[4,2]*x[4] + 
                                   opt$tab_2[5,2]*x[5] + opt$tab_2[7,2]*x[7] + opt$tab_2[8,2]*x[8] +
                                   opt$tab_2[3,2]*x[3]*opt$tab_2[3,2]*x[3] + opt$tab_2[1,2]*x[1]*opt$tab_2[4,2]*x[4] +
                                   opt$tab_2[1,2]*x[1]*opt$tab_2[7,2]*x[7] + opt$tab_2[7,2]*x[7]*opt$tab_2[8,2]*x[8])) )
            }
            
            else{
              
              return( as.numeric(-weight_one(opt$tab_2[1,2]*x[1] + opt$tab_2[9,2]*x[9] + opt$tab_2[10,2]*x[10] + opt$tab_2[5,2]*x[5] +
                                   opt$tab_2[11,2]*x[11] + opt$tab_2[7,2]*x[7] +opt$tab_2[6,2]*x[6] + 
                                   opt$tab_2[10,2]*x[10]*opt$tab_2[11,2]*x[11]+ opt$tab_2[1,2]*x[1]*opt$tab_2[7,2]*x[7])
                                - weight_two(opt$tab_2[1,2]*x[1] + opt$tab_2[3,2]*x[3] + opt$tab_2[4,2]*x[4] + opt$tab_2[5,2]*x[5] +
                                     opt$tab_2[6,2]*x[6] + opt$tab_2[4,2]*x[4]*opt$tab_2[4,2]*x[4] + opt$tab_2[3,2]*x[3]*opt$tab_2[5,2]*x[5])
                                - weight_three(opt$tab_2[1,2]*x[1] + opt$tab_2[4,2]*x[4] + opt$tab_2[5,2]*x[5] + opt$tab_2[7,2]*x[7] +
                                     opt$tab_2[8,2]*x[8] + opt$tab_2[1,2]*x[1]*opt$tab_2[7,2]*x[7] + opt$tab_2[8,2]*x[8]*opt$tab_2[8,2]*x[8])
                                   - weight_four(opt$tab_2[1,2]*x[1] + opt$tab_2[2,2]*x[2] + opt$tab_2[3,2]*x[3] + opt$tab_2[4,2]*x[4] + 
                                      opt$tab_2[5,2]*x[5] + opt$tab_2[7,2]*x[7] + opt$tab_2[8,2]*x[8] +
                                      opt$tab_2[3,2]*x[3]*opt$tab_2[3,2]*x[3] + opt$tab_2[1,2]*x[1]*opt$tab_2[4,2]*x[4] +
                                      opt$tab_2[1,2]*x[1]*opt$tab_2[7,2]*x[7] + opt$tab_2[7,2]*x[7]*opt$tab_2[8,2]*x[8])) )
              # return(-1*(eq1+eq2+eq3+eq4))
            }
            
            View(test)#no output
          }#objective function end
          
          x0 <- opt$tab_2[[3]]
          lb <- opt$tab_2[[3]]
          ub <- opt$tab_2[[4]]
          
          opts <- list("algorithm"="NLOPT_LN_COBYLA",
                       "xtol_rel"=1.0e-8)
          res<- nloptr(x0=x0,eval_f =  obj,
                       eval_g_ineq = constraint,
                       opts = opts,
                       lb=lb, ub=ub)
          
          # optimiser output table 1
          output$optimiser_table32_erin <- renderDataTable({
            df<-data.frame(Predictors = c("Solids Content","Silverson Tip Speed during Emulsion [m/s]","Silverson Tip Speed during Quench  [m/s]",
                                          "Silverson Tip Speed during End Mixing [m/s]","Silverson Tip Speed on Discharge [m/s]",
                                          "Updated Fats Temperature [C]","Fats Injection Rate [%kg/hr]","Temp at point of Quench [C]",
                                          "Quench Injection Rate [%kg/hr]","Final Mixing Time [mins]","Temperature of samples on discharge [C]"),
                           Value = round(res$solution,3)
            )
            DT::datatable(df,selection ="none",rownames = FALSE )
          })
          
          constraint_val1 <- function(x){
              a1 <-  (-1.90929008775433) + 293.747340146854 * x[1] + 0.0185866446926401 *
              ( x[9] ) + -0.221460924788289 * ( x[10] ) + 1.64713604618159 * ( x[5] ) +
              -2.25165790211731 * ( x[11] ) + -0.0301432004945295 *
              ( x[7] ) + 0.911630864596123 *( x[6] ) + ( ( x[10] ) -24.0511031746786) *
              ( ( x[11] ) - 40.3160952380953) * -0.350152923119871 +
              (x[1] - 0.889285714285715) * ( ( x[7] ) - 58.5077556918568) * 5.69749029830664 

              return(a1)            
          }
          constraint_val2 <- function(x){
              a2<-  (-63.1284043740373) + 360.506718384543 * x[1] + 0.0521562257371331 *
              ( x[3] ) + 0.0353296119308415 *( x[4] ) + 3.27376326998214 *( x[5] ) + 2.07763992027492 *
              ( x[6] ) + ( (x[4] ) - 11.690086036606) * ( ( x[4] ) -11.690086036606) * -0.313650248085591 +
              ( (x[3]) - 10.5377695693782) * ( ( x[5] ) -11.6316630241121) * 0.0468925549996396 
              return(a2)
              
              }
          constraint_val3 <- function(x){
              a3 <- 8.93621729663531 + 196.247634160075 * x[1] + 0.39712728468703 *
              ( x[4] ) + 0.709966849477569 *( x[5] ) + 0.169393285431812 * ( x[7] ) + -1.60593890352765 *
              ( x[8] ) + (x[1]-0.889285714285715) * ( ( x[7] ) - 58.5077556918568)* 6.46419044816413 +
              ( ( x[8] )-56.6174285714286) * ( ( x[8] )-56.6174285714286) * -1.86801925904818 
              
              return(a3)
              }
          constraint_val4 <- function(x){
               a4 <- 632.871957397762 + 353.212348019952 * x[1] + 1.85300874016657 *
              ( x[2] ) + -0.0492383896885823 * ( x[3] ) + 0.15817538864863 *( x[4] ) + 2.66081552162148 *
              ( x[5] ) + -0.233420366808025 * ( x[7] ) + -8.57117901092962 *( x[8] ) + (( x[3] ) - 10.5377695693782) * (
                ( x[3] ) - 10.5377695693782) * -0.443509372083955 + (x[1] - 0.889285714285715) * (( x[4] ) - 11.690086036606) *
              -14.3823974090446 + (x[1] - 0.889285714285715) * (( x[7] ) - 58.5077556918568) * 15.7785498295843 +
              (( x[7] ) - 58.5077556918568) * (( x[8] ) - 56.6174285714286) * -0.621838628323516 
                
               return(a4)
          }
          
          
          # optimiser output table 2
          output$optimiser_table22_erin <- renderDataTable({
            
            DT::datatable(as.data.frame(rbind(round(constraint_val1(res$solution),3),round(constraint_val2(res$solution),3),
                                              round(constraint_val3(res$solution),3),round(constraint_val4(res$solution),3)))
                                    ,rownames = c("Fresh Viscosity [Pa s]","24 hr Viscosity [Pa s]",
                                      "24 hr Yiels Stress [Pa s]","1 Week Viscosity [Pa s]"), 
                                      colnames =c("Target variable", "Value"))
          })

          # optimiser output table 3
          if(input$radio_button_erin=='min'){
            output$value_results_erin<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(res$objective,3))
            })
          }
          else{
            output$value_results_erin<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(-1*res$objective,3))
            })
            
          }
          
          
        })#end of run optimiser
          
              
      })#end of non linear observeevent
      observeEvent(input$reset_erin,{
        updateSelectInput(session,"inequality_selection_erin_one",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_erin_one",value = 250)
        updateNumericInput(session,"weight_erin_one",value = 1)
        
        updateSelectInput(session,"inequality_selection_erin_two",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_erin_two",value = 400)
        updateNumericInput(session,"weight_erin_two",value = 1)
        
        updateSelectInput(session,"inequality_selection_erin_three",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_erin_three",value = 150)
        updateNumericInput(session,"weight_erin_three",value = 1)
        
        updateSelectInput(session,"inequality_selection_erin_four",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_erin_four",value = 550)
        updateNumericInput(session,"weight_erin_four",value = 1)
        
        updateRadioButtons(session,"radio_button_erin",selected = "min")
        
        updateSelectInput(session,"inequality_selection_uday_pred",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_uday_pred",value = 33)
        updateNumericInput(session,"weight_pred",value = 1)
        predictor_names_erin <- c("Solids Content_[0.85,1]","Silverson Tip Speed during Emulsion [m/s]_[7.76,23.26]","Silverson Tip Speed during Quench  [m/s]_[0,23.26]",
                                  "Silverson Tip Speed during End Mixing [m/s]_[0,23.26]","Silverson Tip Speed on Discharge [m/s]_[0,23.26]",
                                  "Updated Fats Temperature [C]_[59.7,77.7]","Fats Injection Rate [%kg/hr]_[33.54,117.51]","Temp at point of Quench [C]_[53.94,59.77]",
                                  "Quench Injection Rate [%kg/hr]_[164.06,837.99]","Final Mixing Time [mins]_[8.08,46.05]","Temperature of samples on discharge [C]_[38.34,42.65]")
        
        zero_vector<-rep(1,length(predictor_names_erin))
        min_vector <- c(0.85,7.76,0.00,0.00,0.00,59.7,33.54,53.94,164.06,8.08,38.34)
        max_vector <- c(1.00,23.26,23.26,23.26,23.26,77.70,117.51,59.77,837.99,46.05,42.65)
        coef_data2 <- data.frame(cbind(predictor_names_erin,zero_vector,min_vector,max_vector))
        opt$tab_2 <- coef_data2
        opt$tab_2[[3]]<- as.numeric(opt$tab_2[[3]])
        opt$tab_2[[4]]<- as.numeric(opt$tab_2[[4]])
      })
      
      
      
    }
  )}
