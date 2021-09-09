SD_slurryServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      uday_proxy <- DT::dataTableProxy('optimiser_table1_uday')
      uday_proxy_non_linear <- DT::dataTableProxy('optimiser_table1_uday_non_linear')
      
      opt<-reactiveValues(tab_1=NULL,tab_2=NULL) 
      
      manualinput_uday <- reactiveVal(NULL)
      manual_uday <- reactiveVal(NULL)
      importresults_uday <- reactiveVal(NULL)
      optimise_uday <- reactiveVal(NULL)
      optimise1_uday <- reactiveVal(NULL)
      optimise2_uday <- reactiveVal(NULL)
      weight_one <- reactiveVal(NULL)
      weight_two <- reactiveVal(NULL)

            x_uday <- data.frame(Models_uday <- c("Pred Formula Low Sheer Viscosity = 69.1536246310703*((Target SMC-0.29)/0.315)+177.928009032322*((NaLAS in Slurry-0.09)/0.315)+110.938848553557*((LSA in Slurry-0.13)/0.315)-19.3532538435312*((Sulphate in Slurry-0.12)/0.315)+200.241313148041*((AlkSilicate in Slurry-0.054)/0.315)+84.026446932*((CP5 in Slurry-0.001)/0.315)-133.9508143*(SCMC in Slurry/0.315)+((Target SMC-0.29)/0.315)*(((NaLAS in Slurry-0.09)/0.315)*-617.616933895298)+((Target in SMC-0.29)/0.315)*(((LSA in Slurry-0.13)/0.315)*-390.462090953345)+((Target SMC-0.29)/0.315)*(((Alk Silicate in Slurry -0.054)/0.315)*-764.6979746)+((LSA in Slurry-0.13)/0.315)*(((CP5 in Slurry-0.001)/0.315)*-1142.960714+((LSA in Slurry-0.13)/0.135)*((SCMC in Slurry/0.315)*2249.8386364))"
                                            ,"Torque = -869.69157979082*TargetSMC+ 275.811033852585*NaLAS in Slurry+-640.1501097706*Alk Silicate in Slurry+668.418092332803*CP5 in Slurry+ 318.358001206768*LSA in Slurry+1671.3734983827 *SCMC in Slurry + 388.721602050357*Sulphate in Slurry+Target SMC*(Target SMC*1310.48156368694)+NaLAS in Slurry*(NaLAS in Slurry*212.536004289529)+Sulphate in Slurry*(Sulphate in Slurry*-384.589140519452)+LSA in Slurry*(LSA in Slurry*-17.9351176105554)+AlkSilicate in Slurry*(AlkSilicate in Slurry*6523.70048980465)+CP5 in Slurry*(CP5 in Slurry*-19394.6828836579)+SCMC in Slurry*(SCMC in Slurry*-216978.610667717)"
                                            , 
                                            " Drying Prediction =1.64652727504537 *TargetSMC+ -0.340054974118285 *NaLAS
+0.0349876142645199 *AlkSilicate+ -0.26064073764549 * CP5 +
-0.0575389664392278 * LSA + -1.17237663840093 * SCMC +
-0.298363251134605 * Sulphate
"
      ))

      
      b_uday <- c("TargetSMC", "NaLASinSlurry", "LSAinSlurry" , "SulphateinSlurry","AlkSilicateinSlurry" ,"CP5inSlurry", "SCMCinSlurry")
      c_uday <- c("targetsmc", "NaLASinSlurry", "LSAinSlurry" , "SulphateinSlurry","AlkSilicateinSlurry" ,"CP5inSlurry", "SCMCinSlurry")
      d_uday <- c("targetsmc", "nalas", "lsa" , "sulphate","alksilicate" ,"cp5", "scmc")
      all_vars_uday <-c("TargetSMC", "NaLAS", "LSA" , "Sulphate","AlkSilicate" ,"CP5", "SCMC")
      # model bank table 
      
      output$models_uday <- renderDataTable({
        datatable(x_uday, colnames = c("Models"))
      } )
      
      
      
      # go to simulation
      observeEvent(input$commit_uday,{
        updateTabsetPanel(top_session, "tabs_uday", selected = "Simulation")
      })
      # go to visualization
      observeEvent(input$commit2_uday,{
        updateTabsetPanel(top_session, "tabs_uday", selected = "Visualization")
      })
      
      #rendering advisory tables
      output$advice_uday <- renderDataTable({
        Advisory_table <- data.frame(Ingredients = c("TargetSMC","NaLAS (dry basis)",
                                                     "Alkaline Silicate (dry basis)",
                                                     "Sodium Carbonate (dry basis)","CP5 (dry basis)",
                                                     "SCMC (dry basis)", "Sodium Sulphate (dry basis)"),
                                     Lower_Level = c(0.287, 0.13, 0.07, 0.17, 0.00, 0.00, 0.17),
                                     Upper_Level = c( 0.37, 0.41, 0.16, 0.45, 0.03, 0.01, 0.52))
        datatable(Advisory_table)
      })
      
      colors <- c("red","black", "green","yellow","violet")
      
      
      # profiler renderings
      observeEvent(req(input$profiler_nalas),{

        eqn1 <- "69.1536246310703*((TargetSMC-0.29)/0.315)+177.928009032322*((NaLASinSlurry()-0.09)/0.315)+110.938848553557*((LSAinSlurry()-0.13)/0.315)-19.3532538435312*((SulphateinSlurry()-0.12)/0.315)+200.241313148041*((AlkSilicateinSlurry()-0.054)/0.315)+84.026446932*((CP5inSlurry()-0.001)/0.315)-133.9508143*(SCMCinSlurry()/0.315)+((TargetSMC-0.29)/0.315)*(((NaLASinSlurry()-0.09)/0.315)*-617.616933895298)+((TargetSMC-0.29)/0.315)*(((LSAinSlurry()-0.13)/0.315)*-390.462090953345)+((TargetSMC-0.29)/0.315)*(((AlkSilicateinSlurry() -0.054)/0.315)*-764.6979746)+((LSAinSlurry()-0.13)/0.315)*(((CP5inSlurry()-0.001)/0.315)*-1142.960714+((LSAinSlurry()-0.13)/0.135)*((SCMCinSlurry()/0.315)*2249.8386364))"
        eqn2 = "-869.69157979082*TargetSMC+ 275.811033852585*NaLASinSlurry()+-640.1501097706*AlkSilicateinSlurry()+668.418092332803*CP5inSlurry()+ 318.358001206768*LSAinSlurry()+1671.3734983827 *SCMCinSlurry() + 388.721602050357*SulphateinSlurry()+TargetSMC*(TargetSMC*1310.48156368694)+NaLASinSlurry()*(NaLASinSlurry()*212.536004289529)+SulphateinSlurry()*(SulphateinSlurry()*-384.589140519452)+LSAinSlurry()*(LSAinSlurry()*-17.9351176105554)+AlkSilicateinSlurry()*(AlkSilicateinSlurry()*6523.70048980465)+CP5inSlurry()*(CP5inSlurry()*-19394.6828836579)+SCMCinSlurry()*(SCMCinSlurry()*-216978.610667717)"
        eqn3 <- "1.64652727504537 *TargetSMC+ -0.340054974118285 *NaLAS() +0.0349876142645199 *AlkSilicate()+ -0.26064073764549 *CP5() +-0.0575389664392278 * LSA() + -1.17237663840093 * SCMC() + -0.298363251134605 * Sulphate()"

        NaLAS <- reactive(input$profiler_nalas)
        TargetSMC <- round(seq(from  = 0.287, to = 0.37, length.out = 100),3)
        CP5 <- reactive(input$profiler_cp5)
        SCMC <- reactive(input$profiler_scmc)
        Sulphate <- reactive(input$profiler_sulphate)
        AlkSilicate <- reactive(input$profiler_alksilicate)
        LSA <- reactive(input$profiler_lsa)
        the_sum <- reactive(as.numeric(NaLAS()+CP5()+SCMC()+Sulphate()+AlkSilicate()+LSA()))

        observeEvent(input$profiler_nalas | input$profiler_scmc | input$profiler_alksilicate | input$profiler_cp5 |input$profiler_lsa | input$profiler_sulphate | input$profiler_targetsmc,{
          if(the_sum()==1){
            NaLASinSlurry <- reactive(NaLAS()/the_sum() * (1- TargetSMC))
            AlkSilicateinSlurry <- reactive(AlkSilicate()/the_sum() * (1- TargetSMC))
            CP5inSlurry <- reactive(CP5()/the_sum() * (1- TargetSMC))
            LSAinSlurry <- reactive(LSA()/the_sum() * (1- TargetSMC))
            SCMCinSlurry <- reactive(SCMC()/the_sum() * (1- TargetSMC))
            SulphateinSlurry <- reactive(Sulphate()/the_sum() * (1- TargetSMC))

            Pred_Formula_Low_Sheer_Viscosity <- reactive(eval(parse(text = eqn1)))
            Torque300 <- reactive(eval(parse(text = eqn2)))
            Drying_Prediction <- reactive(eval(parse(text = eqn3)))
            
            output$plot1 <- renderPlot({
              Torque <- Torque300()
              ggplot(data=data.frame(TargetSMC, Torque), aes(x=TargetSMC, y= Torque)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
            
            output$plot2 <- renderPlot({
              Pred_Formula_Low_Sheer_Viscosity <- Pred_Formula_Low_Sheer_Viscosity()
              ggplot(data=data.frame(TargetSMC, Pred_Formula_Low_Sheer_Viscosity), aes(x=TargetSMC, y= Pred_Formula_Low_Sheer_Viscosity)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
            
            output$plot3 <- renderPlot({
              Drying_Prediction <- Drying_Prediction()
              ggplot(data=data.frame(TargetSMC, Drying_Prediction), aes(x=TargetSMC, y= Drying_Prediction)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
            
          }else
          {
            Sulphate <- reactive(1 - as.numeric(NaLAS()+CP5()+SCMC()+AlkSilicate()+LSA()))
            the_sum <- reactive(as.numeric(NaLAS()+CP5()+SCMC()+Sulphate()+AlkSilicate()+LSA()))

            NaLASinSlurry <- reactive(NaLAS()/the_sum() * (1- TargetSMC))
            AlkSilicateinSlurry <- reactive(AlkSilicate()/the_sum() * (1- TargetSMC))
            CP5inSlurry <- reactive(CP5()/the_sum() * (1- TargetSMC))
            LSAinSlurry <- reactive(LSA()/the_sum() * (1- TargetSMC))
            SCMCinSlurry <- reactive(SCMC()/the_sum() * (1- TargetSMC))
            SulphateinSlurry <- reactive(Sulphate()/the_sum() * (1- TargetSMC))

            Pred_Formula_Low_Sheer_Viscosity <- reactive(eval(parse(text = eqn1)))
            Torque300 <- reactive(eval(parse(text = eqn2)))
            Drying_Prediction <- reactive(eval(parse(text = eqn3)))
            
            output$plot1 <- renderPlot({
              Torque <- Torque300()
              ggplot(data=data.frame(TargetSMC, Torque), aes(x=TargetSMC, y= Torque)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
            
            output$plot2 <- renderPlot({
              Pred_Formula_Low_Sheer_Viscosity <- Pred_Formula_Low_Sheer_Viscosity()
              ggplot(data=data.frame(TargetSMC, Pred_Formula_Low_Sheer_Viscosity), aes(x=TargetSMC, y= Pred_Formula_Low_Sheer_Viscosity)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
            
            output$plot3 <- renderPlot({
              Drying_Prediction <- Drying_Prediction()
              ggplot(data=data.frame(TargetSMC, Drying_Prediction), aes(x=TargetSMC, y= Drying_Prediction)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
          }
        })



      })
      
      
      
      #visualization page renderings
      observeEvent(req(input$datacall_uday),
                   {#data_uday <- reactive(read_excel(input$datacall_uday$datapath))
                     #data <- reactive(data()[,-(which(colSums(data())==0))])
                     uday_data_slurry1 <- reactive({
                       req(input$datacall_uday)
                       inFile <- input$datacall_uday
                       if(is.null(inFile)) return(NULL)
                       
                       #xlfile <- read_excel(paste0(inFile$datapath, ".xlsx"))
                       xlfile <- read_excel(input$datacall_uday$datapath)
                       colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
                       colnames(xlfile) <- gsub(" ","",colnames(xlfile))
                       colnames(xlfile) <- tolower(colnames(xlfile))
                       xlfile
                       
                     })
                     df1 <- uday_data_slurry1()
                     df1$sum <- df1$nalas + df1$scmc + df1$alksilicate + df1$cp5 + df1$sulphate + df1$lsa
                     df1$NaLASinSlurry <- df1$nalas/df1$sum*(1-df1$targetsmc)
                     df1$LSAinSlurry <- df1$lsa/df1$sum*(1-df1$targetsmc)
                     df1$SCMCinSlurry <- df1$scmc/df1$sum*(1-df1$targetsmc)
                     df1$AlkSilicateinSlurry <- df1$alksilicate/df1$sum*(1-df1$targetsmc)
                     df1$CP5inSlurry <- df1$cp5/df1$sum*(1-df1$targetsmc)
                     df1$SulphateinSlurry <- df1$sulphate/df1$sum*(1-df1$targetsmc)
                     #View(df1)
                     uday_data_slurry2 <- reactive({
                       req(input$datacall_uday)
                       inFile <- input$datacall_uday
                       if(is.null(inFile)) return(NULL)
                       
                       #file.rename(inFile$datapath, paste0(inFile$datapath, ".xlsx"))
                       #xlfile <- read_excel(paste0(inFile$datapath, ".xlsx"))
                       xlfile <- read_excel(input$datacall_uday$datapath)
                       
                       xlfile
                       
                     })
                     df2 <- uday_data_slurry2()
                     df3 <- df1[tail(seq_along(df1),7)]
                     colnames(df3) <- c("Sum", "NaLAS in Slurry", "LSA in Slurry", "SCMC in Slurry", "Alk Silicate in Slurry", "CP5 in Slurry", "Sulphate in Slurry")
                     # View(df3)
                     finaldf <- as.data.frame(cbind(df2, df3))
                     
                     output$simulationdata_uday <- renderDataTable({
                       uday_data_slurry2()
                     })
                     # View(finaldf)
                     #View(data_uday)
                     updateSelectInput(session, "y_axis_bd", choices = colnames(finaldf), selected = colnames(finaldf)[2])
                     updateSelectInput(session, "x_axis_bd", choices = colnames(finaldf))
                     updateSelectInput(session, "x_line", choices = colnames(finaldf))
                     updateSelectInput(session, "y_line", choices = colnames(finaldf))
                     updateSelectInput(session, "hist_choice_uday", choices = colnames(finaldf))
                     
                     output$simulationdata1_uday <- renderDataTable(finaldf)
                     output$scatterplot_uday <- renderPlot(
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
                     output$ggsmooth_uday<- renderPlot({
                       if(input$smooth2_uday){
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
                           ggplot(data_uday_bd(), aes(x= finaldf[[input$x_axis_bd]],y= finaldf[[input$y_axis_bd]])) +
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
                     
                     output$ggscatter_uday <- renderPlot({
                       if(input$smooth_uday){
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
                           ggplot(finaldf, aes(x= finaldf[[input$x_axis_bd]],y= data_uday_bd()[[input$y_axis_bd]])) +
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
                     
                     output$multi_lines_graph_uday <- renderPlotly({
                       if(length(input$y_axis_bd) == 1){
                         fig <- plot_ly( x = ~data_uday_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd]],mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = as.character(input$y_axis)) )
                         fig
                       }else if(length(input$y_axis_bd) == 2){
                         fig <- plot_ly(data_uday_bd(), x = ~data_uday_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       }
                       else if(length(input$y_axis_bd) == 3){
                         fig <- plot_ly(data_uday_bd(), x = ~data_uday_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 4){
                         fig <- plot_ly(data_uday_bd(), x = ~data_uday_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 5){
                         fig <- plot_ly(data_uday_bd(), x = ~data_uday_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[5]]], name = input$y_axis_bd[5], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 6){
                         fig <- plot_ly(data_uday_bd(), x = ~data_uday_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[5]]], name = input$y_axis_bd[5], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_uday_bd()[[input$y_axis_bd[6]]], name = input$y_axis_bd[6], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       }
                     })
                     output$hist_uday <- renderPlot({
                       ggplot(finaldf, aes(x= finaldf[[input$hist_choice_uday]])) +
                         geom_histogram(color="black", fill="lightblue")+
                         labs(x = as.character(input$hist_choice_uday))
                     })
                   })
      
      
      
      observeEvent(req(x_uday),{
        x1_uday <- reactiveValues()
        observe({
          y_uday <- reactive({
           # munits <- rep("%(w/w)",length(all_vars_uday))
            #sqr1 <- datatable(sqr, editable = T,colnames = c("Model Predictors","Measurement Units", "Enter Simulation Values"))
            munits <- rep("[0-1 %(w/w)]",length(all_vars_uday))
            values <- c(0.287, 0.13, 0.07, 0.17, 0.00, 0.00, 0.17)
            sqr <- do.call(rbind,data.frame(cbind(munits,values)))
            # sqr <- data.frame(t(values))
            colnames(sqr) <- all_vars_uday
            rownames(sqr) <- c("Measurement Units [Fractions (0-1)]","Enter Simulation Values")
            sqr
          })
          x1_uday$df <- y_uday()
        })
        
        
        
        output$simulation_input_uday <- renderDataTable({ 
         # datatable(x1_uday$df, editable = T)
          datatable(x1_uday$df, editable = T) %>%
            formatStyle(
              "NaLAS",
              color = styleInterval(c(0.129, 0.411), c('red', 'black', 'red')))%>%
            formatStyle(
              "TargetSMC",
              color = styleInterval(c(0.286, 0.371), c('red', 'black', 'red')))%>%
            formatStyle(
              "AlkSilicate",
              color = styleInterval(c(0.069, 0.161), c('red', 'black', 'red')))%>%
            formatStyle(
              "CP5",
              color = styleInterval(c(0.00, 0.031), c('red', 'black', 'red')))%>%
            formatStyle(
              "LSA",
              color = styleInterval(c(0.169, 0.451), c('red', 'black', 'red')))%>%
            formatStyle(
              "Sulphate",
              color = styleInterval(c(0.169, 0.521), c('red', 'black', 'red')))%>%
            formatStyle(
              "SCMC",
              color = styleInterval(c(0.00, 0.011), c('red', 'black', 'red')))
        })
        
        
        proxy_uday <- dataTableProxy("simulation_input_uday")
        
        
        observeEvent(input[["simulation_input_uday_cell_edit"]], {
          info <- input[["simulation_input_uday_cell_edit"]]
          
          i <- info$row
          j <- info$col
          v <- info$value
          
          if(!is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            x1_uday$df[i, j] <- DT::coerceValue(v,x1_uday$df[i, j])}
          
          rep <- x1_uday$df
          DT::replaceData(proxy_uday, rep, resetPaging = FALSE)

        })
        
        
        output$modeltable_uday <- renderDataTable(
          datatable(x1_uday$df)
        )

        
        observeEvent(input$simulate_uday,{
          eqn1 <- "69.1536246310703*((TargetSMC-0.29)/0.315)+177.928009032322*((NaLASinSlurry-0.09)/0.315)+110.938848553557*((LSAinSlurry-0.13)/0.315)-19.3532538435312*((SulphateinSlurry-0.12)/0.315)+200.241313148041*((AlkSilicateinSlurry-0.054)/0.315)+84.026446932*((CP5inSlurry-0.001)/0.315)-133.9508143*(SCMCinSlurry/0.315)+((TargetSMC-0.29)/0.315)*(((NaLASinSlurry-0.09)/0.315)*-617.616933895298)+((TargetSMC-0.29)/0.315)*(((LSAinSlurry-0.13)/0.315)*-390.462090953345)+((TargetSMC-0.29)/0.315)*(((AlkSilicateinSlurry -0.054)/0.315)*-764.6979746)+((LSAinSlurry-0.13)/0.315)*(((CP5inSlurry-0.001)/0.315)*-1142.960714+((LSAinSlurry-0.13)/0.135)*((SCMCinSlurry/0.315)*2249.8386364))"
          eqn2 = "-869.69157979082*TargetSMC+ 275.811033852585*NaLASinSlurry+-640.1501097706*AlkSilicateinSlurry+668.418092332803*CP5inSlurry+ 318.358001206768*LSAinSlurry+1671.3734983827 *SCMCinSlurry + 388.721602050357*SulphateinSlurry+TargetSMC*(TargetSMC*1310.48156368694)+NaLASinSlurry*(NaLASinSlurry*212.536004289529)+SulphateinSlurry*(SulphateinSlurry*-384.589140519452)+LSAinSlurry*(LSAinSlurry*-17.9351176105554)+AlkSilicateinSlurry*(AlkSilicateinSlurry*6523.70048980465)+CP5inSlurry*(CP5inSlurry*-19394.6828836579)+SCMCinSlurry*(SCMCinSlurry*-216978.610667717)"
          eqn3 <- "1.64652727504537 *TargetSMC+ -0.340054974118285 *NaLAS +0.0349876142645199 *AlkSilicate+ -0.26064073764549 *CP5 +-0.0575389664392278 * LSA + -1.17237663840093 * SCMC + -0.298363251134605 * Sulphate"
          
          df <- as.data.frame(x1_uday$df)
          
        
          
          df$sum <- as.numeric(df$NaLAS) + as.numeric(df$SCMC) + as.numeric(df$AlkSilicate) + as.numeric(df$CP5) + as.numeric(df$Sulphate) + as.numeric(df$LSA)
          df$NaLASinSlurry <- as.numeric(df$NaLAS)/as.numeric(df$sum)*(1-as.numeric(df$TargetSMC))
          df$LSAinSlurry <- as.numeric(df$LSA)/as.numeric(df$sum)*(1-as.numeric(df$TargetSMC))
          df$SCMCinSlurry <- as.numeric(df$SCMC)/as.numeric(df$sum)*(1-as.numeric(df$TargetSMC))
          df$AlkSilicateinSlurry <- as.numeric(df$AlkSilicate)/as.numeric(df$sum)*(1-as.numeric(df$TargetSMC))
          df$CP5inSlurry <- as.numeric(df$CP5)/as.numeric(df$sum)*(1-as.numeric(df$TargetSMC))
          df$SulphateinSlurry <- as.numeric(df$Sulphate)/as.numeric(df$sum)*(1-as.numeric(df$TargetSMC))
          
          output$newvals_uday <- renderDataTable({
           newvals <-  round(data.frame(cbind(df$NaLASinSlurry,df$LSAinSlurry, df$SCMCinSlurry, df$AlkSilicateinSlurry, df$CP5inSlurry , df$SulphateinSlurry)
                             ),3)
            
            DT::datatable(newvals[2,] , colnames = c("NaLAS in Slurry", "LSA in Slurry", " SCMC in Slurry", " Alk Silicate in Slurry", " CP5 in Slurry", "Sulphate in Slurry"), rownames = NULL)
          
            })
          
          
          output$simulation_heading_uday <- renderUI({
            h3("Calculated Values for other Variables")
          })
          
          for(i in b_uday){
            eqn1 <- gsub(i, df[2,i], eqn1)
            eqn2 <- gsub(i, df[2,i], eqn2)
          }
          
          for(i in all_vars_uday){
            eqn3 <- gsub(i, df[2,i], eqn3)
          }
          Pred_Formula_Low_Sheer_Viscosity <- eval(parse(text = eqn1))
          Torque300 <- eval(parse(text = eqn2))
          Drying_Prediction <- eval(parse(text = eqn3))
          
          
          
          tbl <- cbind(Pred_Formula_Low_Sheer_Viscosity, Torque300, Drying_Prediction)
          df1 <- x1_uday$df
       
          manualinput_uday(df1)
          manual_uday(tbl)
        
          nrdata <- as.data.frame(tbl)
          nrdata1 <- as.data.frame(df)
          output$download1_uday <- downloadHandler(
            filename = function() { "Manual Entry Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
            }
          )
          output$heading1_uday <- renderUI({
            h3("Simulation Results")
          
          })
          
          output$result1_uday <- renderDataTable(
            
            { tbl })
        })
        
        # nrdata2 <- as.data.frame(tbl)
        # nrdata12 <- as.data.frame(df1)
        # View(nrdata2)
        # View(nrdata12)
        # output$download1_uday <- downloadHandler(
        #   filename = function() { "Manual Entry Simulation sd.xlsx"},
        #   content = function(file) {
        #     write_xlsx(list("Input" = nrdata12,"Results" = nrdata2), file)
        #   }
        # )
        
        
        
        uday_data_slurry <- reactive({
          req(input$datacall_uday)
          inFile <- input$datacall_uday
          if(is.null(inFile)) return(NULL)
          
          #xlfile <- read_excel(paste0(inFile$datapath, ".xlsx"))
          xlfile <- read_excel(input$datacall_uday$datapath)
          colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
          colnames(xlfile) <- gsub(" ","",colnames(xlfile))
          colnames(xlfile) <- tolower(colnames(xlfile))
          xlfile
          
        })
        
        observeEvent(uday_data_slurry(),
                     {i <- d_uday[!is.element(d_uday,colnames(uday_data_slurry()))]
                     if(length(i)>=1){
                       showModal(modalDialog(paste0(i, " is not present in imported data but required in equation. 0 is placed in the place of missing variables to render the respective coefficients ineffective.")))
                     }
                     })
        # View(uday_data_slurry()[,1])
        observeEvent(req(input$datacall_uday), {
          
          #View(uday_data_slurry())
          df1 <- uday_data_slurry()
          
          #View(df1)
          df1$sum <- df1$nalas + df1$scmc + df1$alksilicate + df1$cp5 + df1$sulphate + df1$lsa
          df1$NaLASinSlurry <- df1$nalas/df1$sum*(1-df1$targetsmc)
          df1$LSAinSlurry <- df1$lsa/df1$sum*(1-df1$targetsmc)
          df1$SCMCinSlurry <- df1$scmc/df1$sum*(1-df1$targetsmc)
          df1$AlkSilicateinSlurry <- df1$alksilicate/df1$sum*(1-df1$targetsmc)
          df1$CP5inSlurry <- df1$cp5/df1$sum*(1-df1$targetsmc)
          df1$SulphateinSlurry <- df1$sulphate/df1$sum*(1-df1$targetsmc)
          
          #View(df1)
          observeEvent(req(input$simulate2_uday),{
            eqn1 <- "69.1536246310703*((targetsmc-0.29)/0.315)+177.928009032322*((NaLASinSlurry-0.09)/0.315)+110.938848553557*((LSAinSlurry-0.13)/0.315)-19.3532538435312*((SulphateinSlurry-0.12)/0.315)+200.241313148041*((AlkSilicateinSlurry-0.054)/0.315)+84.026446932*((CP5inSlurry-0.001)/0.315)-133.9508143*(SCMCinSlurry/0.315)+((targetsmc-0.29)/0.315)*(((NaLASinSlurry-0.09)/0.315)*-617.616933895298)+((targetsmc-0.29)/0.315)*(((LSAinSlurry-0.13)/0.315)*-390.462090953345)+((targetsmc-0.29)/0.315)*(((AlkSilicateinSlurry -0.054)/0.315)*-764.6979746)+((LSAinSlurry-0.13)/0.315)*(((CP5inSlurry-0.001)/0.315)*-1142.960714+((LSAinSlurry-0.13)/0.135)*((SCMCinSlurry/0.315)*2249.8386364))"
            eqn2 = "-869.69157979082*targetsmc+ 275.811033852585*NaLASinSlurry+-640.1501097706*AlkSilicateinSlurry+668.418092332803*CP5inSlurry+ 318.358001206768*LSAinSlurry+1671.3734983827 *SCMCinSlurry + 388.721602050357*SulphateinSlurry+targetsmc*(targetsmc*1310.48156368694)+NaLASinSlurry*(NaLASinSlurry*212.536004289529)+SulphateinSlurry*(SulphateinSlurry*-384.589140519452)+LSAinSlurry*(LSAinSlurry*-17.9351176105554)+AlkSilicateinSlurry*(AlkSilicateinSlurry*6523.70048980465)+CP5inSlurry*(CP5inSlurry*-19394.6828836579)+SCMCinSlurry*(SCMCinSlurry*-216978.610667717)"
            eqn3 <- "1.64652727504537 *targetsmc+ -0.340054974118285 *nalas +0.0349876142645199 *alksilicate+ -0.26064073764549 *cp5 +-0.0575389664392278 * lsa + -1.17237663840093 * scmc + -0.298363251134605 * sulphate"
            
            
            
            #View(colnames(df()))
            
            
            for(i in c_uday){
              eqn1 <- gsub(i, paste0("df1$",i), eqn1)
              eqn2 <- gsub(i, paste0("df1$",i), eqn2)
            }
            
            for(i in d_uday){
              eqn3 <- gsub(i, paste0("df1$",i), eqn3)
            }
            
            Pred_Formula_Low_Sheer_Viscosity <- eval(parse(text = eqn1))
            Torque300 <- eval(parse(text = eqn2))
            Drying_Prediction <- eval(parse(text = eqn3))
            tbl <- cbind(Pred_Formula_Low_Sheer_Viscosity, Torque300, Drying_Prediction)
            nrdata <- as.data.frame(tbl)
            
            importresults_uday(tbl)
            
           # nrdata1 <- as.data.frame(df)
            output$download2_uday <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
           
            output$heading_uday <- renderUI({
              h3("Simulation Results")
            })
            
            output$modeltable2_uday <- renderDataTable({
              DT::datatable(as.data.frame(cbind(Pred_Formula_Low_Sheer_Viscosity, Torque300, Drying_Prediction)), rownames = FALSE)
            })
          })
        })
        
        
        #})
    
        #optimisation renderings for uday(SD slurry props- drying prediction)
        observeEvent(req(x_uday),
                     {
                       predictors_in_model3_uday<-c("TargetSMC_[0.287,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                                    "CP5_[0,0.03]", "LSA_[0.17,0.45]",
                                                    "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
                       zero_vector<-rep(1,length(predictors_in_model3_uday))
                       min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                       max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                       coef_data <- data.frame(cbind(predictors_in_model3_uday,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                       opt$tab_1 <- coef_data
                       opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                       opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                       
                       #table 1
                       output$optimiser_table1_uday<-renderDataTable({
                         DT::datatable(opt$tab_1,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
                       })
                       
                       observeEvent(input$optimiser_table1_uday_cell_edit,{
                         info <- input$optimiser_table1_uday_cell_edit
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
                         DT::replaceData(uday_proxy, rep, resetPaging = FALSE)
                       })
                       
                       observeEvent(input$run_optimiser_uday,{
                         
                         predictors_in_model3_uday<-c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate")
                         reg_coeff_in_model3_uday<-c(1.64652727504537,-0.340054974118285,0.0349876142645199,-0.26064073764549,-0.0575389664392278,-1.17237663840093,-0.298363251134605)
                         if(input$inequality_selection_uday=="less than or equal to"){
                           constr_uday<-'<='
                         }
                         else if(input$inequality_selection_uday=="greater than or equal to"){
                           constr_uday<-'>='
                         }
                         else{
                           constr_uday<-'='
                         }
                         # View(constr)#works
                         
                         target_uday<-input$numeric_input_uday
                         number.predictors_uday<-length(predictors_in_model3_uday)
                         # View(opt$tab_1)
                         low.lims_uday<-opt$tab_1[[3]]
                         upp.lims_uday<-opt$tab_1[[4]]
                         objective.in_uday<-opt$tab_1[[2]]
                         objective.in_uday<-as.numeric(objective.in_uday)
                         obj.type_uday<-input$radio_button_uday
                         # View(objective.in)#works
                         
                         lps.model_uday <- make.lp(0,number.predictors_uday)
                         add.constraint(lps.model_uday,reg_coeff_in_model3_uday,constr_uday,target_uday)
                         
                         # Bounds for variables
                         set.bounds(lps.model_uday,lower=low.lims_uday)
                         set.bounds(lps.model_uday,upper=upp.lims_uday)
                         # View(low.lims)
                         # View(nrow(low.lims))
                         # View(obj.type_uday)
                         # View(objective.in)
                         
                         # Objective function
                         lp.control(lps.model_uday,sense=obj.type_uday) # min or max
                         # View(objective.in) #getting output
                         
                         set.objfn(lps.model_uday,objective.in_uday) # coefficients
                         # View(upp.lims)
                         
                         # Apply solver
                         solution.status <- solve(lps.model_uday)
                         # View(solution.status)
                         if(solution.status!=0){
                           showModal(modalDialog("Linear Optimisation could not find a solution for the given inputs. Please change the inputs and re-run."))
                         }
                         #unpacking
                         solution.values_uday <- get.primal.solution(lps.model_uday)
                         # View(solution.values_uday)
                         objective.function.value_uday <- solution.values_uday[1]
                         fitted.response_uday <- solution.values_uday[2]
                         solution.values_uday <- solution.values_uday[3:length(solution.values_uday)]
                         
                         results_uday<-data.frame(Value = fitted.response_uday)
                         # colnames(results)<-""
                         row.names(results_uday)<-"Drying Prediction"
                         
                         #dopwnload
                         downresults <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Drying Prediction"), Predicted_or_Optimal_Value= fitted.response_uday)
                         downdf<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate"),
                                            Predicted_or_Optimal_Value=solution.values_uday)
                         downopt <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective func. Value"), Predicted_or_Optimal_Value = objective.function.value_uday)
                         
                         
                         final1 <- rbind(downresults,downdf,downopt)
                         #View(final1)
                         optimise_uday(final1)
                         output$download3_uday <- downloadHandler(
                           filename = function() { "Optimisation for Drying Prediction .xlsx"},
                           content = function(file) {
                             write_xlsx(list("Optimisation Result" = final1), file)
                           }
                         )
                         
                         # optmiser table2
                         output$optimiser_table2_uday<-renderDataTable({
                           DT::datatable(results_uday) })
                         
                         # optmiser table3
                         output$optimiser_table3_uday<- renderDataTable({
                           df_uday<-data.frame(Predictors=c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate"),
                                               Value=solution.values_uday)
                           DT::datatable(df_uday,rownames=FALSE)
                           # DT::datatable(df)
                         })
                         
                         #optimiser textoutput
                         output$value_results_uday<- renderUI({
                           p(paste0("The objective value resulting from the optimisation is : "),round(objective.function.value_uday),4)
                         })
                         
                         
                         
                       })#observeEvent run optimiser ends
                       
                       # reset button
                       observeEvent(input$reset_uday,{
                         updateSelectInput(session,"inequality_selection_uday",selected = "less than or equal to")
                         updateNumericInput(session,"numeric_input_uday",value = .32)
                         updateRadioButtons(session,"radio_button_uday",selected = "min")
                         predictors_in_model3_uday<-c("TargetSMC_[0.29,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                                      "CP5_[0,0.03]", "LSA_[0.17,0.45]",
                                                      "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
                         zero_vector<-rep(1,length(predictors_in_model3_uday))
                         min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                         max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                         coef_data <- data.frame(cbind(predictors_in_model3_uday,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                         opt$tab_1 <- coef_data
                         opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                         opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                         
                       })
                       
                     })#observeevent datacall close
        
        observeEvent(input$downloadresults_uday,{
          
          output$Download_Values_uday <- renderUI({
            ns <- session$ns
            downloadButton(ns("download_all_uday"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
          nrdata <- as.data.frame(manual_uday())
          nrdata1 <- as.data.frame(manualinput_uday())
          nrdata2 <- as.data.frame(importresults_uday())
          nrdata3 <- as.data.frame(optimise_uday())
          nrdata4 <- as.data.frame(optimise2_uday())
          
          output$download_all_uday <- downloadHandler(
            filename = function() { "All Results.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2, "Drying Prediction Optimisation" = nrdata3, "Optimisation Torque and Pred Formula Low Sheer Viscosity value" = nrdata4), file)
            
          })
        })
      })
      
      #non linear optimisation for Udays model
      observeEvent(req(x_uday),{
        
        predictor_names_torque <- c("TargetSMC_[0.29,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                    "CP5_[0.001,0.03]", "LSA_[0.17,0.45]",
                                    "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
        zero_vector<-rep(1,length(predictor_names_torque))
        min_vector <- c(0.29,0.13,0.07,0.001,0.17,0,0.17)
        max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
        coef_data2 <- data.frame(cbind(predictor_names_torque,zero_vector,min_vector,max_vector))
        opt$tab_2 <- coef_data2
        opt$tab_2[[3]]<- as.numeric(opt$tab_2[[3]])
        opt$tab_2[[4]]<- as.numeric(opt$tab_2[[4]])
        
        #table 1
        output$optimiser_table1_uday_non_linear <- renderDataTable({
          DT::datatable(opt$tab_2,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
        })
        
        #cell edit
        observeEvent(input$optimiser_table1_uday_non_linear_cell_edit,{
          info <- input$optimiser_table1_uday_non_linear_cell_edit
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
          DT::replaceData(uday_proxy_non_linear, rep, resetPaging = FALSE)
        })
        
        observeEvent(input$run_optimiser_non_linear,{
          
          target_torque <- input$numeric_input_uday_torque
          inequality_selection_torque <- input$inequality_selection_uday_torque
          weight_one <- input$weight_torque
          
          target_pred <- input$numeric_input_uday_pred
          inequality_selection_pred <- input$inequality_selection_uday_pred
          weight_two <- input$weight_pred
          
          opt$tab_2[[2]] <- as.numeric(opt$tab_2[[2]])
          
            constraint <- function(x){
            
            #equation one 
            constant <- (x[2]+x[3]+x[4]+x[5]+x[6]+x[7])
            # View(constant) #works
            equation_one <- -869.69157979082*x[1] + 275.811033852585*(x[2]/constant * (1-x[1]))+
              -640.1501097706*(x[3]/constant * (1-x[1])) +668.418092332803*(x[4]/constant * (1-x[1]))+
              318.358001206768*(x[5]/constant * (1-x[1]))+ 1671.3734983827 *(x[6]/constant * (1-x[1])) +
              388.721602050357*(x[7]/constant * (1-x[1]))+
              x[1]*(x[1]*1310.48156368694) +
              (x[2]/constant * (1-x[1]))*((x[2]/constant * (1-x[1]))*212.536004289529) +
              (x[7]/constant * (1-x[1]))*((x[7]/constant * (1-x[1]))*-384.589140519452) + 
              (x[5]/constant * (1-x[1]))*((x[5]/constant * (1-x[1]))*-17.9351176105554)+
              (x[3]/constant * (1-x[1]))*((x[3]/constant * (1-x[1]))*6523.70048980465)+
              (x[4]/constant * (1-x[1]))*((x[4]/constant * (1-x[1]))*-19394.6828836579)+
              (x[6]/constant * (1-x[1]))*((x[6]/constant * (1-x[1]))*-216978.610667717) - target_torque 
            
            #equation two
            equation_two <- 69.1536246310703*((x[1]-0.287)/0.315)+177.928009032322*(((x[2]/constant * (1-x[1]))-0.09)/0.315)+
              110.938848553557*(((x[5]/constant * (1-x[1]))-0.13)/0.315)-19.3532538435312*(((x[7]/constant * (1-x[1]))-0.12)/0.315)+
              200.241313148041*(((x[3]/constant * (1-x[1]))-0.054)/0.315)+84.026446932*(((x[4]/constant * (1-x[1]))-0.001)/0.315)-
              133.9508143*((x[6]/constant * (1-x[1]))/0.315)+
              ((x[1]-0.287)/0.315)*((((x[2]/constant * (1-x[1]))-0.09)/0.315)*-617.616933895298)+
              ((x[1]-0.287)/0.315)*((((x[5]/constant * (1-x[1]))-0.13)/0.315)*-390.462090953345)+
              ((x[1]-0.287)/0.315)*((((x[3]/constant * (1-x[1])) -0.054)/0.315)*-764.6979746)+
              ((x[5]-0.13)/0.315)*((((x[4]/constant * (1-x[1]))-0.001)/0.315)*-1142.960714+
              ((x[5]-0.13)/0.135)*(((x[6]/constant * (1-x[1]))/0.315)*2249.8386364)) - target_pred

          #lesser than
            if(inequality_selection_torque== "less than or equal to" & inequality_selection_pred=="less than or equal to"){
              return(c(equation_one,equation_one+ target_torque-36,-1*(equation_one + target_torque)+19 ,equation_two))
            }
          
            else if(inequality_selection_torque== "less than or equal to" & inequality_selection_pred=="greater than or equal to"){
              return(c(equation_one,equation_one+ target_torque-36,-1*(equation_one + target_torque)+19,-1*equation_two))
            }
            
            else if(inequality_selection_torque== "less than or equal to" & inequality_selection_pred=="equal to"){
              return(c(equation_one,equation_one+ target_torque-36,-1*(equation_one + target_torque)+19,equation_two-0.0001,-1*equation_two-0.0001))
            }
            
            #greater than
            else if(inequality_selection_torque == "greater than or equal to" & inequality_selection_pred=="less than or equal to"){
              return(c(-1*equation_one,equation_one+ target_torque-36,-1*(equation_one + target_torque)+19,equation_two))
            }
            
            else if(inequality_selection_torque == "greater than or equal to" & inequality_selection_pred=="greater than or equal to"){
              return(c(-1*equation_one,equation_one+ target_torque-36,-1*(equation_one + target_torque)+19,-1*equation_two))
            }
            
            else if(inequality_selection_torque == "greater than or equal to" & inequality_selection_pred=="equal to"){
              return(c(-1*equation_one,equation_one+ target_torque-36,-1*(equation_one + target_torque)+19,equation_two-0.0001,-1*equation_two-0.0001))
            }
            
            #equal to 
            else if(inequality_selection_torque == "equal to" & inequality_selection_pred=="less than or equal to"){
              return(c(equation_one-0.0001,-1*equation_one-0.0001,equation_two))
            }
            
            else if(inequality_selection_torque == "equal to" & inequality_selection_pred=="greater than or equal to"){
              return(c(equation_one-0.0001,-1*equation_one-0.0001,-1*equation_two))
            }
            
            else{
              return(c(equation_one-0.0001,-1*equation_one-0.0001,equation_two-0.0001,-1*equation_two-0.0001))
            }
            
            } #constraint function end

            
            obj<-function(x){
              constant <- (x[2]+x[3]+x[4]+x[5]+x[6]+x[7])
              eq_one <- weight_one*(opt$tab_2[1,2]*x[1]+opt$tab_2[2,2]*(x[2]/constant *(1-x[1])) +
                                      opt$tab_2[3,2]*(x[3]/constant * (1-x[1]))+opt$tab_2[4,2]*(x[4]/constant * (1-x[1])) +
                                      opt$tab_2[5,2]*(x[5]/constant * (1-x[1]))+opt$tab_2[6,2]*(x[6]/constant * (1-x[1])) +
                                      opt$tab_2[7,2]*(x[7]/constant * (1-x[1]))+opt$tab_2[1,2]*x[1]*opt$tab_2[1,2]*x[1] +
                                      opt$tab_2[2,2]*(x[2]/constant * (1-x[1]))*opt$tab_2[2,2]*(x[2]/constant * (1-x[1])) + 
                                      opt$tab_2[7,2]*(x[7]/constant * (1-x[1]))*opt$tab_2[7,2]*(x[7]/constant * (1-x[1])) +
                                      opt$tab_2[5,2]*(x[5]/constant * (1-x[1]))*opt$tab_2[5,2]*(x[5]/constant * (1-x[1])) +
                                      opt$tab_2[3,2]*(x[3]/constant * (1-x[1]))*opt$tab_2[3,2]*(x[3]/constant * (1-x[1])) +
                                      opt$tab_2[4,2]*(x[4]/constant * (1-x[1]))*opt$tab_2[4,2]*(x[4]/constant * (1-x[1])) +
                                      opt$tab_2[6,2]*(x[6]/constant * (1-x[1]))*opt$tab_2[6,2]*(x[6]/constant * (1-x[1]))  
                                   )
              
              eq_two <- weight_two*(opt$tab_2[1,2]*x[1] + opt$tab_2[2,2]*(x[2]/constant * (1-x[1])) + 
                                      opt$tab_2[5,2]*(x[5]/constant * (1-x[1])) +
                                      opt$tab_2[7,2]*(x[7]/constant * (1-x[1])) + opt$tab_2[3,2]*(x[3]/constant * (1-x[1])) +
                                      opt$tab_2[4,2]*(x[4]/constant * (1-x[1])) + opt$tab_2[6,2]*(x[6]/constant * (1-x[1])) +
                                      opt$tab_2[1,2]*x[1]*opt$tab_2[2,2]*(x[2]/constant * (1-x[1])) +
                                      opt$tab_2[1,2]*x[1]*opt$tab_2[5,2]*(x[5]/constant * (1-x[1])) + opt$tab_2[1,2]*x[1]*opt$tab_2[3,2]*(x[3]/constant * (1-x[1])) +
                                      opt$tab_2[5,2]*(x[5]/constant * (1-x[1]))*(opt$tab_2[4,2]*(x[4]/constant * (1-x[1]))*opt$tab_2[5,2]*(x[5]/constant * (1-x[1]))*opt$tab_2[6,2]*(x[6]/constant * (1-x[1]))))
                
              if(input$radio_button_uday_nonlinear == 'min'){
                   return(eq_one+eq_two)
              }

              else{
                return(-1*eq_one-eq_two)
              }
              
              } #objective function end
            
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
          output$optimiser_table32_uday_torque <- renderDataTable({
            df<-data.frame(Predictors = c("TargetSMC","NaLAS",	"AlkSilicate",	"CP5",
                                          "LSA","SCMC","Sulphate"),
                           Value = round(res$solution,3)
            )
            DT::datatable(df,selection ="none",rownames = FALSE )
          })
          # View(res$solution)
          constraint_value <- function(x){
            constant <- (x[2]+x[3]+x[4]+x[5]+x[6]+x[7])
            
            value <- -869.69157979082*x[1] + 275.811033852585*(x[2]/constant * (1-x[1]))+
              -640.1501097706*(x[3]/constant * (1-x[1])) +668.418092332803*(x[4]/constant * (1-x[1]))+
              318.358001206768*(x[5]/constant * (1-x[1]))+ 1671.3734983827 *(x[6]/constant * (1-x[1])) +
              388.721602050357*(x[7]/constant * (1-x[1]))+
              x[1]*(x[1]*1310.48156368694) +
              (x[2]/constant * (1-x[1]))*((x[2]/constant * (1-x[1]))*212.536004289529) +
              (x[7]/constant * (1-x[1]))*((x[7]/constant * (1-x[1]))*-384.589140519452) + 
              (x[5]/constant * (1-x[1]))*((x[5]/constant * (1-x[1]))*-17.9351176105554)+
              (x[3]/constant * (1-x[1]))*((x[3]/constant * (1-x[1]))*6523.70048980465)+
              (x[4]/constant * (1-x[1]))*((x[4]/constant * (1-x[1]))*-19394.6828836579)+
              (x[6]/constant * (1-x[1]))*((x[6]/constant * (1-x[1]))*-216978.610667717)
            return(value)
          }
          
          constraint_value2 <- function(x){
            constant <- (x[2]+x[3]+x[4]+x[5]+x[6]+x[7])
            
            value <-  69.1536246310703*((x[1]-0.287)/0.315)+177.928009032322*(((x[2]/constant * (1-x[1]))-0.09)/0.315)+
              110.938848553557*(((x[5]/constant * (1-x[1]))-0.13)/0.315)-19.3532538435312*(((x[7]/constant * (1-x[1]))-0.12)/0.315)+
              200.241313148041*(((x[3]/constant * (1-x[1]))-0.054)/0.315)+84.026446932*(((x[4]/constant * (1-x[1]))-0.001)/0.315)-
              133.9508143*((x[6]/constant * (1-x[1]))/0.315)+
              ((x[1]-0.287)/0.315)*((((x[2]/constant * (1-x[1]))-0.09)/0.315)*-617.616933895298)+
              ((x[1]-0.287)/0.315)*((((x[5]/constant * (1-x[1]))-0.13)/0.315)*-390.462090953345)+
              ((x[1]-0.287)/0.315)*((((x[3]/constant * (1-x[1])) -0.054)/0.315)*-764.6979746)+
              ((x[5]-0.13)/0.315)*((((x[4]/constant * (1-x[1]))-0.001)/0.315)*-1142.960714+
              ((x[5]-0.13)/0.135)*(((x[6]/constant * (1-x[1]))/0.315)*2249.8386364))
            
            return(value)
          }
          
          
          # optimiser output table 2
          output$optimiser_table22_uday_torque <- renderDataTable({
            value1 <- round(constraint_value(res$solution),3)
            value2 <- round(constraint_value2(res$solution),3)
            val <- data.frame(Predictors = c("Torque","Pred Formula Low Sheer Viscosity value"),
                              Value = as.data.frame(rbind(value1,value2)))
            
            DT::datatable(as.data.frame(rbind(round(constraint_value(res$solution),3),round(constraint_value2(res$solution),3))) 
                          ,rownames = c("Torque","Pred Formula Low Sheer Viscosity value"), colnames =c("Target variable", "Value"))
            # DT::datatable(val)#,rownames = c("Torque","Pred Formula Low Sheer Viscosity value"), colnames =c("Target variable", "Value"))
          })
          # View(round(constraint_value2(res$solution),3))
          # View(round(constraint_value(res$solution),3))
          # View(rbind(round(constraint_value(res$solution),3),round(constraint_value2(res$solution),3)))
          
          # optimiser output table 3
          if(input$radio_button_uday_nonlinear=='min'){
            output$value_results_uday_torque<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(res$objective,3))
            })
          }
          else{
            output$value_results_uday_torque<- renderUI({
              ns <- session$ns
              p(paste0("The objective function value resulting from the optimisation is : "),round(-1*res$objective,3))
            })
            
          }
          
          downresults12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Torque", "Pred Formula Low Sheer Viscosity value"), Predicted_or_Optimal_Value= c(constraint_value(res$solution), constraint_value2(res$solution)))
          downdf12<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate"),
                               Predicted_or_Optimal_Value=res$solution)
          
          if(input$radio_button_uday_nonlinear=='min'){
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = res$objective)
          }
          else{
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = -1*res$objective)
          }
          
          final123 <- rbind(downresults12,downdf12,downopt12)
          
          optimise2_uday(final123)
          output$download5_uday <- downloadHandler(
            filename = function() { "Optimisation BD Prediction by Model.xlsx"},
            content = function(file) {
              write_xlsx(list("Optimisation Result" = final123), file)
            }
          )
          
          # View(res$objective)
        }) #run optimiser non linear end
        
        
      })#observeevent non linear optimisation end
      
      observeEvent(input$reset_non_linear,{
        updateSelectInput(session,"inequality_selection_uday_torque",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_uday_torque",value = 28)
        updateNumericInput(session,"weight_torque",value = 1)
        updateRadioButtons(session,"radio_button_uday_nonlinear",selected = "min")
        
        updateSelectInput(session,"inequality_selection_uday_pred",selected = "less than or equal to")
        updateNumericInput(session,"numeric_input_uday_pred",value = 33)
        updateNumericInput(session,"weight_pred",value = 1)
        
        predictors_in_model2<-c("TargetSMC_[0.29,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                "CP5_[0,0.03]", "LSA_[0.17,0.45]",
                                "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
        zero_vector<-rep(1,length(predictors_in_model2))
        min_vector<- c(0.29,0.13,0.07,0,0.17,0,0.17)
        max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
        coef_data <- data.frame(cbind(predictors_in_model2,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
        opt$tab_2 <- coef_data
        opt$tab_2[[3]]<- as.numeric(opt$tab_2[[3]])
        opt$tab_2[[4]]<- as.numeric(opt$tab_2[[4]])
      })
      
      
    } 
  )}