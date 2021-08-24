ssmServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      
      # initializing some variables which will be used later
      ashutosh_proxy <- DT::dataTableProxy('optimiser_table1_ashutosh')
      opt<-reactiveValues(tab_1=NULL) 
      manualinput_ashutosh <- reactiveVal(NULL)
      manual_ashutosh <- reactiveVal(NULL)
      importresults_ashutosh <- reactiveVal(NULL)
      optimise_ashutosh <- reactiveVal(NULL)
      optimise1_ashutosh <- reactiveVal(NULL)
      optimise2_ashutosh <- reactiveVal(NULL)
      
      
      # ---------------------------------------------------- MODEL & DATA IMPORT --------------------------------------------------------
      
      # making a dataframe for our two models calculating Flake Final Temp (Model 1 & 2) 
      x_ashutosh <- data.frame(Models_ashutosh <- c("Mean_Seal_Strength(monoPP_Haiti) = (10.085043285)+ (0.143476202)*((Sealing_Pressure - 50)/25)) + 
                                                    (0.5005544208)*((Sealing_Time - 475)/275) + (1.1487322096)*((Sealing_Temperature - 120)/20) - 
                                                    (0.265927563)*((Layer_Thickness - 35)/5) - (0.214751072)*((Sealing_Pressure - 50)/25)*((Sealing_Time - 475)/275) + 
                                                    (0.1653191326)*((Sealing_Pressure - 50)/25)*((Layer_Thickness - 35)/5) - 
                                                    (0.190088312)*((Sealing_Time-475)/275)*((Layer_Thickness - 35)/5) - 
                                                    (0.227255444)*((Layer_Thickness - 35)/5)*((Sealing_Temperature - 120)/20) - 
                                                    (0.438383433)*((Sealing_Pressure - 50)/25)*((Sealing_Pressure - 50)/25) + 
                                                    (0.422789614)*((Sealing_Time-475)/275)*((Sealing_Time-475)/275) ",
                                                    
                                                    "Mean_Seal_Strength(Paper_metOPP/70-100gsmPaper_18metOPP) = (7.9283468534) - (1.744214002)*((Layer_Thickness - 85)/15)+
                                                    (2.0472868262)*((Sealing_Temperature - 180)/60) + (1.0299432676)*((Sealing_Time-475)/275) + 
                                                    (1.0456231626)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) - (0.085441804)*((Sealing_Pressure - 77)/38.5)",
                                                    
                                                    "Mean_Seal_Strength(Paper_metOPP/90gsmPaper_15-18metOPP) = (7.5004364288) + (2.1104133638)*((Sealing_Temperature - 180)/60)+
                                                    (0.9519562501)*((Sealing_Time-475)/275) + (1.0656214205)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + 
                                                    (0.2754606935)*((Layer_Thickness - 16.5)/1.5) - (0.007364972)*((Sealing_Pressure - 77)/38.5)",
                                                    
                                                    "Mean_Seal_Strength(Paper_metOPP/100gsmPaper_18metOPP) = (8.2995281922) + (4.1535657286)*((Sealing_Temperature - 180)/60)+
                                                    (0.9568895775)*((Sealing_Time-475)/275) - (3.199586193)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) + 
                                                    (0.7476644958)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + (0.0536281149)*((Sealing_Pressure - 77)/38.5)",
                                                    
                                                    "Mean_Seal_Strength(Paper_metOPP/70gsmPaper_18metOPP) = (9.544993865) + (2.6037874114)*((Sealing_Temperature - 180)/60)+
                                                    (1.8176938907)*((Sealing_Time-475)/275) + (1.96449116648)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) -
                                                    (0.489432036)*((Sealing_Pressure - 77)/38.5)",
                                                    
                                                    "Mean_Seal_Strength(Paper_metOPP/90gsmPaper_15metOPP) = (6.3165820822) + (1.0534709536)*((Sealing_Temperature - 180)/60)+
                                                    (0.6532790282)*((Sealing_Time-475)/275) + (1.3296269584)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) + 
                                                    (0.7989598978)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + (0.1730372655)*((Sealing_Pressure - 77)/38.5)",
                                                    
                                                    "Mean_Seal_Strength(Paper_metOPP/90gsmPaper_18metOPP) = (9.6899630181) + (0.5344405994)*((Sealing_Temperature - 180)/60)+
                                                    (1.1338418823)*((Sealing_Time-475)/275) - (1.76049007)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) -
                                                    (0.00534394)*((Sealing_Pressure - 77)/38.5)"
      ))
      
      # lists for variable names
      b_ashutosh <- c("Sealing_Pressure", "Sealing_Time", "Sealing_Temperature" , "Layer_Thickness")
      all_vars_ashutosh <-c("Sealing_Pressure", "Sealing_Time", "Sealing_Temperature" , "Layer_Thickness")
      
      # lists for excel file column names
      c_ashutosh <- c("sealingpressurencm2", "sealingtimems", "sealingtemperaturec" , "layer1thicknessum", "layer2thicknessum")
      d_ashutosh <- c("sealingpressurencm2", "sealingtimems", "sealingtemperaturec" , "layer1thicknessum", "layer2thicknessum")
      
      models_ashutosh <- c('monoPP_Haiti','Paper_metOPP_70_100gsmPaper_18metOPP','Paper_metOPP_90gsmPaper_15_18metOPP','Paper_metOPP_100gsmPaper_18metOPP','Paper_metOPP_70gsmPaper_18metOPP','Paper_metOPP_90gsmPaper_15metOPP','Paper_metOPP_90gsmPaper_18metOPP')
      
      
      
      # model bank table output
      output$models_ashutosh <- renderDataTable({
        datatable(x_ashutosh, colnames = c("Models"))
      } )
      
      
      # button action to go to simulation
      observeEvent(input$commit_ashutosh,{
        updateTabsetPanel(top_session, "tabs_ashutosh", selected = "Simulation")
      })
      
      # button action to go to visualization
      observeEvent(input$commit2_ashutosh,{
        updateTabsetPanel(top_session, "tabs_ashutosh", selected = "Visualization")
      })
      
      # rendering advisory tables - predictors and their limits
      output$advice_ashutosh <- renderDataTable({
        Advisory_table <- data.frame(Ingredients = c("Sealing_Pressure","Sealing_Time",
                                                     "Sealing_Temperature",
                                                     "Layer_Thickness"),
                                     Lower_Level = c(30, 200, 100, 30),
                                     Upper_Level = c(70, 700, 140, 40))
        datatable(Advisory_table)
      })
      
      
      colors <- c("red","black", "green","yellow","violet")
      
      # ------------------------------------------------ SIMULATION - PROFILER ------------------------------------------------------
      
      # profiler renderings
      observeEvent(req(input$profiler_Sealing_Pressure),{
        
        eqn1 <-"(10.085043285) + (0.143476202)*((Sealing_Pressure() - 50)/25) + 
                (0.5005544208)*((Sealing_Time() - 475)/275) + (1.1487322096)*((Sealing_Temperature() - 120)/20) - 
                (0.265927563)*((Layer_Thickness() - 35)/5) - (0.214751072)*((Sealing_Pressure() - 50)/25)*((Sealing_Time() - 475)/275) + 
                (0.1653191326)*((Sealing_Pressure() - 50)/25)*((Layer_Thickness() - 35)/5) - 
                (0.190088312)*((Sealing_Time()-475)/275)*((Layer_Thickness() - 35)/5) - 
                (0.227255444)*((Layer_Thickness() - 35)/5)*((Sealing_Temperature() - 120)/20) - 
                (0.438383433)*((Sealing_Pressure() - 50)/25)*((Sealing_Pressure() - 50)/25) + 
                (0.422789614)*((Sealing_Time()-475)/275)*((Sealing_Time()-475)/275)"
        
        eqn2 <-"(7.9283468534) - (1.744214002)*((Layer_Thickness - 85)/15)+
                (2.0472868262)*((Sealing_Temperature - 180)/60) + (1.0299432676)*((Sealing_Time-475)/275) + 
                (1.0456231626)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) - (0.085441804)*((Sealing_Pressure - 77)/38.5)"
          
        eqn3 <-"(7.5004364288) + (2.1104133638)*((Sealing_Temperature - 180)/60)+
                (0.9519562501)*((Sealing_Time-475)/275) + (1.0656214205)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + 
                (0.2754606935)*((Layer_Thickness - 16.5)/1.5) - (0.007364972)*((Sealing_Pressure - 77)/38.5)"
                                                    
        eqn4 <-"(8.2995281922) + (4.1535657286)*((Sealing_Temperature - 180)/60)+
                (0.9568895775)*((Sealing_Time-475)/275) - (3.199586193)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) + 
                (0.7476644958)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + (0.0536281149)*((Sealing_Pressure - 77)/38.5)"
        
        eqn5 <-"(9.544993865) + (2.6037874114)*((Sealing_Temperature - 180)/60)+
                (1.8176938907)*((Sealing_Time-475)/275) + (1.96449116648)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) -
                (0.489432036)*((Sealing_Pressure - 77)/38.5)"
              
        eqn6 <-"(6.3165820822) + (1.0534709536)*((Sealing_Temperature - 180)/60)+
                (0.6532790282)*((Sealing_Time-475)/275) + (1.3296269584)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) + 
                (0.7989598978)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + (0.1730372655)*((Sealing_Pressure - 77)/38.5)"
        
        eqn7 <-"(9.6899630181) + (0.5344405994)*((Sealing_Temperature - 180)/60)+
                (1.1338418823)*((Sealing_Time-475)/275) - (1.76049007)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) -
                (0.00534394)*((Sealing_Pressure - 77)/38.5)"
        
        eqn1 <- gsub("\n","",eqn1)
        eqn2 <- gsub("\n","",eqn2)
        eqn3 <- gsub("\n","",eqn3)
        eqn4 <- gsub("\n","",eqn4)
        eqn5 <- gsub("\n","",eqn5)
        eqn6 <- gsub("\n","",eqn6)
        eqn7 <- gsub("\n","",eqn7)
        
        # initializing lists
        Sealing_Pressure_list <- round(seq(from  = 30, to = 70, length.out = 41),2)
        Sealing_Time_list <- round(seq(from  = 200, to = 700, length.out = 501),2)
        Sealing_Temperature_list <- round(seq(from  = 100, to = 140, length.out = 141),2)
        Layer_Thickness_list <- round(seq(from  = 30, to = 40, length.out = 11),2)
        
        # initializing reactive variables 
        Sealing_Pressure <- reactive(input$profiler_Sealing_Pressure)
        Sealing_Time <- reactive(input$profiler_Sealing_Time)
        Sealing_Temperature <- reactive(input$profiler_Sealing_Temperature)
        Layer_Thickness <- reactive(input$profiler_Layer_Thickness)
       
        # making sure all variables are changed to numeric
        # Sealing_Pressure <- as.numeric(Sealing_Pressure)
        # Sealing_Time <- as.numeric(Sealing_Time())
        # Sealing_Temperature <- as.numeric(Sealing_Temperature())
        # Layer_Thickness <- as.numeric(Layer_Thickness())
        
        observeEvent(input$profiler_Sealing_Time | input$profiler_Sealing_Temperature | input$profiler_Layer_Thickness | input$profiler_Sealing_Pressure ,
                     {
                       Pr_eqn1 <- gsub("Sealing_Pressure[(][)]","Sealing_Pressure_list",eqn1)
                       Mean_Seal_Strnt_Pr <- reactive(eval(parse(text = Pr_eqn1)))
                        

                       output$plot1 <- renderPlot({
                         Mean_Seal_Strength <- Mean_Seal_Strnt_Pr()
                         
                         ggplot(data=data.frame(Sealing_Pressure_list, Mean_Seal_Strength), aes(x=Sealing_Pressure_list, y= Mean_Seal_Strength)) +
                           geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                           gghighlight(Sealing_Pressure_list == input$profiler_Sealing_Pressure) + ylim(8, 13)
                       })
                       
                       
                       Ti_eqn1 <- gsub("Sealing_Time[(][)]","Sealing_Time_list",eqn1)
                       Mean_Seal_Strnt_Time <- reactive(eval(parse(text = Ti_eqn1)))
                       
                       output$plot2 <- renderPlot({
                         Mean_Seal_Strength <- Mean_Seal_Strnt_Time()
                         ggplot(data=data.frame(Sealing_Time_list, Mean_Seal_Strength), aes(x=Sealing_Time_list, y= Mean_Seal_Strength)) +
                           geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
                           gghighlight(round(Sealing_Time_list,0) == input$profiler_Sealing_Time) + ylim(8, 13)
                       })
                       
                       
                       Te_eqn1 <- gsub("Sealing_Temperature[(][)]","Sealing_Temperature_list",eqn1)
                       Mean_Seal_Strnt_Temp <- reactive(eval(parse(text = Te_eqn1)))
                       
                       output$plot3 <- renderPlot({
                         Mean_Seal_Strength <- Mean_Seal_Strnt_Temp()
                         ggplot(data=data.frame(Sealing_Temperature_list, Mean_Seal_Strength), aes(x=Sealing_Temperature_list, y= Mean_Seal_Strength)) +
                           geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
                           gghighlight(Sealing_Temperature_list == input$profiler_Sealing_Temperature) + ylim(8, 13)
                       })
                   
                       
                       Lt_eqn1 <- gsub("Layer_Thickness[(][)]","Layer_Thickness_list",eqn1)
                       Mean_Seal_Strnt_LT<- reactive(eval(parse(text = Lt_eqn1)))
                       
                       output$plot4 <- renderPlot({
                         Mean_Seal_Strength <- Mean_Seal_Strnt_LT()
                         ggplot(data=data.frame(Layer_Thickness_list, Mean_Seal_Strength), aes(x=Layer_Thickness_list, y= Mean_Seal_Strength)) +
                           geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
                           gghighlight(Layer_Thickness_list == input$profiler_Layer_Thickness)  + ylim(8, 13) 
                       })
                         
                      })
                       
                       # output$plot2 <- renderPlot({
                       #   Mean_Seal_Strength <- Mean_Seal_Strnt()
                       #   ggplot(data=data.frame(Sealing_Time, Mean_Seal_Strength), aes(x=Sealing_Time, y= Mean_Seal_Strength)) +
                       #     geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                       #     gghighlight(Sealing_Time == input$profiler_Sealing_Time)
                       # })
                       # 
                       # output$plot3 <- renderPlot({
                       #   Mean_Seal_Strength <- Mean_Seal_Strnt()
                       #   ggplot(data=data.frame(Sealing_Temperature, Mean_Seal_Strength), aes(x=Sealing_Temperature, y= Mean_Seal_Strength)) +
                       #     geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                       #     gghighlight(Sealing_Temperature == input$profiler_Sealing_Temperature)
                       # })
                       # 
                       # output$plot4 <- renderPlot({
                       #   Mean_Seal_Strength <- Mean_Seal_Strnt()
                       #   ggplot(data=data.frame(Layer_Thickness, Mean_Seal_Strength), aes(x=Layer_Thickness, y= Mean_Seal_Strength)) +
                       #     geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                       #     gghighlight(Layer_Thickness == input$profiler_Layer_Thickness)
                       # })
       
                     
      })
      
      
      # --------------------------------------------- VISUALIZATION ------------------------------------------------------
      
      #visualization page renderings
      observeEvent(req(input$datacall_ashutosh),
                   {
                     ashutosh_data_slurry1 <- reactive({
                       req(input$datacall_ashutosh)
                       inFile <- input$datacall_ashutosh
                       if(is.null(inFile)) return(NULL)
                       
                       xlfile <- read_excel(input$datacall_ashutosh$datapath , sheet='stack_3')
                       colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
                       colnames(xlfile) <- gsub("_","",colnames(xlfile))
                       colnames(xlfile) <- gsub(" ","",colnames(xlfile))
                       colnames(xlfile) <- tolower(colnames(xlfile))
                       xlfile
                     })
                     df1 <- ashutosh_data_slurry1()
                     
                     
                     ashutosh_data_slurry2 <- reactive({
                       req(input$datacall_ashutosh)
                       inFile <- input$datacall_ashutosh
                       if(is.null(inFile)) return(NULL)
                       
                       # reading only sheet in which predictors exist
                       xlfile <- read_excel(input$datacall_ashutosh$datapath, sheet='stack_3')
                       # colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
                       # colnames(xlfile) <- gsub("_","",colnames(xlfile))
                       # colnames(xlfile) <- gsub(" ","",colnames(xlfile))
                       # colnames(xlfile) <- tolower(colnames(xlfile))
                       xlfile
                       
                     })
                     df2 <- ashutosh_data_slurry2()
                     finaldf <- as.data.frame(df2)
                     
                     finaldf <- finaldf[c('Sealing_Pressure_N_cm2', 'Sealing_Time_ms', 'Layer_1_thickness_um', 'Sealing_Temperature_C')]

                     output$simulationdata_ashutosh <- renderDataTable({
                       ashutosh_data_slurry2()
                     })
                     
                     updateSelectInput(session, "y_axis_bd", choices = colnames(finaldf), selected = colnames(finaldf)[2])
                     updateSelectInput(session, "x_axis_bd", choices = colnames(finaldf), selected = colnames(finaldf)[1])
                     updateSelectInput(session, "x_line", choices = colnames(finaldf))
                     updateSelectInput(session, "y_line", choices = colnames(finaldf))
                     updateSelectInput(session, "hist_choice_ashutosh", choices = colnames(finaldf))
                     
                     output$simulationdata1_ashutosh <- renderDataTable(finaldf)
                     output$scatterplot_ashutosh <- renderPlot(
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
                     output$ggsmooth_ashutosh<- renderPlot({
                       if(input$smooth2_ashutosh){
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
                           ggplot(data_ashutosh_bd(), aes(x= finaldf[[input$x_axis_bd]],y= finaldf[[input$y_axis_bd]])) +
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
                     
                     output$ggscatter_ashutosh <- renderPlot({
                       if(input$smooth_ashutosh){
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
                           ggplot(finaldf, aes(x= finaldf[[input$x_axis_bd]],y= data_ashutosh_bd()[[input$y_axis_bd]])) +
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
                     
                     output$multi_lines_graph_ashutosh <- renderPlotly({
                       if(length(input$y_axis_bd) == 1){
                         fig <- plot_ly( x = ~data_ashutosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd]],mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = as.character(input$y_axis)) )
                         fig
                       }else if(length(input$y_axis_bd) == 2){
                         fig <- plot_ly(data_ashutosh_bd(), x = ~data_ashutosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       }
                       else if(length(input$y_axis_bd) == 3){
                         fig <- plot_ly(data_ashutosh_bd(), x = ~data_ashutosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 4){
                         fig <- plot_ly(data_ashutosh_bd(), x = ~data_ashutosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 5){
                         fig <- plot_ly(data_ashutosh_bd(), x = ~data_ashutosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[5]]], name = input$y_axis_bd[5], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       } else if(length(input$y_axis_bd) == 6){
                         fig <- plot_ly(data_ashutosh_bd(), x = ~data_ashutosh_bd()[[input$x_axis_bd]], marker=list(size=10))
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[1]]], name = input$y_axis_bd[1],mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[2]]], name = input$y_axis_bd[2], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[3]]], name = input$y_axis_bd[3], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[4]]], name = input$y_axis_bd[4], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[5]]], name = input$y_axis_bd[5], mode = 'lines')
                         fig <- fig %>% add_lines(y = ~data_ashutosh_bd()[[input$y_axis_bd[6]]], name = input$y_axis_bd[6], mode = 'lines')
                         fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis_bd)),yaxis = list (title = "") )
                         fig
                       }
                     })
                     output$hist_ashutosh <- renderPlot({
                       ggplot(finaldf, aes(x= finaldf[[input$hist_choice_ashutosh]])) +
                         geom_histogram(color="black", fill="lightblue")+
                         labs(x = as.character(input$hist_choice_ashutosh))
                     })
                   })
      
      
      # --------------------------------------------- SIMULATION - MANUAL ENTRY --------------------------------------------------
      
      observeEvent(req(x_ashutosh),{
        x1_ashutosh <- reactiveValues()
        observe({
          y_ashutosh <- reactive({
            values <- c(50, 475, 120, 35)
            sqr <- data.frame(t(values))
            colnames(sqr) <- all_vars_ashutosh
            rownames(sqr) <- c("Enter Simulation Values")
            sqr
          })
          x1_ashutosh$df <- y_ashutosh()
        })
  
        
        output$simulation_input_ashutosh <- renderDataTable({ 
          
          datatable(x1_ashutosh$df, editable = T) %>%
            formatStyle(
              "Sealing_Pressure",
              color = styleInterval(c(30, 70), c('red', 'black', 'red')))%>%
            formatStyle(
              "Sealing_Time",
              color = styleInterval(c(200, 700), c('red', 'black', 'red')))%>%
            formatStyle(
              "Sealing_Temperature",
              color = styleInterval(c(100, 140), c('red', 'black', 'red')))%>%
            formatStyle(
              "Layer_Thickness",
              color = styleInterval(c(30, 40), c('red', 'black', 'red')))
        })
        
        
        proxy_ashutosh <- dataTableProxy("simulation_input_ashutosh")
        
        
        # to capture edited values in our saved table - x1_ashutosh
        
        observeEvent(input[["simulation_input_ashutosh_cell_edit"]], {
          info <- input[["simulation_input_ashutosh_cell_edit"]]
          
          i <- info$row
          j <- info$col
          v <- info$value
          
          if(!is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            x1_ashutosh$df[i, j] <- DT::coerceValue(v,x1_ashutosh$df[i, j])}
          
          rep <- x1_ashutosh$df
          DT::replaceData(proxy_ashutosh, rep, resetPaging = FALSE)
          
        })
        
        
        output$modeltable_ashutosh <- renderDataTable(
          datatable(x1_ashutosh$df)
        )
        
        
        observeEvent(input$simulate_ashutosh,{
          
          eqn1 <-"(10.085043285) + (0.143476202)*((Sealing_Pressure - 50)/25) + 
                (0.5005544208)*((Sealing_Time - 475)/275) + (1.1487322096)*((Sealing_Temperature - 120)/20) - 
                (0.265927563)*((Layer_Thickness - 35)/5) - (0.214751072)*((Sealing_Pressure - 50)/25)*((Sealing_Time - 475)/275) + 
                (0.1653191326)*((Sealing_Pressure - 50)/25)*((Layer_Thickness - 35)/5) - 
                (0.190088312)*((Sealing_Time-475)/275)*((Layer_Thickness - 35)/5) - 
                (0.227255444)*((Layer_Thickness - 35)/5)*((Sealing_Temperature - 120)/20) - 
                (0.438383433)*((Sealing_Pressure - 50)/25)*((Sealing_Pressure - 50)/25) + 
                (0.422789614)*((Sealing_Time-475)/275)*((Sealing_Time-475)/275)"
          
          eqn2 <-"(7.9283468534) - (1.744214002)*((Layer_Thickness - 85)/15)+
                (2.0472868262)*((Sealing_Temperature - 180)/60) + (1.0299432676)*((Sealing_Time-475)/275) + 
                (1.0456231626)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) - (0.085441804)*((Sealing_Pressure - 77)/38.5)"
          
          eqn3 <-"(7.5004364288) + (2.1104133638)*((Sealing_Temperature - 180)/60)+
                (0.9519562501)*((Sealing_Time-475)/275) + (1.0656214205)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + 
                (0.2754606935)*((Layer_Thickness - 16.5)/1.5) - (0.007364972)*((Sealing_Pressure - 77)/38.5)"
          
          eqn4 <-"(8.2995281922) + (4.1535657286)*((Sealing_Temperature - 180)/60)+
                (0.9568895775)*((Sealing_Time-475)/275) - (3.199586193)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) + 
                (0.7476644958)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + (0.0536281149)*((Sealing_Pressure - 77)/38.5)"
          
          eqn5 <-"(9.544993865) + (2.6037874114)*((Sealing_Temperature - 180)/60)+
                (1.8176938907)*((Sealing_Time-475)/275) + (1.96449116648)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) -
                (0.489432036)*((Sealing_Pressure - 77)/38.5)"
          
          eqn6 <-"(6.3165820822) + (1.0534709536)*((Sealing_Temperature - 180)/60)+
                (0.6532790282)*((Sealing_Time-475)/275) + (1.3296269584)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) + 
                (0.7989598978)*((Sealing_Temperature - 180)/60)*((Sealing_Time-475)/275) + (0.1730372655)*((Sealing_Pressure - 77)/38.5)"
          
          eqn7 <-"(9.6899630181) + (0.5344405994)*((Sealing_Temperature - 180)/60)+
                (1.1338418823)*((Sealing_Time-475)/275) - (1.76049007)*((Sealing_Temperature - 180)/60)*((Sealing_Temperature - 180)/60) -
                (0.00534394)*((Sealing_Pressure - 77)/38.5)"
          
          eqn1 <- gsub("\n","",eqn1)
          eqn2 <- gsub("\n","",eqn2)
          eqn3 <- gsub("\n","",eqn3)
          eqn4 <- gsub("\n","",eqn4)
          eqn5 <- gsub("\n","",eqn5)
          eqn6 <- gsub("\n","",eqn6)
          eqn7 <- gsub("\n","",eqn7)
          
          
          df <- as.data.frame(x1_ashutosh$df)
          
          # prefixing 'df' to column names
          for(i in b_ashutosh){
            eqn1 <- gsub(i, df[1,i], eqn1)
            eqn2 <- gsub(i, df[1,i], eqn2)
            eqn3 <- gsub(i, df[1,i], eqn3)
            eqn4 <- gsub(i, df[1,i], eqn4)
            eqn5 <- gsub(i, df[1,i], eqn5)
            eqn6 <- gsub(i, df[1,i], eqn6)
            eqn7 <- gsub(i, df[1,i], eqn7)
          }
          
          
          monoPP_Haiti <- round(eval(parse(text = eqn1)),3)
          Paper_metOPP_70_100gsmPaper_18metOPP <- round(eval(parse(text = eqn2)),3)
          Paper_metOPP_90gsmPaper_15_18metOPP <- round(eval(parse(text = eqn3)),3)
          Paper_metOPP_100gsmPaper_18metOPP<-  round(eval(parse(text = eqn4)),3)
          Paper_metOPP_70gsmPaper_18metOPP <- round(eval(parse(text = eqn5)),3)
          Paper_metOPP_90gsmPaper_15metOPP <- round(eval(parse(text = eqn6)),3)
          Paper_metOPP_90gsmPaper_18metOPP <- round(eval(parse(text = eqn7)),3)
          
          
          # result table
          tbl <- cbind(monoPP_Haiti,Paper_metOPP_70_100gsmPaper_18metOPP,Paper_metOPP_90gsmPaper_15_18metOPP,Paper_metOPP_100gsmPaper_18metOPP,Paper_metOPP_70gsmPaper_18metOPP,Paper_metOPP_90gsmPaper_15metOPP,Paper_metOPP_90gsmPaper_18metOPP)
          
          # input table
          df1 <- x1_ashutosh$df
          
          manualinput_ashutosh(df1)
          manual_ashutosh(tbl)
          
          nrdata <- as.data.frame(tbl)
          nrdata1 <- as.data.frame(df)
          
          output$download1_ashutosh <- downloadHandler(
            filename = function() { "Manual Entry Simulation.xlsx"},
            content = function(file) {
              write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
            }
          )
          output$heading1_ashutosh <- renderUI({
            h3("Simulation Results")
            
          })
          
          output$result1_ashutosh <- renderDataTable(
            { tbl })
        })
        
        
        # ---------------------------------------- SIMULATION - IMPORTED DATA -------------------------------------------------
        
        ashutosh_data_slurry <- reactive({
          req(input$datacall_ashutosh)
          inFile <- input$datacall_ashutosh
          if(is.null(inFile)) return(NULL)
          
          xlfile <- read_excel(input$datacall_ashutosh$datapath, sheet='stack_3')
          colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
          colnames(xlfile) <- gsub("_","",colnames(xlfile))
          colnames(xlfile) <- gsub(" ","",colnames(xlfile))
          colnames(xlfile) <- tolower(colnames(xlfile))
          xlfile
          
        })
        
        # checking whether data contains all required columns
        observeEvent(ashutosh_data_slurry(),
                     {i <- d_ashutosh[!is.element(d_ashutosh,colnames(ashutosh_data_slurry()))]
                     if(length(i)>=1){
                       showModal(modalDialog(paste0(i, " is not present in imported data but required in equation. 0 is placed in the place of missing variables to render the respective coefficients ineffective.")))
                     }
                     })
        
        observeEvent(req(input$datacall_ashutosh), {
          
          
          df1 <- ashutosh_data_slurry()
          
          df1 <- df1[c('sealingpressurencm2', 'sealingtimems', 'layer1thicknessum','layer2thicknessum','sealingtemperaturec')]
          
          observeEvent(req(input$simulate2_ashutosh),{
            
            eqn1 <-"(10.085043285) + (0.143476202)*((sealingpressurencm2 - 50)/25) + 
                (0.5005544208)*((sealingtimems - 475)/275) + (1.1487322096)*((sealingtemperaturec - 120)/20) - 
                (0.265927563)*((layer1thicknessum - 35)/5) - (0.214751072)*((sealingpressurencm2 - 50)/25)*((sealingtimems - 475)/275) + 
                (0.1653191326)*((sealingpressurencm2 - 50)/25)*((layer1thicknessum - 35)/5) - 
                (0.190088312)*((sealingtimems-475)/275)*((layer1thicknessum - 35)/5) - 
                (0.227255444)*((layer1thicknessum - 35)/5)*((sealingtemperaturec - 120)/20) - 
                (0.438383433)*((sealingpressurencm2 - 50)/25)*((sealingpressurencm2 - 50)/25) + 
                (0.422789614)*((sealingtimems-475)/275)*((sealingtimems-475)/275)"
            
            eqn2 <-"(7.9283468534) - (1.744214002)*((layer2thicknessum - 85)/15)+
                (2.0472868262)*((sealingtemperaturec - 180)/60) + (1.0299432676)*((sealingtimems-475)/275) + 
                (1.0456231626)*((sealingtemperaturec - 180)/60)*((sealingtimems-475)/275) - (0.085441804)*((sealingpressurencm2 - 77)/38.5)"
            
            eqn3 <-"(7.5004364288) + (2.1104133638)*((sealingtemperaturec - 180)/60)+
                (0.9519562501)*((sealingtimems-475)/275) + (1.0656214205)*((sealingtemperaturec - 180)/60)*((sealingtimems-475)/275) + 
                (0.2754606935)*((layer2thicknessum - 16.5)/1.5) - (0.007364972)*((sealingpressurencm2 - 77)/38.5)"
            
            eqn4 <-"(8.2995281922) + (4.1535657286)*((sealingtemperaturec - 180)/60)+
                (0.9568895775)*((sealingtimems-475)/275) - (3.199586193)*((sealingtemperaturec - 180)/60)*((sealingtemperaturec - 180)/60) + 
                (0.7476644958)*((sealingtemperaturec - 180)/60)*((sealingtimems-475)/275) + (0.0536281149)*((sealingpressurencm2 - 77)/38.5)"
            
            eqn5 <-"(9.544993865) + (2.6037874114)*((sealingtemperaturec - 180)/60)+
                (1.8176938907)*((sealingtimems-475)/275) + (1.96449116648)*((sealingtemperaturec - 180)/60)*((sealingtimems-475)/275) -
                (0.489432036)*((sealingpressurencm2 - 77)/38.5)"
            
            eqn6 <-"(6.3165820822) + (1.0534709536)*((sealingtemperaturec - 180)/60)+
                (0.6532790282)*((sealingtimems-475)/275) + (1.3296269584)*((sealingtemperaturec - 180)/60)*((sealingtemperaturec - 180)/60) + 
                (0.7989598978)*((sealingtemperaturec - 180)/60)*((sealingtimems-475)/275) + (0.1730372655)*((sealingpressurencm2 - 77)/38.5)"
            
            eqn7 <-"(9.6899630181) + (0.5344405994)*((sealingtemperaturec - 180)/60)+
                (1.1338418823)*((sealingtimems-475)/275) - (1.76049007)*((sealingtemperaturec - 180)/60)*((sealingtemperaturec - 180)/60) -
                (0.00534394)*((sealingpressurencm2 - 77)/38.5)"
            
            eqn1 <- gsub("\n","",eqn1)
            eqn2 <- gsub("\n","",eqn2)
            eqn3 <- gsub("\n","",eqn3)
            eqn4 <- gsub("\n","",eqn4)
            eqn5 <- gsub("\n","",eqn5)
            eqn6 <- gsub("\n","",eqn6)
            eqn7 <- gsub("\n","",eqn7)
            
            # prefixing 'df1' to column_names in equations
            for(i in c_ashutosh){
              eqn1 <- gsub(i, paste0("df1$",i), eqn1)
              eqn2 <- gsub(i, paste0("df1$",i), eqn2)
              eqn3 <- gsub(i, paste0("df1$",i), eqn3)
              eqn4 <- gsub(i, paste0("df1$",i), eqn4)
              eqn5 <- gsub(i, paste0("df1$",i), eqn5)
              eqn6 <- gsub(i, paste0("df1$",i), eqn6)
              eqn7 <- gsub(i, paste0("df1$",i), eqn7)
            }
            
            
            monoPP_Haiti <- round(eval(parse(text = eqn1)),3)
            Paper_metOPP_70_100gsmPaper_18metOPP <- round(eval(parse(text = eqn2)),3)
            Paper_metOPP_90gsmPaper_15_18metOPP <- round(eval(parse(text = eqn3)),3)
            Paper_metOPP_100gsmPaper_18metOPP<-  round(eval(parse(text = eqn4)),3)
            Paper_metOPP_70gsmPaper_18metOPP <- round(eval(parse(text = eqn5)),3)
            Paper_metOPP_90gsmPaper_15metOPP <- round(eval(parse(text = eqn6)),3)
            Paper_metOPP_90gsmPaper_18metOPP <- round(eval(parse(text = eqn7)),3)
            
            
            # result table
            tbl <- cbind(monoPP_Haiti,Paper_metOPP_70_100gsmPaper_18metOPP,Paper_metOPP_90gsmPaper_15_18metOPP,Paper_metOPP_100gsmPaper_18metOPP,Paper_metOPP_70gsmPaper_18metOPP,Paper_metOPP_90gsmPaper_15metOPP,Paper_metOPP_90gsmPaper_18metOPP)
            
            
            nrdata <- as.data.frame(tbl)
            importresults_ashutosh(tbl)
            
            output$download2_ashutosh <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
            
            output$heading_ashutosh <- renderUI({
              h3("Simulation Results")
            })
            
            output$modeltable2_ashutosh <- renderDataTable({
              DT::datatable(as.data.frame(cbind(monoPP_Haiti,Paper_metOPP_70_100gsmPaper_18metOPP,Paper_metOPP_90gsmPaper_15_18metOPP,Paper_metOPP_100gsmPaper_18metOPP,Paper_metOPP_70gsmPaper_18metOPP,Paper_metOPP_90gsmPaper_15metOPP,Paper_metOPP_90gsmPaper_18metOPP)), rownames = FALSE)
            })
          })
        })
        
        
        
        # --------------------------------------------- OPTIMIZATION ------------------------------------------------------
        
        #optimisation renderings for ashutosh(SD slurry props- drying prediction)
        observeEvent(req(x_ashutosh),
                     {
                       predictors_in_model3_ashutosh<-c("TargetSMC_[0.287,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                                       "CP5_[0,0.03]", "LSA_[0.17,0.45]",
                                                       "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
                       zero_vector<-rep(1,length(predictors_in_model3_ashutosh))
                       min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                       max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                       coef_data <- data.frame(cbind(predictors_in_model3_ashutosh,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                       opt$tab_1 <- coef_data
                       opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                       opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                       
                       #table 1
                       output$optimiser_table1_ashutosh<-renderDataTable({
                         DT::datatable(opt$tab_1,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
                       })
                       
                       observeEvent(input$optimiser_table1_ashutosh_cell_edit,{
                         info <- input$optimiser_table1_ashutosh_cell_edit
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
                         DT::replaceData(ashutosh_proxy, rep, resetPaging = FALSE)
                       })
                       
                       observeEvent(input$run_optimiser_ashutosh,{
                         
                         predictors_in_model3_ashutosh<-c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate")
                         reg_coeff_in_model3_ashutosh<-c(1.64652727504537,-0.340054974118285,0.0349876142645199,-0.26064073764549,-0.0575389664392278,-1.17237663840093,-0.298363251134605)
                         if(input$inequality_selection_ashutosh=="less than or equal to"){
                           constr_ashutosh<-'<='
                         }
                         else if(input$inequality_selection_ashutosh=="greater than or equal to"){
                           constr_ashutosh<-'>='
                         }
                         else{
                           constr_ashutosh<-'='
                         }
                         # View(constr)#works
                         
                         target_ashutosh<-input$numeric_input_ashutosh
                         number.predictors_ashutosh<-length(predictors_in_model3_ashutosh)
                         # View(opt$tab_1)
                         low.lims_ashutosh<-opt$tab_1[[3]]
                         upp.lims_ashutosh<-opt$tab_1[[4]]
                         objective.in_ashutosh<-opt$tab_1[[2]]
                         objective.in_ashutosh<-as.numeric(objective.in_ashutosh)
                         obj.type_ashutosh<-input$radio_button_ashutosh
                         # View(objective.in)#works
                         
                         lps.model_ashutosh <- make.lp(0,number.predictors_ashutosh)
                         add.constraint(lps.model_ashutosh,reg_coeff_in_model3_ashutosh,constr_ashutosh,target_ashutosh)
                         
                         # Bounds for variables
                         set.bounds(lps.model_ashutosh,lower=low.lims_ashutosh)
                         set.bounds(lps.model_ashutosh,upper=upp.lims_ashutosh)
                         # View(low.lims)
                         # View(nrow(low.lims))
                         # View(obj.type_ashutosh)
                         # View(objective.in)
                         
                         # Objective function
                         lp.control(lps.model_ashutosh,sense=obj.type_ashutosh) # min or max
                         # View(objective.in) #getting output
                         
                         set.objfn(lps.model_ashutosh,objective.in_ashutosh) # coefficients
                         # View(upp.lims)
                         
                         # Apply solver
                         solution.status <- solve(lps.model_ashutosh)
                         # View(solution.status)
                         if(solution.status!=0){
                           showModal(modalDialog("Linear Optimisation could not find a solution for the given inputs. Please change the inputs and re-run."))
                         }
                         #unpacking
                         solution.values_ashutosh <- get.primal.solution(lps.model_ashutosh)
                         # View(solution.values_ashutosh)
                         objective.function.value_ashutosh <- solution.values_ashutosh[1]
                         fitted.response_ashutosh <- solution.values_ashutosh[2]
                         solution.values_ashutosh <- solution.values_ashutosh[3:length(solution.values_ashutosh)]
                         
                         results_ashutosh<-data.frame(Value = fitted.response_ashutosh)
                         # colnames(results)<-""
                         row.names(results_ashutosh)<-"Drying Prediction"
                         
                         #dopwnload
                         downresults <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Drying Prediction"), Predicted_or_Optimal_Value= fitted.response_ashutosh)
                         downdf<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate"),
                                            Predicted_or_Optimal_Value=solution.values_ashutosh)
                         downopt <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective func. Value"), Predicted_or_Optimal_Value = objective.function.value_ashutosh)
                         
                         
                         final1 <- rbind(downresults,downdf,downopt)
                         #View(final1)
                         optimise_ashutosh(final1)
                         output$download3_ashutosh <- downloadHandler(
                           filename = function() { "Optimisation for Drying Prediction .xlsx"},
                           content = function(file) {
                             write_xlsx(list("Optimisation Result" = final1), file)
                           }
                         )
                         
                         # optmiser table2
                         output$optimiser_table2_ashutosh<-renderDataTable({
                           DT::datatable(results_ashutosh) })
                         
                         # optmiser table3
                         output$optimiser_table3_ashutosh<- renderDataTable({
                           df_ashutosh<-data.frame(Predictors=c("TargetSMC","NaLAS","AlkSilicate","CP5","LSA","SCMC","Sulphate"),
                                                  Value=solution.values_ashutosh)
                           DT::datatable(df_ashutosh,rownames=FALSE)
                           # DT::datatable(df)
                         })
                         
                         #optimiser textoutput
                         output$value_results_ashutosh<- renderUI({
                           p(paste0("The objective value resulting from the optimisation is : "),round(objective.function.value_ashutosh),4)
                         })
                         
                         
                         
                       })#observeEvent run optimiser ends
                       
                       # reset button
                       observeEvent(input$reset_ashutosh,{
                         updateSelectInput(session,"inequality_selection_ashutosh",selected = "less than or equal to")
                         updateNumericInput(session,"numeric_input_ashutosh",value = .32)
                         updateRadioButtons(session,"radio_button_ashutosh",selected = "min")
                         predictors_in_model3_ashutosh<-c("TargetSMC_[0.287,0.37]","NaLAS_[0.13,0.41]","AlkSilicate_[0.07,0.16]",
                                                         "CP5_[0,0.03]", "LSA_[0.17,0.45]",
                                                         "SCMC_[0,0.01]","Sulphate_[0.17,0.52]")
                         zero_vector<-rep(1,length(predictors_in_model3_ashutosh))
                         min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                         max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                         coef_data <- data.frame(cbind(predictors_in_model3_ashutosh,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                         opt$tab_1 <- coef_data
                         opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                         opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                         
                       })
                       
                     })#observeevent datacall close
        
        observeEvent(input$downloadresults_ashutosh,{
          
          output$Download_Values_ashutosh <- renderUI({
            ns <- session$ns
            downloadButton(ns("download_all_ashutosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          })
          nrdata <- as.data.frame(manual_ashutosh())
          nrdata1 <- as.data.frame(manualinput_ashutosh())
          nrdata2 <- as.data.frame(importresults_ashutosh())
          nrdata3 <- as.data.frame(optimise_ashutosh())
          
          output$download_all_ashutosh <- downloadHandler(
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